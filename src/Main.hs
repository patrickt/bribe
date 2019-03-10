{-# LANGUAGE ConstraintKinds, LambdaCase, RecordWildCards, ScopedTypeVariables #-}

module Main (main) where

import Prelude hiding (fail)

import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.Fail hiding (fail)
import           Control.Effect.Reader
import           Control.Effect.Trace
import           Control.Effect.Writer
import qualified Control.Exception as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Attoparsec.Text as Atto
import qualified Data.ByteString.Char8 as B
import           Data.Default (def)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Data.Yaml as YAML
import           Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import           System.Directory (doesFileExist)
import           System.Exit
import           System.Path ((<.>), (</>))
import           Options.Applicative as Opt
import qualified System.Path as Path
import           System.Posix.Directory
import           System.Process

import Bribe
import Command
import Settings

type Checking sig m
  = ( Member (Reader Command) sig
    , Member (Reader Dep) sig
    , Member (Reader Settings) sig
    , Member (Error Fatal) sig
    , Member (Writer Result) sig
    , Member (Lift IO) sig
    , Member Trace sig
    , Effect sig
    , Carrier sig m
    , MonadIO m
    )

data LicenseError
  = Ignoring
  | Missing FilePath
  | Mismatch String Info License
  | Disallowed Text
    deriving (Eq, Show)

data Fatal
  = AttoParseFailure String
  | YAMLParseFailure Text String
  | CabalParseFailure Text
  | GhcPkgFailure
    deriving (Show)

-- Could write our own handler, but this is easier
trace' :: Checking sig m => String -> m ()
trace' msg = whenM (asks (verbose . config)) (trace msg)

-- Main loop, each dependency provided by the Reader effect
process :: Checking sig m => m ()
process = runError verify >>= either recover (void . pure)

-- Handle and log non-fatal errors
recover :: Checking sig m => LicenseError -> m ()
recover err = do
  dep@(Dep dname dversion) <- ask
  case err of
    Ignoring -> trace' (T.unpack dname <> ": IGNORING") *> tell ignore
    Missing sys -> do
      trace' (T.unpack dname <> ": LICENSE NOT FOUND!")
      tell (missing dep)
      whenM (asks isUpdating) (download sys "LICENSE")
    Disallowed t -> do
      trace' (T.unpack dname <> ": DISALLOWED LICENSE!")
      tell (invalid dep t)
    Mismatch sys current license -> do
      trace' (T.unpack dname <> ": MISMATCH!")
      tell (mismatched dep (version current))
      whenM (asks isUpdating) $ do
        let newInfo = current { version = dversion }
        let newLicense = license { preamble = textDoc newInfo }
        liftIO . T.writeFile sys . textDoc $ newLicense

-- Actually do the checking
verify :: (Member (Error LicenseError) sig, Checking sig m) => m ()
verify = do
  Dep depName depVersion <- ask
  whenM (elem depName <$> asks ignored) (throwError Ignoring)

  Config{..} <- asks config
  let (path :: Path.AbsRelFile) =
        workdir
        </> Path.dir ".licenses"
        </> Path.dir project
        </> Path.dir "cabal"
        </> Path.file (T.unpack depName) <.> "txt"

  let sys = Path.toString path
  unlessM (liftIO (doesFileExist sys)) (throwError (Missing sys))

  eLicense <- Atto.parseOnly parseLicense <$> liftIO (T.readFile sys)
  onDisk   <- either (throwError . AttoParseFailure) pure eLicense

  let eInfo = YAML.decodeEither' @Info . encodeUtf8 . preamble $ onDisk
  current <- either (throwError . YAMLParseFailure depName . show) pure eInfo

  valids <- asks allowed
  unless (license current `elem` valids) (throwError (Disallowed (license current)))
  trace' ("LICENSE: " <> show (license current))

  when (version current /= depVersion) (throwError (Mismatch sys current onDisk))

  trace' (T.unpack depName <> ": OK")
  tell succeeding

-- Download a license file. The parameter represents the filename to try
-- (first "LICENSE", then "LICENSE.txt" as a fallback)
download :: Checking sig m => FilePath -> Text -> m ()
download sys p = do
  pkg <- asks package
  tag <- asks depTag
  let licenseURL = Req.http "hackage.haskell.org" /: "package" /: tag /: "src" /: p
  trace' (show licenseURL)
  eResult <-
    liftIO
    . Exc.try @Exc.SomeException
    . Req.runReq def
    $ Req.req Req.GET licenseURL Req.NoReqBody Req.bsResponse mempty

  case eResult of
    Left _  -> if p == "LICENSE" then download sys "LICENSE.txt" else pure ()
    Right bs -> do
      trace' "Downloaded license file."

      (code, out, _) <- liftIO $ readProcessWithExitCode "stack" ["exec", "ghc-pkg", "--", "describe", T.unpack pkg] ""

      when (code == ExitSuccess) $ do
        let mCabal = Cabal.parseInstalledPackageInfo out
        case fmap fromCabal mCabal of
          Cabal.ParseFailed e -> trace' ("CABAL PARSE FAILURE " <> show e)
          Cabal.ParseOk _ newinfo -> do
            let newlicense = License (decodeUtf8 . YAML.encode $ newinfo) (decodeUtf8 . Req.responseBody $ bs)
            liftIO . T.writeFile sys . textDoc $ newlicense

textDoc :: Pretty a => a -> Text
textDoc = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

main :: IO ()
main = do
  let opts = info (helper <*> commandParser) (fullDesc <> header "bribe - get past license checks A$AP")

  cmd <- Opt.execParser opts

  let cfg = config cmd

  changeWorkingDirectory . Path.toString . workdir $ cfg

  (code, out, _) <- readProcessWithExitCode "stack" ["ls", "dependencies", project cfg] ""

  when (code /= ExitSuccess) (die ("Failed executing `stack ls dependencies`: " <> out))

  let settingsPath = Path.toString (workdir cfg </> Path.file ".licensed.yml")

  settings <- YAML.decodeFileEither @Settings settingsPath >>= \case
    Left err  -> mempty <$ putStrLn ("Warning: couldn't load " <> settingsPath <> " (" <> show err <> ")")
    Right val -> pure val

  let parse = parseDep `Atto.sepBy` Atto.char '\n'

  let (Right parsed) = Atto.parseOnly parse (T.pack out)

  result <- runM
    . runTraceByPrinting
    . runFail
    . runError @Fatal
    . execWriter
    . runReader cmd
    . runReader settings
    . traverse (flip runReader process)
    $ parsed
  case result of
    Left err           -> die ("Unexpected JSON value (debug: " <> show err <> ")")
    Right (Left err)   -> die ("Fatal error: " <> show err)
    Right (Right done) -> do
      Pretty.putDoc (pretty @Result done) *> putStrLn ""
      when (isChecking cmd && any isMismatch (failures done))
        (putStrLn "(╯°□°）╯︵ ┻━┻" *> exitFailure)
