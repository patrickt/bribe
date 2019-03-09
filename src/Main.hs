{-# LANGUAGE ApplicativeDo, RecordWildCards, ScopedTypeVariables #-}

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
import           Data.Default (def)
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding
import qualified Data.Text.IO as T
import           Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Yaml as YAML
import           Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import           Options.Applicative as Opt
import           System.Directory (doesFileExist)
import           System.Exit
import           System.Path ((<.>), (</>))
import qualified System.Path as Path
import           System.Posix.Directory
import           System.Process

import Bribe

data Config dir = Config
  { project :: String
  , workdir :: dir
  } deriving (Show, Functor, Foldable, Traversable)

data CommandF dir
  = Check  { config :: Config dir }
  | Update { config :: Config dir }
    deriving (Show, Functor, Foldable, Traversable)

type Command = CommandF Path.AbsRelDir

shouldUpdate :: Command -> Bool
shouldUpdate (Update _) = True
shouldUpdate _          = False

commandParser :: Opt.Parser (CommandF FilePath)
commandParser = hsubparser $ mconcat
  [ command "check"    (info (Check <$> configParser)  (progDesc "Check the validity of a cached .licensed directory"))
  , command "update"   (info (Update <$> configParser) (progDesc "Patch or update license files"))
  ]

configParser :: Opt.Parser (Config FilePath)
configParser =
  Config
  <$> strOption (short 'p' <> long "project"   <> metavar "PROJECT"  <> help "Project to process")
  <*> strOption (short 'd' <> long "directory" <> metavar "DIRECTORY" <> help "Working directory")


skipThese :: [Text]
skipThese = ["Cabal", "Only", "cabal-doctest"]

download :: ( Member (Error BribeException) sig
           , Member (Reader Dep) sig
           , Member Trace sig
           , Carrier sig m
           , MonadIO m
           )
         => Text -> m ()
download p = do
  tag <- asks depTag
  let licenseURL = Req.http "hackage.haskell.org" /: "package" /: tag /: "src" /: p
  eResult <-
    liftIO
    . Exc.try @Exc.SomeException
    . Req.runReq def
    $ Req.req Req.GET licenseURL Req.NoReqBody Req.bsResponse mempty

  case eResult of
    Left _  -> if p == "LICENSE" then download "LICENSE.txt" else pure ()
    Right _ -> trace "Downloaded license file."

textDoc :: Pretty a => a -> Text
textDoc = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

process :: ( Member (Reader Command) sig
          , Member (Reader Dep) sig
          , Member (Error BribeException) sig
          , Member (Writer Result) sig
          , Member Trace sig
          , Carrier sig m
          , MonadIO m
          )
        => m ()
process = do
  dep@(Dep depName depVersion) <- ask
  unless (depName `elem` skipThese) $ do
    Config{..} <- asks config
    let (path :: Path.AbsRelFile) =
          workdir
          </> Path.dir ".licenses"
          </> Path.dir project
          </> Path.dir "cabal"
          </> Path.file (T.unpack depName) <.> "txt"

    let sys = Path.toString path
    should <- asks shouldUpdate
    exists <- liftIO (doesFileExist sys)
    if not exists
      then do
        trace (T.unpack depName <> ": LICENSE NOT FOUND!")
        tell (missing dep)
        when should (download "LICENSE")
      else do
        eLicense <- Atto.parseOnly parseLicense <$> liftIO (T.readFile sys)
        license  <- either (throwError . AttoParseFailure) pure eLicense

        let eInfo = YAML.decodeEither' @Info . encodeUtf8 . preamble $ license
        current <- either (throwError . YAMLParseFailure depName . show) pure eInfo

        if version current == depVersion
          then do
            trace (T.unpack depName <> ": OK")
            tell succeeding
          else do
            trace (T.unpack depName <> ": MISMATCH!")
            tell (mismatched dep (version current))
            when should $ do
              let newInfo = current { version = depVersion }
              let newLicense = license { preamble = textDoc newInfo }
              liftIO . T.writeFile sys . textDoc $ newLicense


data BribeException
  = LicenseFileNotFound FilePath
  | AttoParseFailure String
  | YAMLParseFailure Text String
    deriving (Show)

main :: IO ()
main = do
  let opts = info (helper <*> commandParser) (fullDesc <> header "bribe - get past license checks A$AP")

  -- Parse command-line arguments and ensure path validity
  tcfg <- Opt.execParser opts
  cmd <- case traverse Path.parse tcfg of
    Left  l -> die  ("Fatal error: " <> show l)
    Right c -> pure @_ @Command c

  let cfg = config cmd

  changeWorkingDirectory . Path.toString . workdir $ cfg

  (code, out, _) <- readProcessWithExitCode "stack" ["ls", "dependencies", project cfg] ""

  when (code /= ExitSuccess) (die ("Failed executing `stack ls dependencies`: " <> out))

  let parse = parseDep `Atto.sepBy` Atto.char '\n'

  let (Right parsed) = Atto.parseOnly parse (T.pack out)

  result <- runM
    . runTraceByIgnoring
    . runFail
    . runError @_ @_ @BribeException
    . execWriter
    . runReader cmd
    . for_ parsed
    $ flip runReader process
  case result of
    Left err             -> die ("Unexpected JSON value (debug: " <> show err <> ")")
    Right (Left err)     -> die ("Fatal error: " <> show err)
    Right (Right done)   -> Pretty.putDoc (pretty @Result done) *> putStrLn ""
