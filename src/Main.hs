{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DeriveTraversable, DuplicateRecordFields, FlexibleContexts,
             FlexibleInstances, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

module Main where

import Prelude hiding (fail)

import           Control.Applicative
import           Control.Effect
import           Control.Effect.Error
import           Control.Effect.Fail hiding (fail)
import           Control.Effect.Reader
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
  = Check    { config :: Config dir }
  | Download { config :: Config dir }
    deriving (Show, Functor, Foldable, Traversable)

type Command = CommandF Path.AbsRelDir

shouldDownload :: Command -> Bool
shouldDownload (Download _) = True
shouldDownload _            = False

commandParser :: Opt.Parser (CommandF FilePath)
commandParser = subparser
  (  command "check"    (info ((Check <$> configParser)    <**> helper) (progDesc "Check the validity of a cached .licensed directory"))
  <> command "download" (info ((Download <$> configParser) <**> helper) (progDesc "Patch or download license files"))
  )

configParser :: Opt.Parser (Config FilePath)
configParser =
  Config
  <$> strOption (short 'p' <> long "project"  <> metavar "PROJECT"  <> help "Project to process")
  <*> strOption (short 'd' <> long "directory" <> metavar "DIRECTORY" <> help "Working directory")


skipThese :: [Text]
skipThese = ["Cabal", "Only", "cabal-doctest"]

download :: ( Member (Error BribeException) sig
           , Member (Reader Dep) sig
           , Carrier sig m
           , MonadIO m
           )
         => Text
         -> m ()
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
    Right _ -> liftIO (putStrLn "Found license file.")

textDoc :: Pretty a => a -> Text
textDoc = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

process :: ( Member (Reader Command) sig
          , Member (Reader Dep) sig
          , Member (Error BribeException) sig
          , Carrier sig m
          , MonadIO m
          )
        => m ()
process = do
  Dep depName depVersion <- ask
  unless (depName `elem` skipThese) $ do
    Config{..} <- asks config
    let (path :: Path.AbsRelFile) =
          workdir
          </> Path.dir ".licenses"
          </> Path.dir project
          </> Path.dir "cabal"
          </> Path.file (T.unpack depName) <.> "txt"

    let sys = Path.toString path
    should <- asks shouldDownload
    exists <- liftIO (doesFileExist sys)
    if not exists
      then do
        liftIO (T.putStrLn (depName <> ": LICENSE NOT FOUND!"))

        when should (download "LICENSE")
      else do
        eLicense <- Atto.parseOnly parseLicense <$> liftIO (T.readFile sys)
        license  <- either (throwError . AttoParseFailure) pure eLicense

        let eInfo = YAML.decodeEither' @Info . encodeUtf8 . preamble $ license
        current <- either (throwError . YAMLParseFailure depName . show) pure eInfo

        if version current == depVersion
          then liftIO (T.putStrLn (depName <> ": OK"))
          else do
            liftIO (T.putStrLn (depName <> ": MISMATCH!"))
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

  print (T.lines . T.pack $ out)

  let parse = parseDep `Atto.sepBy` Atto.char '\n'

  let (Right parsed) = Atto.parseOnly parse (T.pack out)

  result <- runM
    . runFail
    . runError @_ @_ @BribeException
    . runReader cmd
    . for_ parsed
    $ flip runReader process
  case result of
    Right (Left err) -> die ("Fatal error: " <> show err)
    Left err         -> die ("Unexpected JSON value (debug: " <> show err <> ")")
    _                -> pure ()
