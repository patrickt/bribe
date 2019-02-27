{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances,
             OverloadedStrings, RecordWildCards, ScopedTypeVariables, TypeApplications #-}

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
import           Data.Bifunctor
import           Data.Default (def)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Encoding
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import           Data.Text.Prettyprint.Doc (Pretty (..), Doc)
import qualified Data.Text.IO as T
import qualified Data.Yaml as YAML
import           Debug.Trace
import           GHC.Generics
import           Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req
import           Options.Applicative as Opt
import           System.Directory (doesFileExist)
import           System.Exit
import           System.Path ((<.>), (</>))
import qualified System.Path as Path
import           System.Posix.Directory
import           System.Process

data Config dir = Config
  { cfgProject   :: String
  , cfgDirectory :: dir
  } deriving (Show, Functor, Foldable, Traversable)

type Conf = Config Path.AbsRelDir

configParser :: Opt.Parser (Config FilePath)
configParser =
  Config
  <$> strOption (short 'p' <> long "project"  <> metavar "PROJECT"  <> help "Project to process")
  <*> strOption (short 'd' <> long "directory" <> metavar "DIRECTORY" <> help "Working directory")

data Info = Info
  { name     :: Text
  , version  :: Text
  , summary  :: Maybe Text
  , homepage :: Maybe Text
  , license  :: Text
  } deriving (Show, Eq, Generic, YAML.FromJSON)

instance Pretty Info where
  pretty Info{..} = Pretty.vcat
    [ "type: cabal"
    , "name: " <> pretty name
    , "version: " <> pretty version
    , maybe "" (\s -> "summary: " <> pretty s) summary
    , maybe "" (\s -> "homepage: " <> pretty s) homepage
    , "license: " <> pretty license
    ]

data Dep = Dep
  { depName    :: Text
  , depVersion :: Text
  } deriving (Eq, Show)

depTag :: Dep -> Text
depTag (Dep n v) = n <> "-" <> v

data License = License
  { preamble :: Text
  , legalese :: Text
  } deriving (Eq, Show)

instance Pretty License where
  pretty License{..} = Pretty.vcat
    [ "---"
    , pretty preamble
    , "---"
    , pretty legalese
    ]

parseDep :: Atto.Parser Dep
parseDep = do
  traceM "start"
  n <- Atto.takeTill (== ' ')
  void $ Atto.char ' '
  v <- Atto.takeTill (== '\n')
  pure (Dep n v)

parseLicense :: Atto.Parser License
parseLicense = do
  "---\n"
  yaml <- Atto.manyTill Atto.anyChar "---\n"
  rest <- Atto.takeText
  pure (License (T.pack yaml) rest)


skipThese :: [Text]
skipThese = ["Cabal", "Only", "cabal-doctest"]

-- infoFromDescription :: ( Member (Reader Conf) sig
--                        , Member (Error BribeException)
--                        , Carrier sig m, MonadIO m
--                        )
--                     => m Info
-- infoFromDescription = do
--   (code, out, err) <- readProcessWithExitCode "stack" ["exec", "ghc-pkg", "--", "describe", ""] ""


download :: ( Member (Error BribeException) sig
            , Carrier sig m
            , MonadIO m
            )
         => Text
         -> Dep
         -> m ()
download p d@Dep{..} = do
  let licenseURL = Req.http "hackage.haskell.org" /: "package" /: depTag d /: "src" /: p
  eResult <-
    liftIO
    . Exc.try @Exc.SomeException
    . Req.runReq def
    $ Req.req Req.GET licenseURL Req.NoReqBody Req.bsResponse mempty

  case eResult of
    Left err -> if p == "LICENSE" then download "LICENSE.txt" d else pure ()
    Right _  -> liftIO (putStrLn "Found license file.")

textDoc :: Pretty a => a -> Text
textDoc = Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . pretty

process :: ( Member (Reader Conf) sig
           , Member (Error BribeException) sig
           , Carrier sig m
           , MonadIO m
           )
        => Dep
        -> m ()
process d@Dep{..}
  | depName `elem` skipThese = pure ()
  | otherwise = do
      Config{..} <- ask
      let (path :: Path.AbsRelFile) = cfgDirectory </> Path.dir ".licenses" </> Path.dir cfgProject </> Path.dir "cabal" </> Path.file (T.unpack depName) <.> "txt"

      let sys = Path.toString path
      exists <- liftIO (doesFileExist sys)
      if not exists
        then do
          liftIO (T.putStrLn (depName <> ": LICENSE NOT FOUND!"))
          download "LICENSE" d
        else do
          eLicense <- Atto.parseOnly parseLicense <$> liftIO (T.readFile sys)
          license  <- either (throwError . AttoParseFailure) pure eLicense

          let eInfo = YAML.decodeEither' @Info . encodeUtf8 . preamble $ license
          info <- either (throwError . YAMLParseFailure . show) pure eInfo

          let v = version info

          if version info == depVersion
            then liftIO (T.putStrLn (depName <> ": OK"))
            else do
              liftIO (T.putStrLn (depName <> ": MISMATCH!"))
              let newInfo = info { version = depVersion }
              let newLicense = license { preamble = textDoc newInfo }
              liftIO . T.writeFile sys . textDoc $ newLicense


data BribeException
  = LicenseFileNotFound FilePath
  | AttoParseFailure String
  | YAMLParseFailure String
    deriving (Show)

main :: IO ()
main = do
  let opts = info (configParser <**> helper)
        (fullDesc
         <> progDesc "update a .licensed directory from a Stack snapshot"
         <> header "bribe - get past license checks A$AP")

  -- Parse command-line arguments and ensure path validity
  tcfg <- Opt.execParser opts
  pcfg <- case traverse Path.parse tcfg of
    Left  l -> die  ("Fatal error: " <> show l)
    Right c -> pure (c :: Conf)

  -- Go to the specified working
  changeWorkingDirectory . Path.toString . cfgDirectory $ pcfg

  putStrLn "starting"

  (code, out, err) <- readProcessWithExitCode "stack" ["ls", "dependencies", cfgProject pcfg] ""

  when (code /= ExitSuccess) (die ("Failed executing `stack ls dependencies`: " <> out))

  putStrLn "deps out"
  print (T.lines . T.pack $ out)

  let parse = parseDep `Atto.sepBy` Atto.char '\n'

  let (Right parsed) = Atto.parseOnly parse (T.pack out)

  result <- runM . runFail . runError @_ @_ @BribeException . runReader pcfg . void $ traverse process parsed
  case result of
    Right (Left err) -> die ("Fatal error: " <> show err)
    Left err         -> die ("Unexpected JSON value (debug: " <> show err <> ")")
    _                -> pure ()


  -- result <- runM . runReader cfg . runError $ workflow
  -- case id @(Either BribeError _) result of
  --   Left err -> putDoc (pretty err) >> exitFailure
  --   Right _  -> pure ()
