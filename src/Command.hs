module Command
  ( Config (..)
  , Command (..)
  , isChecking
  , isUpdating
  , commandParser
  ) where

import           Options.Applicative as Opt
import qualified System.Path as Path

data Config = Config
  { project :: String
  , workdir :: Path.AbsRelDir
  , verbose :: Bool
  } deriving (Show, Eq)

data Command
  = Check  { config :: Config }
  | Update { config :: Config }
    deriving (Show, Eq)

isChecking :: Command -> Bool
isChecking (Check _) = True
isChecking _         = False

isUpdating :: Command -> Bool
isUpdating = not . isChecking

commandParser :: Opt.Parser Command
commandParser = hsubparser $ mconcat
  [ command "check"    (info (Check <$> configParser)  (progDesc "Check the validity of a cached .licensed directory"))
  , command "update"   (info (Update <$> configParser) (progDesc "Patch or update license files"))
  ]

configParser :: Opt.Parser Config
configParser = Config
  <$> strOption (short 'p' <> long "project"   <> metavar "PROJECT"  <> help "Project to process")
  <*> pathOption (short 'd' <> long "directory" <> metavar "DIRECTORY" <> help "Working directory")
  <*> switch (short 'v' <> long "verbose" <> help "Verbose output")

pathOption :: Mod OptionFields Path.AbsRelDir -> Parser Path.AbsRelDir
pathOption = Opt.option (Opt.eitherReader Path.parse)
