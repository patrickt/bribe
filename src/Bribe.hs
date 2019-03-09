{-# LANGUAGE DuplicateRecordFields #-}

module Bribe
  ( Info (..)
  , License (..)
  , parseLicense
  , Dep (Dep)
  , depTag
  , parseDep
  ) where

import           Control.Monad
import qualified Data.Attoparsec.Text as Atto
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Yaml as YAML
import           GHC.Generics

data Info = Info
  { name     :: Text
  , version  :: Text
  , summary  :: Maybe Text
  , homepage :: Maybe Text
  , license  :: Text -- ^ a short name, usually an SPDX identifier
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

data License = License
  { preamble :: Text -- ^ an encoded 'Info'
  , legalese :: Text -- ^ the actual contents of the license
  } deriving (Eq, Show)

instance Pretty License where
  pretty License{..} = Pretty.vcat
    [ "---"
    , pretty preamble
    , "---"
    , pretty legalese
    ]

parseLicense :: Atto.Parser License
parseLicense = do
  "---\n"
  yaml <- Atto.manyTill Atto.anyChar "---\n"
  rest <- Atto.takeText
  pure (License (T.pack yaml) rest)

data Dep = Dep
  { name     :: Text
  , version  :: Text
  } deriving (Eq, Show)

depTag :: Dep -> Text
depTag (Dep n v) = n <> "-" <> v

parseDep :: Atto.Parser Dep
parseDep = do
  n <- Atto.takeTill (== ' ')
  void $ Atto.char ' '
  v <- Atto.takeTill (== '\n')
  pure (Dep n v)
