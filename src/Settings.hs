module Settings
  ( Settings (..)
  ) where

import Data.Text (Text)
import Data.Yaml as YAML
import GHC.Generics
import Data.Monoid.Generic

data Settings = Settings
  { allowed  :: [Text]
  , ignored  :: [Text]
  } deriving (Eq, Show, Generic)
    deriving Semigroup via GenericSemigroup Settings
    deriving Monoid via GenericMonoid Settings

instance FromJSON Settings where
  parseJSON = withObject ".licensed.yml" $ \o -> do
    let cabal = withObject "cabal: stanza" (.: "cabal")
    Settings <$> (o .: "allowed")
             <*> (o .: "ignored" >>= cabal)
