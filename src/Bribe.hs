{-# LANGUAGE DerivingVia, DuplicateRecordFields, LambdaCase, OverloadedLists #-}

module Bribe
  ( whenM
  , unlessM
  , Info (..)
  , fromCabal
  , License (..)
  , parseLicense
  , Dep (Dep)
  , package
  , depTag
  , parseDep
  , Result (..)
  , isMismatch
  , succeeding
  , missing
  , mismatched
  , invalid
  , ignore
  ) where

import           Control.Monad
import qualified Data.Attoparsec.Text as Atto
import           Data.Foldable
import           Data.Sequence (Seq)
import           Data.Monoid
import           Data.Monoid.Generic
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc (Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Yaml as YAML
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.PackageName as Cabal
import           GHC.Generics

whenM, unlessM :: Monad m => m Bool -> m () -> m ()
whenM act handle = act >>= flip when handle
unlessM act handle = act >>= flip unless handle

data Info = Info
  { name     :: Text
  , version  :: Text
  , summary  :: Maybe Text
  , homepage :: Maybe Text
  , license  :: Text -- ^ a short name, usually an SPDX identifier
  } deriving (Show, Eq, Generic, YAML.FromJSON, YAML.ToJSON)

fromCabal :: Cabal.InstalledPackageInfo -> Info
fromCabal p = Info { name     = T.pack . Cabal.unPackageName . Cabal.pkgName . Cabal.sourcePackageId $ p
                   , version  = T.pack . Cabal.prettyShow . Cabal.pkgVersion . Cabal.sourcePackageId $ p
                   , summary  = Just . T.pack . Cabal.synopsis $ p
                   , homepage = Just . T.pack . Cabal.homepage $ p
                   , license  = munge . T.pack . either Cabal.prettyShow Cabal.prettyShow . Cabal.license $ p
                   }

munge :: Text -> Text
munge t
  | t == "BSD-3" = "bsd-3-clause"
  | t == "MIT"   = "mit"
  | otherwise = t

instance Pretty Info where
  pretty Info{..} = Pretty.vcat
    [ "type: cabal"
    , "name: " <> pretty name
    , "version: " <> pretty version
    , maybe "" (\s -> "summary: " <> escaping s) summary
    , maybe "" (\s -> "homepage: " <> pretty s ) homepage
    , "license: " <> pretty license
    ]
    where escaping x
            | ":" `T.isPrefixOf` x = Pretty.squotes $ pretty x
            | otherwise            = pretty x

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
  void "---\n"
  yaml <- Atto.manyTill Atto.anyChar "---\n"
  rest <- Atto.takeText
  pure (License (T.pack yaml) rest)

data Dep = Dep
  { package :: Text
  , version :: Text
  } deriving (Eq, Show)

depTag :: Dep -> Text
depTag (Dep n v) = n <> "-" <> v

parseDep :: Atto.Parser Dep
parseDep = do
  n <- Atto.takeTill (== ' ')
  void " "
  v <- Atto.takeTill (== '\n')
  pure (Dep n v)

data Failure
  = Mismatch Dep Text
  | Missing Dep
  | Invalid Dep Text
  deriving (Eq, Show)

isMismatch :: Failure -> Bool
isMismatch Mismatch{} = True
isMismatch _          = False

instance Pretty Failure where
  pretty = \case
    Mismatch found expected -> "-" <+> pretty (depTag found) <+> Pretty.parens ("Stackage version:" <+> pretty expected)
    Missing found           -> "-" <+> pretty (depTag found) <+> "(license not downloaded)"
    Invalid found lic       -> "-" <+> pretty (depTag found) <+> Pretty.parens ("invalid license:" <+> pretty lic)

data Result = Result
  { succeeded :: Sum Int
  , failures  :: Seq Failure
  , ignores   :: Sum Int
  } deriving (Eq, Show, Generic)
    deriving Semigroup via (GenericSemigroup Result)
    deriving Monoid    via (GenericMonoid Result)

instance Pretty Result where
  pretty Result{..}
    | null failures =
      pretty (getSum succeeded) <+> "successes, 0 failures," <+> pretty (getSum ignores) <+> "ignored"
    | otherwise = Pretty.vcat
      [ pretty (getSum succeeded) <+> "successes," <+> pretty (length failures) <+> "failures," <+> pretty (getSum ignores) <+> "ignored"
      , "Failures:"
      , Pretty.vcat (toList . fmap pretty $ failures)
      ]

succeeding :: Result
succeeding = Result 1 [] 0

mismatched :: Dep -> Text -> Result
mismatched d t = Result 0 [Mismatch d t] 0

missing :: Dep -> Result
missing d = Result 0 [Missing d] 0

invalid :: Dep -> Text -> Result
invalid d t = mempty { failures = [Invalid d t]}

ignore :: Result
ignore = Result 0 [] 1
