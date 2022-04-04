{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module Panfiguration.Core (
     Panfiguration(..)
    , Result(..)
    , Source(..)
    , declCase
    , asCase
    , withNames
    , envs
    , opts
    , defaults
    , fullDefaults
    , logger
    , Panfigurable
    , exec
    , run
    ) where

import Barbies
import Barbies.Bare
import Barbies.Constraints (Dict(..))
import Barbies.TH
import Control.Applicative
import Control.Monad (forM)
import Data.Bifunctor
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Options.Applicative as O
import System.Environment (getEnvironment)

import Panfiguration.FromParam
import Panfiguration.Case

newtype Result a = Result { unResult :: Maybe (String, a) }
    deriving (Show, Eq, Ord)
    deriving (Semigroup, Monoid) via Compose First ((,) String) a

data Source h = Source
    { sourceCase :: Case
    , sourceRun :: h (Const String) -> IO (h Result)
    }

mapConsts :: FunctorB h => (a -> b) -> h (Const a) -> h (Const b)
mapConsts f = bmap (first f)

data Panfiguration h = Panfiguration
    { fieldNameCase :: First Case
    , loggerFunction :: First (String -> IO ())
    , sources :: [Source h]
    }

instance Semigroup (Panfiguration h) where
    Panfiguration a b c <> Panfiguration x y z = Panfiguration (a <> x) (b <> y) (c <> z)

instance Monoid (Panfiguration h) where
    mempty = Panfiguration mempty mempty mempty

mkSource :: Case -> (h (Const String) -> IO (h Result)) -> Panfiguration h
mkSource c f = mempty { sources = [Source c f] }

-- | Set the letter case of the data declaration
declCase :: Case -> Panfiguration h
declCase c = mempty { fieldNameCase = pure c }

-- | Set the letter case of the sources
asCase :: Panfiguration h -> Case -> Panfiguration h
asCase pfg c = pfg { sources = [ s { sourceCase = c } | s <- sources pfg] }

-- | Update names being used for the backends
withNames :: Panfiguration h -> (h (Const String) -> h (Const String)) -> Panfiguration h
withNames pfg f = pfg { sources = [ s { sourceRun = sourceRun s . f } | s <- sources pfg] }

envs :: (TraversableB h, ConstraintsB h, AllB FromParam h) => Panfiguration h
envs = mkSource SNAKE $ \envNames -> do
    vars <- getEnvironment
    either fail pure $ btraverseC @FromParam
        (\(Const k) -> Result . fmap ("environment variable",)
            <$> traverse (first ((k ++ ": ") ++) . fromParam) (lookup k vars))
        envNames

opts :: (TraversableB h, ConstraintsB h, AllB FromParam h) => Panfiguration h
opts = mkSource kebab $ \optNames -> do
    let parsers = btraverseC @FromParam
            (\(Const k) -> fmap (Result . fmap ("command line option",)) $ optional $ mkOption k)
            optNames
    O.execParser $ O.info (parsers <**> O.helper) mempty
  where
    mkOption k = O.option (O.eitherReader fromParam) $ O.long k

defaults :: FunctorB h => h Maybe -> Panfiguration h
defaults def = mkSource AsIs $ const $ pure $ bmap (Result . fmap ("default",)) def

-- | Provide all the default values by a plain record
fullDefaults :: (BareB b, FunctorB (b Covered)) => b Bare Identity  -> Panfiguration (b Covered)
fullDefaults = defaults . bmap (Just . runIdentity) . bcover

logger :: (String -> IO ()) -> Panfiguration h
logger f = mempty { loggerFunction = pure f }

resolve :: (String -> IO ()) -> Dict Show a -> Const String a -> Result a -> IO a
resolve logFunc Dict (Const key) (Result r) = case r of
    Nothing -> fail $ "No default value is provided for " <> key
    Just (src, v) -> explain src v
  where
    explain src v = v <$ logFunc (unwords [key <> ":", "using", show v, "from the", src])

type Panfigurable h = (FieldNamesB h
    , TraversableB h
    , ApplicativeB h
    , ConstraintsB h
    , AllB Show h
    , AllB FromParam h)

-- | Parse all the relevant environment variables and command line options, then merges them.
exec :: (Panfigurable h)
    => Panfiguration h
    -> IO (h Result)
exec Panfiguration{..} = do
    let names = mapConsts
            (split $ fromMaybe camel $ getFirst fieldNameCase)
            bfieldNames

    results <- forM sources $ \Source{..} -> sourceRun
        $ mapConsts (join sourceCase) names
 
    pure $ foldr (bzipWith (<>)) (bpure mempty) results

run :: (BareB b, Panfigurable (b Covered))
    => Panfiguration (b Covered)
    -> IO (b Bare Identity)
run panfig = do
    result <- exec panfig
    let logFunc = fromMaybe mempty $ getFirst $ loggerFunction panfig
    fmap bstrip $ bsequence' $ bzipWith3 (resolve logFunc) bdicts bfieldNames result