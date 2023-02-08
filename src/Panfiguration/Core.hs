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
    , errorLogger
    , Panfigurable
    , exec
    , run
    , runMaybe
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
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import qualified Options.Applicative as O
import System.Environment (getEnvironment)

import Panfiguration.FromParam
import Panfiguration.Case

data Result a = Result
    { resultSources :: [String]
    , resultUsed :: [String]
    , resultContent :: Maybe a
    }
    deriving (Show, Eq, Ord)

mkResult :: String -> Maybe a -> Result a
mkResult key = Result [key] [key]

instance FromParam a => Semigroup (Result a) where
    Result s0 _ Nothing <> Result s1 _ Nothing = Result (s0 <> s1) mempty Nothing
    Result s0 _ Nothing <> Result s1 u (Just a) = Result (s0 <> s1) u (Just a)
    Result s0 u (Just a) <> Result s1 _ Nothing = Result (s0 <> s1) u (Just a)
    Result s0 u0 (Just a) <> Result s1 u1 (Just b)
        =   let (side, c) = mergeParams a b
                used = case side of
                    LT -> u0
                    EQ -> u0 <> u1
                    GT -> u1
            in Result (s0 <> s1) used $ Just c

instance FromParam a => Monoid (Result a) where
    mempty = Result [] [] Nothing

data Source h = Source
    { sourceCase :: Case
    , sourceRun :: h (Const String) -> IO (h Result)
    }

mapConsts :: FunctorB h => (a -> b) -> h (Const a) -> h (Const b)
mapConsts f = bmap (first f)

data Panfiguration h = Panfiguration
    { fieldNameCase :: First Case
    , loggerFunction :: First (String -> IO ())
    , errorLoggerFunction :: First (String -> IO ())
    , sources :: [Source h]
    }

instance Semigroup (Panfiguration h) where
    Panfiguration a b c d <> Panfiguration x y z w = Panfiguration (a <> x) (b <> y) (c <> z) (d<>w)

instance Monoid (Panfiguration h) where
    mempty = Panfiguration mempty mempty mempty mempty

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
        (\(Const k) -> tag k <$> traverse fromParam (lookup k vars))
        envNames
  where
    tag k = mkResult $ "env:" <> k

opts :: (TraversableB h, ConstraintsB h, AllB FromParam h) => Panfiguration h
opts = mkSource kebab $ \optNames -> do
    let parsers = btraverseC @FromParam
            (\(Const k) -> fmap (tag k) $ optional $ mkOption k)
            optNames
    O.execParser $ O.info (parsers <**> O.helper) mempty
  where
    tag k = mkResult $ "--" <> k
    mkOption k = O.option (O.eitherReader fromParam) $ O.long k

defaults :: FunctorB h => h Maybe -> Panfiguration h
defaults def = mkSource AsIs $ const $ pure $ bmap (mkResult "the default") def

-- | Provide all the default values by a plain record
fullDefaults :: (BareB b, FunctorB (b Covered)) => b Bare Identity  -> Panfiguration (b Covered)
fullDefaults = defaults . bmap (Just . runIdentity) . bcover

logger :: (String -> IO ()) -> Panfiguration h
logger f = mempty { loggerFunction = pure f }

errorLogger :: (String -> IO ()) -> Panfiguration h
errorLogger f = mempty { errorLoggerFunction = pure f }

resolve :: (String -> IO ()) -> Maybe (String -> IO ()) -> Dict Show a -> Const (NE.NonEmpty String) a -> Result a -> Compose IO Maybe a
resolve logFunc errorLog Dict (Const key) (Result srcs used r) = Compose $ r <$ case r of
    Nothing -> fromMaybe logFunc errorLog $ unwords [displayKey key <> ":", "None of", commas srcs, "provides a value"]
    Just v -> logFunc $ unwords [displayKey key <> ":", "using", show v, "from", commas used]
    where
        displayKey = intercalate "." . NE.toList
        commas [] = ""
        commas [a] = a
        commas [a, b] = unwords [a, "and", b]
        commas (x : xs) = x <> ", " <> commas xs


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
            (fmap $ split $ fromMaybe camel $ getFirst fieldNameCase)
            bnestedFieldNames

    results <- forM sources $ \Source{..} -> sourceRun
        $ mapConsts (join sourceCase . concat) names
 
    pure $ foldr (bzipWithC @FromParam (<>)) (bpureC @FromParam mempty) results

runMaybe :: (Panfigurable h)
    => Panfiguration h
    -> IO (h Maybe)
runMaybe panfig = do
    result <- exec panfig
    let logFunc = fromMaybe mempty $ getFirst $ loggerFunction panfig
        errorLogFunc = getFirst $ errorLoggerFunction panfig
    bsequence $ bzipWith3 (resolve logFunc errorLogFunc) bdicts bnestedFieldNames result

run :: (BareB b, Panfigurable (b Covered))
    => Panfiguration (b Covered)
    -> IO (b Bare Identity)
run panfig = do
    maybes <- runMaybe panfig
    fmap bstrip <$> maybe (error "Failed to run panfiguration") pure $ bsequence' maybes