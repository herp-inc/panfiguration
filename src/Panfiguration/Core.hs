{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
    , Template(..)
    , plainTemplate
    , bareTemplate
    , Panfigurable(..)
    , exec
    , run
    , runMaybe
    ) where

import Barbies
import Barbies.Bare
import Barbies.Constraints (Dict(..), ClassF)
import Barbies.TH
import Control.Applicative
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.Functor.Compose
import Data.Functor.Identity
import Data.List (intercalate, union)
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..), All, Any)
import Data.String
import qualified Data.Text as Text
import qualified Options.Applicative as O
import System.Environment (getEnvironment)

import Panfiguration.FromParam
import Panfiguration.Case

data Result a = Result
    { resultSources :: [String]
    , resultUsed :: [String]
    , resultContent :: Maybe a
    }
    deriving (Show, Eq, Ord, Functor)

mkResult :: String -> Maybe a -> Result a
mkResult key = Result [key] [key]

-- | TODO: remove this
instance Applicative Result where
    pure a = Result [] [] (Just a)
    Result s0 u0 f <*> Result s1 u1 a = Result (s0 `union` s1) (u0 `union` u1) (f <*> a)

instance Panfigurable a => Semigroup (Result a) where
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

instance Panfigurable a => Monoid (Result a) where
    mempty = Result [] [] Nothing

class Panfigurable a where
    declaredCase :: Case
    declaredCase = camel

    template :: [String] -> Template a

    -- | Merge two parameters. The 'Ordering' indicates which side of the arguments is used.
    mergeParams :: a -> a -> (Ordering, a)
    mergeParams a _ = (LT, a)

plainTemplate :: FromParam a => [String] -> Template a
plainTemplate ts = Plain ts fromParam

deriving instance Panfigurable a => Panfigurable (Identity a)

instance Panfigurable Bool where
    template = plainTemplate

instance Panfigurable Char where
    template = plainTemplate

instance FromParam a => Panfigurable [a] where
    template = plainTemplate

instance Panfigurable Int where
    template = plainTemplate

instance Panfigurable Integer where
    template = plainTemplate

instance Panfigurable Text.Text where
    template = plainTemplate

instance Panfigurable ByteString where
    template = plainTemplate

instance FromParam a => Panfigurable (Maybe a) where
    template = plainTemplate

instance Panfigurable Any where
    template = plainTemplate

instance Panfigurable All where
    template = plainTemplate

instance (FieldNamesB h, TraversableB h, ConstraintsB h, AllBF Panfigurable f h) => Panfigurable (Barbie h f) where
    template prefix = Barbie
        <$> btraverseC @(ClassF Panfigurable f)
            (\(Const name) -> template $ prefix <> split (declaredCase @(Barbie h f)) name)
            bfieldNames

data Template a where
    Plain :: [String] -> (String -> Either ParseError a) -> Template a
    Pure :: a -> Template a
    Ap :: Template (a -> b) -> Template a -> Template b

instance Functor Template where
    fmap = Ap . Pure

instance Applicative Template where
    pure = Pure
    (<*>) = Ap

instance FromParam a => IsString (Template a) where
    fromString str = Plain [str] fromParam

toEnvParser :: Case -> [(String, String)] -> Template a -> Either String (Result a)
toEnvParser srcCase vars (Plain (join srcCase -> k) parse)
    = mkResult ("environment variable " <> k)
    <$> traverse parse (lookup k vars)
toEnvParser srcCase vars (Ap f g) = (<*>) <$> toEnvParser srcCase vars f <*> toEnvParser srcCase vars g
toEnvParser _ _ (Pure a) = pure (pure a)

toOptParser :: Case -> Template a -> O.Parser (Result a)
toOptParser srcCase (Plain (join srcCase -> k) parse) = fmap (mkResult $ "command line option " <> k)
    $ optional $ O.option (O.eitherReader parse) $ O.long k
toOptParser srcCase (Ap f g) = (<*>) <$> toOptParser srcCase f <*> toOptParser srcCase g
toOptParser _ (Pure a) = pure (pure a)

data Source h = Source
    { sourceCase :: Case
    , sourceRun :: Case -> h Template -> IO (h Result)
    }

data Panfiguration h = Panfiguration
    { fieldNameCase :: First Case
    , loggerFunction :: First (String -> IO ())
    , sources :: [Source h]
    }

instance Semigroup (Panfiguration h) where
    Panfiguration a b c <> Panfiguration x y z = Panfiguration (a <> x) (b <> y) (c <> z)

instance Monoid (Panfiguration h) where
    mempty = Panfiguration mempty mempty mempty

mkSource :: Case -> (Case -> h Template -> IO (h Result)) -> Panfiguration h
mkSource c f = mempty { sources = [Source c f] }

-- | Set the letter case of the data declaration
declCase :: Case -> Panfiguration h
declCase c = mempty { fieldNameCase = pure c }

-- | Set the letter case of the sources
asCase :: Panfiguration h -> Case -> Panfiguration h
asCase pfg c = pfg { sources = [ s { sourceCase = c } | s <- sources pfg] }

-- | Update names being used for the backends
withNames :: Panfiguration h -> (h Template -> h Template) -> Panfiguration h
withNames pfg f = pfg { sources = [ s { sourceRun = \c -> sourceRun s c . f } | s <- sources pfg] }

envs :: (TraversableB h, ConstraintsB h, AllB Panfigurable h) => Panfiguration h
envs = mkSource SNAKE $ \srcCase envNames -> do
    vars <- getEnvironment
    either fail pure $ btraverseC @Panfigurable (toEnvParser srcCase vars) envNames

opts :: (TraversableB h, ConstraintsB h, AllB Panfigurable h) => Panfiguration h
opts = mkSource kebab $ \srcCase optNames -> do
    let parsers = btraverseC @Panfigurable (toOptParser srcCase) optNames
    O.execParser $ O.info (parsers <**> O.helper) mempty

defaults :: FunctorB h => h Maybe -> Panfiguration h
defaults def = mkSource AsIs $ \_ _ -> pure $ bmap (mkResult "default") def

-- | Provide all the default values by a plain record
fullDefaults :: (BareB b, FunctorB (b Covered)) => b Bare Identity -> Panfiguration (b Covered)
fullDefaults = defaults . bmap (Just . runIdentity) . bcover

logger :: (String -> IO ()) -> Panfiguration h
logger f = mempty { loggerFunction = pure f }

resolve :: (String -> IO ()) -> Dict Show a -> Const String a -> Result a -> Compose IO Maybe a
resolve logFunc Dict (Const key) (Result srcs used r) = Compose $ r <$ case r of
    Nothing -> logFunc $ unwords [key <> ":", "None of", intercalate "," srcs, "provides a value"]
    Just v -> logFunc $ unwords [key <> ":", "using", show v, "from", intercalate "," used]

type PanfigurableB h = (FieldNamesB h
    , TraversableB h
    , ApplicativeB h
    , ConstraintsB h
    , AllB Show h
    , AllB Panfigurable h)

templates :: PanfigurableB h => Panfiguration h -> h Template
templates Panfiguration{..} = bmapC @Panfigurable
    (template . splitter . getConst)
    bfieldNames
    where
        splitter = split $ fromMaybe camel $ getFirst fieldNameCase

exec :: PanfigurableB h => Panfiguration h -> IO (h Result)
exec = execWith <*> templates

-- | Parse all the relevant environment variables and command line options, then merges them.
execWith :: (PanfigurableB h)
    => Panfiguration h
    -> h Template
    -> IO (h Result)
execWith Panfiguration{..} ts = do
    results <- forM sources $ \Source{..} -> sourceRun sourceCase ts
    pure $ foldr (bzipWithC @Panfigurable (<>)) (bpureC @Panfigurable mempty) results

runMaybe :: (PanfigurableB h)
    => Panfiguration h
    -> IO (h Maybe)
runMaybe panfig = do
    result <- exec panfig
    let logFunc = fromMaybe mempty $ getFirst $ loggerFunction panfig
    bsequence $ bzipWith3 (resolve logFunc) bdicts bfieldNames result

run :: (BareB b, PanfigurableB (b Covered))
    => Panfiguration (b Covered)
    -> IO (b Bare Identity)
run panfig = do
    maybes <- runMaybe panfig
    fmap bstrip <$> maybe (error "Failed to run panfiguration") pure $ bsequence' maybes

bareTemplate :: (BareB b, PanfigurableB (b Covered), AllB (ClassF Panfigurable Identity) (b Covered)) => [String] -> Template (b Bare Identity)
bareTemplate prefix = bstrip . getBarbie <$> template prefix