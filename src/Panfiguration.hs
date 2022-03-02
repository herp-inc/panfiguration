{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
module Panfiguration (
    FromParam(..)
    , readFromParam
    , Secret(..)
    , Panfiguration(..)
    , snake
    , fromDefaults
    , run
    ) where

import Barbies
import Barbies.Bare
import Barbies.Constraints (Dict(..))
import Barbies.TH
import Control.Applicative
import Data.Bifunctor
import Data.ByteString.Char8 as BC (ByteString, pack)
import Data.Char
import Data.Char
import Data.Functor.Identity
import Data.Typeable
import GHC.Generics ((:*:)(..))
import Network.Socket (HostName, PortNumber)
import qualified Options.Applicative as O
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

-- | A newtype wrapper to distinguish confidential values.
-- 'show' and error messages from 'fromParam' mask its contents.
newtype Secret a = Secret { unSecret :: a }

instance Show a => Show (Secret a) where
    show = ('*' <$) . show . unSecret

class FromParam a where
    -- | Parse a parameter
    fromParam :: String -> Either String a
    default fromParam :: (Typeable a, Read a) => String -> Either String a
    fromParam = readFromParam

    fromParamList :: String -> Either String [a]
    fromParamList _ = Left "No implementation for fromParamList"

-- | A reasonable default implementation for 'fromParam' via 'Read'
readFromParam :: forall a. (Typeable a, Read a) => String -> Either String a
readFromParam str = maybe (Left err) Right $ readMaybe str
    where
        err = unwords ["failed to parse", str, "as", show (typeRep (Proxy :: Proxy a))]

instance (Typeable a, FromParam a) => FromParam (Secret a) where
    fromParam str = either (const err) (pure . Secret) $ fromParam str where
        err = Left $ unwords ["failed to parse", '*' <$ str, "as", show (typeRep (Proxy :: Proxy a))]

instance FromParam Bool where
    fromParam str = case map toLower str of
        "false" -> Right False
        "true" -> Right True
        _ -> Left "Expected true or false"

instance FromParam Char where
    fromParam [c] = Right c
    fromParam _ = Left "Got more than one character"
    fromParamList = Right

instance FromParam a => FromParam [a] where
    fromParam = fromParamList

instance FromParam Int
instance FromParam Integer
instance FromParam PortNumber

instance FromParam ByteString where
    fromParam str
        | all ((<128) . fromEnum) str = Right $ BC.pack str
        | otherwise = Left "expected ByteString, but found a non-ASCII character"

parseEnvs :: (TraversableB h, ConstraintsB h, AllB FromParam h) => h (Const String) -> IO (h Maybe)
parseEnvs envNames = do
    envs <- getEnvironment
    either fail pure $ btraverseC @FromParam
        (\(Const k) -> traverse (first ((k ++ ": ") ++) . fromParam) (lookup k envs))
        envNames

parseOpts :: (TraversableB h, ConstraintsB h, AllB FromParam h) => h (Const String) -> IO (h Maybe)
parseOpts optNames = do
    let opts = btraverseC @FromParam
            (\(Const k) -> optional $ mkOption k)
            optNames
    O.execParser $ O.info (opts <**> O.helper) mempty
  where
    mkOption k = O.option (O.eitherReader fromParam) $ O.long k

resolve :: (String -> IO ()) -> (Dict Show :*: Const String) a -> Maybe a -> (Const String :*: Maybe) a -> Maybe a -> IO a
resolve logger (Dict :*: Const key) def (Const envName :*: env) opt = case opt of
    Nothing -> case env of
        Nothing -> case def of
            Nothing -> fail $ "No default value is provided for " <> key
            Just v -> explain v "default"
        Just v -> explain v $ "environment variable " <> envName
    Just v -> explain v "command line argument"
  where
    explain v src = v <$ logger (unwords [key <> ":", "using", show v, "from the", src])

data Panfiguration h = Panfiguration
    { envNames :: h (Const String) -- ^ a record of environment variable names
    , optNames :: h (Const String) -- ^ a record of command line option names
    , defaults :: h Maybe -- ^ default values
    }

-- | Create a sensible 'Panfiguration' for a record type where the field names are snake_case.
-- The corresponding environment variables are SNAKE_CASE, and the option names are kebab-case.
snake :: (ApplicativeB h, FieldNamesB h) => Panfiguration h
snake = Panfiguration
    { envNames = bmap (first (map toUpper)) bfieldNames
    , optNames = bmap (first snakeToKebab) bfieldNames
    , defaults = bpure Nothing
    }

-- | Provide all the default values by a plain record
fromDefaults :: (BareB b, FunctorB (b Covered)) => b Bare Identity -> b Covered Maybe
fromDefaults = bmap (Just . runIdentity) . bcover

-- | Parse all the relevant environment variables and command line options, then merges them.
-- Each parameter is overriden in following order of priority:
--
-- * Command line option
-- * Environment variable
-- * Default
run :: (BareB b
    , FieldNamesB (b Covered)
    , TraversableB (b Covered)
    , ApplicativeB (b Covered)
    , ConstraintsB (b Covered)
    , AllB Show (b Covered)
    , AllB FromParam (b Covered))
    => (String -> IO ()) -- ^ logger
    -> Panfiguration (b Covered)
    -> IO (b Bare Identity)
run logger Panfiguration{..} = do
    envs <- parseEnvs envNames
    opts <- parseOpts optNames
    fmap bstrip $ bsequence' $ bzipWith4
        (resolve logger)
        (bzipWith (:*:) bdicts bfieldNames)
        defaults
        (bzipWith (:*:) envNames envs)
        opts

snakeToKebab :: String -> String
snakeToKebab = map go where
    go '_' = '-'
    go c = c
