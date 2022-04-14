{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
module Panfiguration.FromParam (Secret(..), FromParam(..), readFromParam) where

import Data.ByteString.Char8 as BC (ByteString, pack)
import Data.Char
import Data.Monoid
import Data.Typeable
import Network.Socket (PortNumber)
import Text.Read (readMaybe)
import qualified Data.Text as Text

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

    -- | Merge two parameters. The 'Ordering' indicates which side of the arguments is used.
    mergeParams :: a -> a -> (Ordering, a)
    mergeParams a _ = (LT, a)

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
instance FromParam Text.Text where
    fromParam = pure . Text.pack

instance FromParam ByteString where
    fromParam str
        | all ((<128) . fromEnum) str = Right $ BC.pack str
        | otherwise = Left "expected ByteString, but found a non-ASCII character"

instance FromParam a => FromParam (Maybe a) where
    fromParam str = Just <$> fromParam str

instance FromParam Any where
    fromParam = fmap Any . fromParam
    mergeParams (Any False) a = (GT, a)
    mergeParams (Any True) _ = (LT, Any True)

instance FromParam All where
    fromParam = fmap All . fromParam
    mergeParams (All False) _ = (LT, All False)
    mergeParams (All True) a = (GT, a)
