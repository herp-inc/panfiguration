{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import Barbies.TH
import Data.Proxy
import Panfiguration

passthroughBareB [d|
    data Bind = Bind { host :: String, port :: Int }
    data ServerArgs = ServerArgs
        { http :: Bind
        , enable_service_log :: Bool
        , environment :: String
        , extensions :: Collect String
        }
    |]

deriving instance Show Bind
deriving instance Show ServerArgs

getServerArgs :: IO ServerArgs
getServerArgs = run $ mconcat
    [ logger putStrLn
    , declCase snake
    , opts `asCase` kebab
    , envs
    , defaults ServerArgs
        { http = Bind (Just "0.0.0.0") (Just 8080)
        , enable_service_log = Just True
        , environment = Nothing
        , extensions = Just $ Collect []
        }
    ]

main :: IO ()
main = getServerArgs >>= print

_unused :: ()
_unused = Proxy @BindH `seq` Proxy @ServerArgsH `seq` ()