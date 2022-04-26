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
        }
    |]

deriving instance Show Bind
deriving instance Show ServerArgs

getServerArgs :: IO ServerArgs
getServerArgs = run $ mconcat
    [ logger putStrLn
    , declCase snake
    , opts `asCase` kebab
    , fullDefaults ServerArgs
        { http = Bind "0.0.0.0" 8080
        , enable_service_log = True
        , environment = "dev"
        }
    ]

main :: IO ()
main = getServerArgs >>= print

_unused :: ()
_unused = Proxy @BindH `seq` Proxy @ServerArgsH `seq` ()