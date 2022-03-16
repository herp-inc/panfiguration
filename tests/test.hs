{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
import Barbies.TH
import Panfiguration

passthroughBareB [d|
    data ServerArgs = ServerArgs
        { http_host :: String
        , http_port :: Int
        , enable_service_log :: Bool
        , environment :: String
        }
    |]

deriving instance Show ServerArgs

getServerArgs :: IO ServerArgs
getServerArgs = run $ mconcat
    [ logger putStrLn
    , declCase snake
    , envs `withNames` \names -> names
        { http_host = "HTTP_HOST"
        , http_port = "HTTP_PORT"
        }
    , opts `asCase` kebab
    , fullDefaults ServerArgs
        { http_host = "0.0.0.0"
        , http_port = 8080
        , enable_service_log = True
        , environment = "dev"
        }
    ]

main = getServerArgs >>= print