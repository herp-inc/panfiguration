{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Barbies.TH
import Panfiguration

passthroughBareB [d|
    data Listen = Listen
        { host :: String
        , port :: Int
        }
    |]

instance Panfigurable Listen where
    template = bareTemplate

passthroughBareB [d|
    data ServerArgs = ServerArgs
        { http :: Listen
        , enable_service_log :: Bool
        , environment :: String
        }
    |]

deriving instance Show Listen
deriving instance Show ServerArgs

getServerArgs :: IO ServerArgs
getServerArgs = run $ mconcat
    [ logger putStrLn
    , declCase snake
    , opts `asCase` kebab
    , fullDefaults ServerArgs
        { http = Listen "0.0.0.0" 8080
        , enable_service_log = True
        , environment = "dev"
        }
    ]

main :: IO ()
main = getServerArgs >>= print