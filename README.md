Example
----

```haskell
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

getServerArgs :: IO ServerArgs
getServerArgs = Panfiguration.run putStrLn snake
    { envNames = (envNames snake)
        { http_host = "HTTP_HOST"
        , http_port = "HTTP_PORT"
        }
    , defaults = ServerArgs
        { http_host = Just "0.0.0.0"
        , http_port = Just 8080
        , enable_service_log = Just True
        , environment = Nothing -- required parameter
        }
    }
```
