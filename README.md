Usage
----

Panfiguration is a library that provides a composable, automatically-derived interface for configuration parameters.

Currently three backends are supported; `envs` for environment variables, `opts` for command-line options and `defaults` for default values.
The `Monoid` instance makes these backends composable. See the example below for the basic usage.

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
getServerArgs = run putStrLn $ mconcat
    [ decCase snake
    , envs `withNames` \names -> names
        { http_host = "HTTP_HOST"
        , http_port = "HTTP_PORT"
        }
    , opts `asCase` kebab
    , defaults ServerArgs
        { http_host = Just "0.0.0.0"
        , http_port = Just 8080
        , enable_service_log = Just True
        , environment = Nothing -- required parameter
        }
    ]
```

Naming conventions
----

`decCase` specifies the naming convention of the Haskell data declaration (the default is `camel`).
The naming conventions are configurable by the `asCase` modifier.
By default, `envs` and `opts` uses SNAKE_CASE and kebab-case respectively.

The following styles are supported:

```haskell
AsIs
Camel
camel
snake
SNAKE
kebab
KEBAB
Prefixed <str>
```

You can also override individual names directly by `withNames`.
