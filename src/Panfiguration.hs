{-# LANGUAGE PatternSynonyms #-}
module Panfiguration (
    Panfiguration,
    declCase,
    asCase,
    withNames,
    envs,
    opts,
    defaults,
    fullDefaults,
    logger,
    errorLogger,
    run,
    -- * Naming convention
    Case(..),
    camel,
    snake,
    pattern SNAKE,
    pattern KEBAB,
    kebab,
    -- * Parameter
    FromParam,
    Secret(..),
    Collect(..),
    ) where

import Panfiguration.Core
import Panfiguration.Case
import Panfiguration.FromParam