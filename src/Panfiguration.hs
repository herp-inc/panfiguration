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
    run,
    -- * Nested panfiguration
    Panfigurable(..),
    bareTemplate,
    -- * Naming convention
    Case(..),
    camel,
    snake,
    pattern SNAKE,
    pattern KEBAB,
    kebab,
    -- * Parameter
    FromParam,
    Secret(..)
    ) where

import Panfiguration.Core
import Panfiguration.Case
import Panfiguration.FromParam