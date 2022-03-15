{-# LANGUAGE PatternSynonyms #-}
module Panfiguration (
    Panfiguration,
    decCase,
    asCase,
    withNames,
    envs,
    opts,
    defaults,
    fullDefaults,
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
    Secret(..)
    ) where

import Panfiguration.Core
import Panfiguration.Case
import Panfiguration.FromParam