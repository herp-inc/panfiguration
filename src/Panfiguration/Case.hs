{-# LANGUAGE PatternSynonyms #-}
module Panfiguration.Case
    ( Case(..)
    , camel
    , snake
    , pattern SNAKE
    , pattern KEBAB
    , kebab
    -- * Operation
    , split
    , join
    , convert
    )
    where

import Data.Char
import qualified Data.List.Split as S
import Data.List (intercalate)

data Case = Delimiter String
    | AsIs
    | Camel
    | LowerCamel
    | Upper Case
    | Lower Case
    | Prefixed String Case

camel :: Case
camel = LowerCamel

snake :: Case
snake = Lower $ Delimiter "_"

pattern SNAKE :: Case
pattern SNAKE = Upper (Delimiter "_")

kebab :: Case
kebab = Lower $ Delimiter "-"

pattern KEBAB :: Case
pattern KEBAB = Upper (Delimiter "-")

split :: Case -> String -> [String]
split AsIs = pure
split (Delimiter d) = S.splitOn d
split Camel = splitCamel
split LowerCamel = splitCamel
split (Lower c) = split c
split (Upper c) = split c
split (Prefixed p c) = \str -> case split c str of
    x : xs | x == p -> xs
    xs -> xs

splitCamel :: String -> [String]
splitCamel = concatMap (bravo "") . alpha "" where
    alpha "" "" = []
    alpha buf "" = [reverse buf]
    alpha buf (x:u:l:xs) | isUpper u && isLower l = reverse (x : buf) : alpha [l, u] xs
    alpha buf (x:xs) = alpha (x : buf) xs
    
    bravo "" "" = []
    bravo buf "" = [reverse buf]
    bravo buf (l:u:xs) | isLower l && isUpper u = reverse (l : buf) : bravo [u] xs
    bravo buf (x:xs) = bravo (x : buf) xs

join :: Case -> [String] -> String
join AsIs xs = concat xs
join (Delimiter d) xs = intercalate d xs
join Camel xs = concatMap capitalise xs
join LowerCamel (x : xs) = x <> concatMap capitalise xs
join LowerCamel [] = ""
join (Upper c) xs = map toUpper $ join c xs
join (Lower c) xs = map toLower $ join c xs
join (Prefixed str c) xs = join c $ str : xs
 
capitalise :: String -> String
capitalise (x : xs) = toUpper x : xs
capitalise xs = xs

convert :: Case -> Case -> String -> String
convert c d = join d . split c