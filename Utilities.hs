module Utilities where

import Data.List

import Data.Map (Map)
import qualified Data.Map as Map

elemsAll :: (a -> Bool) -> Map k a -> Bool
elemsAll p = Map.fold ((&&) . p) True

-- Pretty printing

between l r s = l ++ s ++ r

angles = between "<" ">"
braces = between "{" "}"

sepBy c = concat . (intersperse c)

sepByComma = sepBy ","

proj31 (x1,x2,x3) = x1
proj32 (x1,x2,x3) = x2
