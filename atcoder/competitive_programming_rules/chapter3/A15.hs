module A15 where

import Data.List ( sort )
import qualified Data.Set as Set

erase :: [Int] -> [Int]
erase xs = Set.toList $ Set.fromList xs