{-# LANGUAGE MultiParamTypeClasses #-}

module Database.SqlDelta.Util (symmetricDiffList, symmetricDiffSet ) where

import Data.Set as Set (Set, (\\), fromList)

symmetricDiff :: (Ord b) => (a b -> Set b) -> a b -> a b -> (Set b, Set b)
symmetricDiff toSet old new =
  let oldSet = toSet old
      newSet = toSet new
  in (oldSet \\ newSet, newSet \\ oldSet)

symmetricDiffSet :: (Ord b) => Set b -> Set b -> (Set b, Set b)
symmetricDiffSet = symmetricDiff id

symmetricDiffList :: (Ord b) => [b] -> [b] -> (Set b, Set b)
symmetricDiffList = symmetricDiff fromList