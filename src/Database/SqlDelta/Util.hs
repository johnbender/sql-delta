{-# LANGUAGE MultiParamTypeClasses #-}

module Database.SqlDelta.Util (symmetricDiff ) where

import Data.Set as Set (Set, (\\), fromList, toAscList)

symmetricDiff :: (Ord b) => [b] -> [b] -> ([b], [b])
symmetricDiff old new =
  let oldSet = fromList old
      newSet = fromList new
  in (toAscList $ oldSet \\ newSet, toAscList $ newSet \\ oldSet)