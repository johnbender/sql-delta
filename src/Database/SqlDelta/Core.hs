module Database.SqlDelta.Core (reconcile) where

import Database.SqlDelta.Ast (AttributeDef(..),
                              Statement(..),
                              Substatement(..),
                              defaultParseFlags,
                              parseStatements,
                              toSubs)

import Database.SqlDelta.Util (symmetricDiffSet, symmetricDiffList)

import Data.Either (rights)
import Data.Set as Set (Set(..), (\\), fromList, toAscList, null)
import Text.Show.Pretty (ppShow)
import Data.Text.Lazy (pack)
import Data.List (zip, foldl', sort)

reconcile old new = do
  oldSql <- readFile old
  newSql <- readFile new
  return $ diff (parse old oldSql) (parse new newSql)

parse path sql =
    let lines = Nothing
        flags = defaultParseFlags
    in case parseStatements flags path lines (pack sql) of
         Right x -> x
         _       -> []

diff old new =
  -- get the items only in the old and only in the new for removal/addition
  -- get the matching items and diff them
  let (onlyOld, onlyNew) = symmetricDiffList old new
      matchOld = toAscList $ (fromList old) \\ onlyOld
      matchNew = toAscList $ (fromList new) \\ onlyNew
  in concat $ zipWith diffStatement matchOld matchNew

-- TODO abstract the "Statement" in `diffStatement` using quantified wrapper
--      most likely part of the Diff typeclass as `<>` and implemented for
--      the relevant ast types
diffStatement (CreateTable _ _ oldAttrs oldCstr) (CreateTable _ _ newAttrs newCstr) =
  -- diff the table constrains
  (symmetricDiffList (toSubs oldCstr) (toSubs newCstr))

  -- diff the attributes to get the old and the new
  : (symmetricDiffList (toSubs oldAttrs) (toSubs newAttrs))

  -- diff the contraints and other parts of the attributes
  : (diffAttrs oldAttrs newAttrs)

diffAttrs :: [AttributeDef] -> [AttributeDef] -> [(Set Substatement, Set Substatement)]
diffAttrs old new =
  let oldConstraints =  sort $ map colConstraints old
      newConstraints = sort $ map colConstraints new
  in zipWith (\a b -> symmetricDiffList a b ) oldConstraints newConstraints

constraints (CreateTable _ _ _ cs) =  map Substatement cs
colConstraints (AttributeDef _ _ _ _ cs) = map Substatement cs
