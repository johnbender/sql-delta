module Database.SqlDelta.Core (reconcile) where

import Database.SqlDelta.Ast (Statement(..),
                              (<=>),
                              defaultParseFlags,
                              parseStatements)

import Data.Either (rights)
import Data.Set (difference, fromList, toList)
import Data.Text.Lazy (pack)
import Text.Show.Pretty (ppShow)

reconcile old new = do
  oldSql <- readFile old
  newSql <- readFile new
  return $ ppShow $ zipWith diff (parse old oldSql) (parse new newSql)

parse path sql =
    let lines = Nothing
        flags = defaultParseFlags
    in case parseStatements flags path lines (pack sql) of
         Right x -> x
         _       -> []

diff old new | old <=> new = []
             | otherwise   = diffStatement old new []

-- TODO handle constraints
diffStatement old@(CreateTable _ _ _ _) new@(CreateTable _ _ _ _) diffs =
    diffAttributes (attrs old) (attrs new) diffs

diffStatement (CreateTable _ _ as _) x diffs = as ++ diffs

diffAttributes old new diffs =
    (toList $ difference (fromList old) (fromList new)) ++ diffs

attrs (CreateTable _ _ as _) = as
