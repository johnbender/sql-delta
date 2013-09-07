module Database.SqlDelta.Core (reconcile) where

import Database.HsSqlPpp.Ast (Statement(CreateTable),
                              NameComponent(..),
                              nameComponents)

import Database.HsSqlPpp.Parser (parseStatements, defaultParseFlags)
import Database.HsSqlPpp.Catalog ()
import Data.Either (rights)
import Data.Text.Lazy (pack)
import Data.List (zip)


reconcile old new = do
  oldSql <- readFile old
  newSql <- readFile new
  return $ zipWith diff (parse old oldSql) (parse new oldSql)

parse path sql =
    let lines = Nothing
        flags = defaultParseFlags
    in case parseStatements flags path lines (pack sql) of
         Right x -> x
         _       -> []

diff old new | old == new = []
             | otherwise  = diff' old new []

-- TODO handle constraints
diff' old@(CreateTable _ oldName _ _) new@(CreateTable _ newName _ _) diffs =
    if (name old) /= (name new) then
        (show new):diffs
    else
        diffAttributes (attrs old) (attrs new) diffs

diff' (CreateTable _ _ _ _) x diffs = (show x):diffs

diffAttributes old new diffs = [ show y | (x, y) <- (zip old new), x /= y ] ++ diffs

attrs (CreateTable _ _ as _) = as
name (CreateTable _ n  _ _) = show (map (\(Nmc s) -> s) (nameComponents n))