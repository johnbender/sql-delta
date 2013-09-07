module Database.SqlDelta.Core (reconcile) where

import Database.HsSqlPpp.Ast (Statement(CreateTable),
                              AttributeDef(..),
                              NameComponent(..),
                              nameComponents)

import Database.HsSqlPpp.Parser (parseStatements, defaultParseFlags)
import Database.HsSqlPpp.Catalog ()
import Data.Either (rights)
import Data.Text.Lazy (pack)
import Data.List (zip)
import Text.Show.Pretty (ppShow)

class (Eq a) => Diff a where
    -- check for a difference
    (<=>)  :: a -> a -> Bool
    (<=>) = (==)

    (</=>) :: a -> a -> Bool
    (</=>) a b = not (a <=> b)

instance Diff Statement where
    (<=>) (CreateTable _ x1 y1 z1) (CreateTable _ x2 y2 z2) =
        (x1, y1, z1) == (x2, y2, z2)

instance Diff AttributeDef where
    (<=>) (AttributeDef _ x1 y1 z1 xx1) (AttributeDef _ x2 y2 z2 xx2) =
        (x1, y1, z1, xx1) == (x2, y2, z2, xx2)


reconcile old new = do
  oldSql <- readFile old
  newSql <- readFile new
  return $ ppShow $ zipWith diff (parse old oldSql) (parse new oldSql)

parse path sql =
    let lines = Nothing
        flags = defaultParseFlags
    in case parseStatements flags path lines (pack sql) of
         Right x -> x
         _       -> []

diff old new | old <=> new  = []
             | otherwise    = [new] --  new -- diff' old new []

-- TODO handle constraints
diff' old@(CreateTable _ _ _ _) new@(CreateTable _ _ _ _) diffs =
    diffAttributes (attrs old) (attrs new) diffs

diff' (CreateTable _ _ _ _) x diffs = ("", ""):diffs

diffAttributes old new diffs =
    let diffNames = [(attrName x, attrName y) | (x, y) <- zip old new ]
    in diffNames ++ diffs

attrs (CreateTable _ _ as _) = as

tableName (CreateTable _ n _ _) = (map (\(Nmc s) -> s) (nameComponents n))

attrName (AttributeDef _ (Nmc s) _ _ _) = s
