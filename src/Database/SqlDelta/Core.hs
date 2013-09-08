module Database.SqlDelta.Core (reconcile) where

import Database.HsSqlPpp.Ast (Statement(CreateTable),
                              AttributeDef(..),
                              NameComponent(..),
                              nameComponents)

import Database.HsSqlPpp.Parser (parseStatements, defaultParseFlags)
import Database.HsSqlPpp.Catalog ()
import Data.Either (rights)
import Data.Text.Lazy (pack)
import Data.List (zip, foldl')
import Data.Set (difference, fromList, toList)
import Text.Show.Pretty (ppShow)
import Debug.Trace (trace)

class (Ord a) => Diff a where
    -- check for a difference
    (<=>) :: a -> a -> Bool
    (<=>) x y = EQ == (compare x y)

    (</=>) :: a -> a -> Bool
    (</=>) x y = not (x <=> y)

-- replacement for derived show instances, inteded to exclude anotation info
class (Show a) => Shown a where
    shown :: a -> String
    shown = show

instance Shown Statement where
    shown (CreateTable _ x attrs z) =
        foldl' (\acc x -> acc ++ (shown x) ++ " ") "" attrs

instance Shown AttributeDef where
    shown (AttributeDef _ (Nmc name) _ _ _)  = name

instance Ord Statement where
    compare fst@(CreateTable _ _ _ _) snd@(CreateTable _ _ _ _) =
        compare (shown fst) (shown snd)

instance Ord AttributeDef where
    compare fst@(AttributeDef _ _ _ _ _) snd@(AttributeDef _ _ _ _ _) =
        compare (shown fst) (shown snd)

instance Diff AttributeDef
instance Diff Statement

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
             | otherwise    = diff' old new []

-- TODO handle constraints
diff' old@(CreateTable _ _ _ _) new@(CreateTable _ _ _ _) diffs =
    diffAttributes (attrs old) (attrs new) diffs

diff' (CreateTable _ _ as _) x diffs = as ++ diffs

diffAttributes old new diffs =
    (toList $ difference (fromList old) (fromList new)) ++ diffs

attrs (CreateTable _ _ as _) = as

tableName (CreateTable _ n _ _) = (map (\(Nmc s) -> s) (nameComponents n))

attrName (AttributeDef _ (Nmc s) _ _ _) = s
