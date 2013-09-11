{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.SqlDelta.Ast (AttributeDef(..),
                              Statement(..),
                              Substatement(..),
                              (<=>),
                              defaultParseFlags,
                              parseStatements,
                              toSubs) where

import Database.HsSqlPpp.Ast (AttributeDef(..),
                              Constraint(..),
                              Name(..),
                              NameComponent(..),
                              RowConstraint(..),
                              Statement(CreateTable),
                              TypeName(..),
                              nameComponents)

import Database.SqlDelta.Util

import Database.HsSqlPpp.Parser (parseStatements, defaultParseFlags)
import Data.List (foldl')

data Substatement = forall a . (Shown a, Eq a, Ord a) => Substatement a

toSubs :: (Shown a, Eq a, Ord a) => [a] -> [Substatement]
toSubs = map Substatement

-- not sure why it's name RowConstraint in hssqlppp
type ColumnConstraint = RowConstraint

class (Ord a) => Diff a where
  (<=>) :: a -> a -> Bool
  (<=>) x y = EQ == (compare x y)

  (</=>) :: a -> a -> Bool
  (</=>) x y = not (x <=> y)

class (Show a) => Shown a where
  shown :: a -> String
  shown = show

instance (Shown a) => Shown [a] where
  shown as = foldl' (\acc x -> acc ++ (shown x) ++ " ") "" as

instance Shown Statement where
  shown (CreateTable _ name attrs constraints) =
    shown name ++ " " ++ shown attrs ++ " " ++ shown constraints

instance Shown AttributeDef where
  shown (AttributeDef _ (Nmc name) typeName _ colConstraints) =
    name ++ (shown typeName)

instance Shown Constraint where
  shown (CheckConstraint _ s _) = s
  shown (PrimaryKeyConstraint _ s components) = s ++ shown components
  shown (UniqueConstraint _ s components) = s ++ shown components
  shown (ReferenceConstraint _ s fstComps name sndComps fstCascade sndCascade) =
    s ++ shown fstComps ++ (shown $ nameComponents name) ++ shown sndComps

instance Shown ColumnConstraint where
  shown (NotNullConstraint _ s) = s
  shown (NullConstraint _ s) = s
  shown (RowCheckConstraint _ s scalar) = s ++ (show scalar)
  shown (RowPrimaryKeyConstraint _ s) = s
  shown (RowUniqueConstraint _ s) = s
  shown (RowReferenceConstraint _ s name maybeNameComp c1 c2) =
    s ++ (shown name) ++ (show maybeNameComp) ++ (show c1) ++ (show c2)

instance Shown TypeName where
  shown (SimpleTypeName _ name) = shown name

instance Shown Name where
  shown name = shown $ nameComponents name

instance Shown NameComponent where
  shown (Nmc s) = s
  shown (QNmc s) = s

instance Ord Statement where
  compare (CreateTable _ fstName _ _) (CreateTable _ sndName _ _) =
    compare (shown fstName) (shown sndName)

instance Ord AttributeDef where
  compare fst snd = compare (shown fst) (shown snd)

instance Ord Constraint where
  compare fst snd = compare (shown fst) (shown snd)

instance Ord ColumnConstraint where
  compare fst snd = compare (shown fst) (shown snd)

instance Ord Substatement where
  compare (Substatement fst) (Substatement snd) = compare (shown fst) (shown snd)

instance Eq Substatement where
  (==) (Substatement fst) (Substatement snd) = (show fst) == (show snd)

instance Show Substatement where
  (show) (Substatement a) = show a

instance Diff AttributeDef
instance Diff Statement
