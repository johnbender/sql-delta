module Database.SqlDelta.Ast (Statement(..),
                              (<=>),
                              defaultParseFlags,
                              parseStatements) where

import Database.HsSqlPpp.Ast (Statement(CreateTable),
                              AttributeDef(..),
                              NameComponent(..),
                              TypeName(..),
                              Name(..),
                              Constraint(..),
                              nameComponents)

import Database.HsSqlPpp.Parser (parseStatements, defaultParseFlags)
import Database.HsSqlPpp.Catalog ()
import Data.List (zip, foldl')

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
    shown (AttributeDef _ (Nmc name) typeName _ _) = name ++ (shown typeName)

instance Shown Constraint where
    shown (CheckConstraint _ s _) = s
    shown (PrimaryKeyConstraint _ s components) = s ++ shown components
    shown (UniqueConstraint _ s components) = s ++ shown components
    shown (ReferenceConstraint _ s fstComps name sndComps fstCascade sndCascade) =
        s ++ shown fstComps ++ (shown $ nameComponents name) ++ shown sndComps

instance Shown TypeName where
    shown (SimpleTypeName _ name) = shown name

instance Shown Name where
    shown name = shown $ nameComponents name

instance Shown NameComponent where
    shown (Nmc s) = s
    shown (QNmc s) = s

instance Ord Statement where
    compare fst@(CreateTable _ _ _ _) snd@(CreateTable _ _ _ _) =
        compare (shown fst) (shown snd)

instance Ord AttributeDef where
    compare fst snd = compare (shown fst) (shown snd)

instance Diff AttributeDef
instance Diff Statement
