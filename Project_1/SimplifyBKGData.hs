{-
    Řešení: FLP Funkcionální projekt - Simplify-BKG
    Autor: Radek Duchoň, xducho07
    Datum: 10. 4. 2021
-}

module SimplifyBKGData where

import Data.List (intercalate)

-- Typ pro pravidla
data Rule = Rule { 
    leftSide :: Char,
    rightSide :: [Char]
    } deriving (Eq)

-- Typ pro bezkontextovou gramatiku
data BKG = BKG { 
    nonterms :: [String],
    terms :: [String],
    start :: Char,
    rules :: [Rule]
    } deriving (Eq)    


instance Show Rule where
    show (Rule left right) = [left] ++ "->" ++ right


instance Show BKG where
    show (BKG nonterms terms startState rules) = (intercalate "," nonterms) ++ "\n" ++ intercalate "," terms ++ "\n" ++ [startState] ++ "\n" ++ intercalate "\n" (map show rules)    