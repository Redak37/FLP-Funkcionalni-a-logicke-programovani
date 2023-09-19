{-
    Řešení: FLP Funkcionální projekt - Simplify-BKG
    Autor: Radek Duchoň, xducho07
    Datum: 10. 4. 2021
-}

module SimplifyBKGParse where

import Data.List.Split (splitOn)
import SimplifyBKGData (BKG(..), Rule(..))


-- Zpracování pravidel
parseRule :: String -> Rule
parseRule rule = splitRule (splitOn "->" rule)
            where
                splitRule [[leftSide],rightSide] = Rule leftSide rightSide
                splitRule _ = Rule 'e' "Error" -- Error

-- Předevedení pole řetězců do interní reprezentace
createBKG :: [String] ->  BKG
createBKG (nonterms:terms:[startState]:rules) = BKG (splitOn "," nonterms) (splitOn "," terms) startState (map parseRule rules)
createBKG _ = BKG [] [] ' ' [] -- Chybně zadaná gramatika