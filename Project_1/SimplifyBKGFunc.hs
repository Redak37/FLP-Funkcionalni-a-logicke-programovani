{-
    Řešení: FLP Funkcionální projekt - Simplify-BKG
    Autor: Radek Duchoň, xducho07
    Datum: 10. 4. 2021
-}

module SimplifyBKGFunc where

import Data.Char (isLower, isUpper)
import System.Exit (die)
import Control.Monad (when)
import SimplifyBKGData (BKG(..), Rule(..))

-- Odstraňuje duplicity
uniq :: Eq a => [a] -> [a]
uniq = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

-- Odstraní dupllikáty neterminálů, terminálů a pravidel
deduplicateBKG :: BKG -> BKG
deduplicateBKG bkg = BKG {
    nonterms = uniq (nonterms bkg),
    terms = uniq (terms bkg),
    start = start bkg,
    rules = uniq (rules bkg)
}

-- Kontrola, zda obsahuje seznam stringů po jednom neterminálu
checkNtT :: [String] -> Bool
checkNtT ntt = (length (concat ntt) /= length(ntt) || "" `elem` ntt)

-- Kontrola požadavků na zadané neterminály
checkNonterms :: [String] -> Bool
checkNonterms nonterms = (nonterms == [] || checkNtT nonterms || not (all isUpper (concat nonterms)))

-- Kontrola požadavků na zadané terminály
checkTerms :: [String] -> Bool
checkTerms terms = (checkNtT terms || not (all isLower (concat terms)))

-- Zkontroluje validitu levé strany pravidel
checkLeftRules :: String -> [Rule] -> Bool
checkLeftRules nonterms (rule:rulesList) = not ((leftSide rule) `elem` nonterms) || checkLeftRules nonterms rulesList
checkLeftRules _ [] = False

-- Zkontroluje zda každý znak řetězce patří do dané abecedy
stringAlpha :: String -> String -> Bool
stringAlpha (x:xs) alphabet = (x `elem` alphabet) && stringAlpha xs alphabet
stringAlpha [] _ = True

-- Kontrola pravé strany pravidla
checkRightSide :: String -> String -> Bool
checkRightSide rightSide alphabet
    | rightSide == "#" = False
    | rightSide == "" = True
    | otherwise = not (stringAlpha rightSide alphabet)

-- Zkontroluje validitu pravé strany pravidel
checkRightRules :: [Rule] -> String -> Bool
checkRightRules (rule:rulesList) alphabet = checkRightSide (rightSide rule) alphabet || checkRightRules rulesList alphabet
checkRightRules [] _ = False

-- Kontrola syntaxe BKG
syntaxCheckBKG :: BKG -> IO()
syntaxCheckBKG bkg = do
    when (checkNonterms (nonterms bkg)) $ die "Nekorektní gramatika - Neterminály"
    when (checkTerms (terms bkg)) $ die "Nekorektní gramatika - Terminály"
    when (not (isUpper (start bkg))) $ die "Nekorektní gramatika - Počáteční neterminál"
    when (checkLeftRules (concat (nonterms bkg)) (rules bkg)) $ die "Nekorektní gramatika - Pravidla - levá strana"
    when (checkRightRules (rules bkg) ((concat (nonterms bkg)) ++ (concat (terms bkg)))) $ die "Nekorektní gramatika - Pravidla - pravá strana"

-- Provedení jednoho kroku získávání terminujících terminálů
getNewNt :: [Rule] -> String -> [String] -> [String]
getNewNt (rule:ruleList) alphabet nt 
    | [leftSide rule] `elem` nt = getNewNt ruleList alphabet nt
    | stringAlpha (rightSide rule) (alphabet ++ "#") = getNewNt ruleList alphabet (nt ++ [[leftSide rule]])
    | otherwise = getNewNt ruleList alphabet nt
getNewNt [] _ nt = nt

-- Ziskání terminujících neterminálů
getNt :: BKG -> [String] -> [String]
getNt bkg nt = do
    let possible = (concat (terms bkg)) ++ (concat nt)
    let nt_new = getNewNt (rules bkg) possible []
    if nt /= nt_new then
        getNt bkg nt_new
    else if [start bkg] `elem` nt_new then
        nt_new
    else
        [[start bkg]] ++ nt_new

-- Redukce pravidel na základě terminujících neterminálů
reduceRules :: [Rule] -> [String] -> String -> [Rule] -> [Rule]
reduceRules (rule:ruleList) nt alphabet reduced
    | [leftSide rule] `elem` nt && stringAlpha (rightSide rule) alphabet = reduceRules ruleList nt alphabet (reduced ++ [rule])
    | otherwise = reduceRules ruleList nt alphabet reduced
reduceRules [] _ _ reduced = reduced

-- Získávání platných terminálů z pravých stran pravidel
getRightN :: String -> [String] -> String -> [String]
getRightN (x:xs) nt alphabet
    | x `elem` alphabet = getRightN xs nt alphabet
    | [x] `elem` nt = getRightN xs nt alphabet
    | otherwise = getRightN xs (nt ++ [[x]]) alphabet
getRightN [] nt alphabet = nt

-- Získávání použitelných neterminálů
getUsableN :: [Rule] -> [String] -> String -> [String]
getUsableN (rule:ruleList) nt alphabet
    | [leftSide rule] `elem` nt = getUsableN ruleList (getRightN (rightSide rule) nt alphabet) alphabet
    | otherwise = getUsableN ruleList nt alphabet
getUsableN [] nt _ = nt

-- Ziskání finálníh neterminálů generujíćich gramatiku
getFinalN :: [Rule] -> [String] -> String -> [String]
getFinalN rules nt terms = do
    let alphabet = terms ++ (concat nt)
    let nt_new = getUsableN rules nt alphabet
    if nt /= nt_new then getFinalN rules nt_new alphabet else nt_new

-- Zkontroluje, zda je terminál obsažen v nějakém pravidle
ruleTerm :: [Rule] -> Char -> Bool
ruleTerm (rule:rules) term = term `elem` (rightSide rule) || ruleTerm rules term
ruleTerm [] _ = False

-- Vrátí zredukovanou množinu terminálů použitých v pravidlech
reduceTerms :: [Rule] -> String -> [String]
reduceTerms rules (term:termList)
    | ruleTerm rules term = [[term]] ++ (reduceTerms rules termList)
    | otherwise = reduceTerms rules termList
reduceTerms _ [] = []