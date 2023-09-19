{-
    Řešení: FLP Funkcionální projekt - Simplify-BKG
    Autor: Radek Duchoň, xducho07
    Datum: 10. 4. 2021
-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import SimplifyBKGData (BKG(..), Rule(..))
import SimplifyBKGFunc (deduplicateBKG, getFinalN, getNt, reduceRules, reduceTerms, syntaxCheckBKG)
import SimplifyBKGParse (createBKG)


main :: IO ()
main = do
    (action, input) <- procArgs =<< getArgs
    action input

-- Zpracování příkazového řádku
procArgs :: [String] -> IO (String -> IO (), String)
procArgs [x] = procArgs [x, []]
procArgs [x,y] = do
    input <- if y == [] then getContents else readFile y
    case x of
     "-i" -> return (intern, input)
     "-1" -> return (step1, input)
     "-2" -> return (step2, input)
     _    -> die ("unknown option " ++ x)
procArgs _ = die "Očekáván 1 nebo 2 arggumenty: [-i|-1|-2] SOUBOR"

-- Vypíše vypíše gramatiku načtenou do interní reprezentace
intern :: String -> IO ()
intern input = print (createBKG (lines input))

-- Vypíše gramatiku po vykonání první kroku algoritmu
step1 :: String -> IO ()
step1 input = do
    let bkg = deduplicateBKG (createBKG (lines input))
    syntaxCheckBKG bkg
    let nt = getNt bkg []
    let newRules = reduceRules (rules bkg) nt ((concat (terms bkg)) ++ "#" ++ (concat nt)) []
    let newTerms = reduceTerms newRules (concat (terms bkg))
    print (BKG nt newTerms (start bkg) newRules)

-- Vypíše gramatiku po vykonání druhého kroku algoritmu
step2 :: String -> IO ()
step2 input = do
    let bkg = deduplicateBKG (createBKG (lines input))
    syntaxCheckBKG bkg
    let nt = getNt bkg []
    let newRules = reduceRules (rules bkg) nt ((concat (terms bkg)) ++ "#" ++ (concat nt)) []
    let finalN = getFinalN newRules [[start bkg]] ((concat (terms bkg)) ++ "#")
    let finalRules = reduceRules (rules bkg) finalN ((concat (terms bkg)) ++ "#" ++ (concat finalN)) []
    let finalTerms = reduceTerms finalRules (concat (terms bkg))
    print (BKG finalN (finalTerms) (start bkg) finalRules)