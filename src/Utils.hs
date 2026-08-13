module Utils
    ( lireMotDePasse
    , lireMotDePasseValide
    , genererReference
    , referencesInfinies
    , lireDouble
    , pause
    ) where

import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
    ( hSetEcho
    , stdin
    )

import Validation (motDePasseValide)

-- Liste infinie évaluée paresseusement.
-- Elle permet également de démontrer l'évaluation paresseuse de Haskell.
referencesInfinies :: [String]
referencesInfinies =
    map
        (\n -> "TX-" ++ show (n :: Integer))
        [1 ..]

-- Génération d'une référence temporelle
genererReference :: IO String
genererReference = do
    timestamp <- getPOSIXTime

    let t :: Integer
        t = round timestamp

    return ("TX-" ++ show t)

-- Lecture sécurisée d'un montant
lireDouble :: IO Double
lireDouble = do
    ligne <- getLine

    case reads ligne of
        [(montant, "")] ->
            return montant

        _ -> do
            putStrLn "Montant invalide. Entrez un nombre."
            lireDouble

-- Pause
pause :: IO ()
pause = do
    putStrLn ""
    putStrLn "Appuyez sur Entree pour continuer..."
    _ <- getLine
    return ()

-- ============================================================
-- SAISIE SECURISEE DU MOT DE PASSE
-- ============================================================

lireMotDePasse :: IO String
lireMotDePasse = do
    putStr "Mot de passe : "

    hSetEcho stdin False

    motDePasse <- getLine

    hSetEcho stdin True

    putStrLn ""

    return motDePasse

-- Meme saisie, mais controlee : redemande tant que le mot de
-- passe ne fait pas au moins 4 caracteres (Validation.motDePasseValide).
lireMotDePasseValide :: IO String
lireMotDePasseValide = do
    motDePasse <- lireMotDePasse

    if motDePasseValide motDePasse
        then return motDePasse
        else do
            putStrLn "Mot de passe trop court (4 caracteres minimum)."
            lireMotDePasseValide
