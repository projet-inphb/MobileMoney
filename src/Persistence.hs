module Persistence
    ( initialiserDonnees
    , sauvegarderComptes
    , chargerComptes
    , sauvegarderTransaction
    , chargerTransactions
    ) where

import Types
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    )
import System.IO
    ( IOMode(WriteMode, AppendMode)
    , withFile
    , hSetEncoding
    , utf8
    , hPutStr
    )
import Control.Exception (evaluate)
import Text.Read (readMaybe)

-- ============================================================
-- FICHIERS DE DONNEES
-- ============================================================

fichierComptes :: FilePath
fichierComptes = "data/comptes.txt"

fichierTransactions :: FilePath
fichierTransactions = "data/transactions.txt"

-- ============================================================
-- INITIALISATION
-- ============================================================

initialiserDonnees :: IO ()
initialiserDonnees = do
    createDirectoryIfMissing True "data"

    comptesExiste <- doesFileExist fichierComptes

    if not comptesExiste
        then writeFile fichierComptes ""
        else return ()

    transactionsExiste <- doesFileExist fichierTransactions

    if not transactionsExiste
        then writeFile fichierTransactions ""
        else return ()

-- ============================================================
-- CONVERSION COMPTE -> TEXTE
-- ============================================================

serialiserCompte :: Compte -> String
serialiserCompte compte =
    numero compte
    ++ "|"
    ++ proprietaire compte
    ++ "|"
    ++ motDePasse compte
    ++ "|"
    ++ show (operateur compte)
    ++ "|"
    ++ show (solde compte)

-- ============================================================
-- CONVERSION TEXTE -> COMPTE
-- ============================================================

deserialiserCompte :: String -> Maybe Compte
deserialiserCompte ligne =
    case split "|" ligne of

        [num, prop, mdp, op, soldeTexte] -> do

            operateur' <- readMaybe op
            solde' <- readMaybe soldeTexte

            return
                (Compte
                    { numero = num
                    , proprietaire = prop
                    , motDePasse = mdp
                    , operateur = operateur'
                    , solde = solde'
                    })

        _ ->
            Nothing

-- ============================================================
-- SAUVEGARDE DES COMPTES
-- ============================================================

sauvegarderComptes :: [Compte] -> IO ()
sauvegarderComptes comptes = do
    initialiserDonnees

    withFile fichierComptes WriteMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle (unlines (map serialiserCompte comptes))

-- ============================================================
-- CHARGEMENT DES COMPTES
-- ============================================================

chargerComptes :: IO [Compte]
chargerComptes = do
    initialiserDonnees

    contenu <- readFile fichierComptes
    _ <- evaluate (length contenu)

    return
        [ compte
        | ligne <- lines contenu
        , Just compte <- [deserialiserCompte ligne]
        ]
-- ============================================================
-- TRANSACTION -> TEXTE
-- ============================================================

serialiserTransaction :: Transaction -> String
serialiserTransaction transaction =
    reference transaction
    ++ "|"
    ++ expediteur transaction
    ++ "|"
    ++ destinataire transaction
    ++ "|"
    ++ show (montant transaction)
    ++ "|"
    ++ show (typeTransaction transaction)

-- ============================================================
-- SAUVEGARDE D'UNE TRANSACTION
-- ============================================================

sauvegarderTransaction :: Transaction -> IO ()
sauvegarderTransaction transaction = do
    initialiserDonnees

    withFile fichierTransactions AppendMode $ \handle -> do
        hSetEncoding handle utf8
        hPutStr handle
            (serialiserTransaction transaction ++ "\n")

-- ============================================================
-- CHARGEMENT DES TRANSACTIONS
-- ============================================================

chargerTransactions :: IO [Transaction]
chargerTransactions = do
    initialiserDonnees

    contenu <- readFile fichierTransactions
    _ <- evaluate (length contenu)

    return
        [ transaction
        | ligne <- lines contenu
        , Just transaction <- [deserialiserTransaction ligne]
        ]

-- ============================================================
-- TEXTE -> TRANSACTION
-- ============================================================

deserialiserTransaction :: String -> Maybe Transaction
deserialiserTransaction ligne =
    case split "|" ligne of

        [ref, expediteur', destinataire', montantTexte, typeTexte] -> do

            montant' <- readMaybe montantTexte
            type' <- readMaybe typeTexte

            return
                (Transaction
                    { reference = ref
                    , expediteur = expediteur'
                    , destinataire = destinataire'
                    , montant = montant'
                    , typeTransaction = type'
                    })

        _ ->
            Nothing

-- ============================================================
-- SPLIT SIMPLE
-- ============================================================

split :: String -> String -> [String]
split separateur texte
    | separateur == "" =
        [texte]

    | otherwise =
        case breakOn separateur texte of

            Nothing ->
                [texte]

            Just (avant, apres) ->
                avant : split separateur apres

breakOn :: String -> String -> Maybe (String, String)
breakOn separateur texte =
    chercher texte
    where

        chercher [] =
            Nothing

        chercher xs
            | prefix separateur xs =
                Just
                    ( []
                    , drop (length separateur) xs
                    )

            | otherwise =
                case xs of

                    [] ->
                        Nothing

                    (x:reste) ->
                        case chercher reste of

                            Nothing ->
                                Nothing

                            Just (avant, apres) ->
                                Just
                                    (x : avant, apres)

prefix :: String -> String -> Bool
prefix [] _ =
    True

prefix _ [] =
    False

prefix (x:xs) (y:ys) =
    x == y && prefix xs ys
