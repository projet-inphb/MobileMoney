module Journal
    ( ecrireJournal
    , lireJournal
    ) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (appendFile, readFile)

-- ============================================================
-- ECRITURE DANS LE JOURNAL
-- ============================================================

ecrireJournal :: FilePath -> String -> IO ()
ecrireJournal chemin message = do

    let dossier = takeDirectory chemin

    -- Création automatique du dossier parent
    if null dossier || dossier == "."
        then return ()
        else createDirectoryIfMissing True dossier

    -- Ajout de l'événement au journal
    appendFile chemin (message ++ "\n")

-- ============================================================
-- LECTURE DU JOURNAL
-- ============================================================

lireJournal :: FilePath -> IO String
lireJournal chemin =
    readFile chemin
