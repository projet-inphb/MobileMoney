module Auth
    ( authentifier
    , creerIdentifiants
    ) where

import Types
import Chiffrement

-- ============================================================
-- CREATION D'IDENTIFIANTS
-- ============================================================

creerIdentifiants :: String -> String
creerIdentifiants motDePasseOriginal =
    chiffrer motDePasseOriginal

-- ============================================================
-- AUTHENTIFICATION
-- ============================================================

authentifier ::
    String ->
    String ->
    Compte ->
    Bool

authentifier numeroSaisi motDePasseSaisi compte =

    numeroSaisi == numero compte
    &&
    dechiffrer (motDePasse compte)
        == motDePasseSaisi
