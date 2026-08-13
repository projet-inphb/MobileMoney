module Compte
    ( creerCompte
    , crediter
    , debiter
    ) where

import Types
import Chiffrement

-- ============================================================
-- CREATION D'UN COMPTE
-- ============================================================

creerCompte ::
    String ->
    String ->
    String ->
    Operateur ->
    Compte

creerCompte numeroCompte nom motDePasseCompte operateurCompte =

    Compte
        { numero = numeroCompte
        , proprietaire = nom
        , motDePasse = chiffrer motDePasseCompte
        , operateur = operateurCompte
        , solde = 0
        }

-- ============================================================
-- CREDIT
-- ============================================================

crediter :: Double -> Compte -> Compte
crediter montant compte =

    compte
        { solde = solde compte + montant
        }

-- ============================================================
-- DEBIT
-- ============================================================

debiter :: Double -> Compte -> Compte
debiter montant compte =

    compte
        { solde = solde compte - montant
        }
