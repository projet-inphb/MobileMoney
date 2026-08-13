module Etat
    ( crediterCompte
    , debiterCompte
    ) where

import Types
import Control.Monad.State

-- ============================================================
-- CREDIT D'UN COMPTE
-- ============================================================

crediterCompte :: Double -> State Compte ()
crediterCompte montant = do

    compteActuel <- get

    let nouveauSolde =
            solde compteActuel + montant

    put
        compteActuel
            { solde = nouveauSolde
            }

-- ============================================================
-- DEBIT D'UN COMPTE
-- ============================================================

debiterCompte :: Double -> State Compte Bool
debiterCompte montant = do

    compteActuel <- get

    if montant <= solde compteActuel

        then do

            let nouveauSolde =
                    solde compteActuel - montant

            put
                compteActuel
                    { solde = nouveauSolde
                    }

            return True

        else do

            return False
