module Pipeline
    ( pipelineTransactions
    , detecterFraude
    , calculerMontantTotal
    , transactionsSuspectes
    ) where

import Types
import Validation

-- Détection d'une transaction dépassant le plafond
detecterFraude :: Transaction -> Bool
detecterFraude transaction =
    montant transaction > plafondBCEAO

-- Pipeline :
-- 1. validation du montant
-- 2. filtrage des montants invalides
-- 3. détection des transactions suspectes

pipelineTransactions :: [Transaction] -> [Transaction]
pipelineTransactions transactions =
    filter
        (not . detecterFraude)
        (filter
            (montantValide . montant)
            transactions)

-- Somme des montants avec foldr
calculerMontantTotal :: [Transaction] -> Double
calculerMontantTotal =
    foldr
        (\transaction total ->
            montant transaction + total)
        0

-- Transactions suspectes
transactionsSuspectes :: [Transaction] -> [Transaction]
transactionsSuspectes =
    filter detecterFraude
