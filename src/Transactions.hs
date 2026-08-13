module Transactions
(
    totalTransactions,
    sommeTransactions,
    grossesTransactions,
    rechercherTransaction
)
where

import Types

-- Nombre de transactions (récursif)
totalTransactions :: [Transaction] -> Int
totalTransactions [] = 0
totalTransactions (_:xs) =
    1 + totalTransactions xs


-- Somme totale des montants (récursif)
sommeTransactions :: [Transaction] -> Double
sommeTransactions [] = 0
sommeTransactions (t:ts) =
    montant t + sommeTransactions ts


-- Transactions supérieures à un seuil (récursif)
grossesTransactions ::
    Double ->
    [Transaction] ->
    [Transaction]

grossesTransactions _ [] = []

grossesTransactions seuil (t:ts)

    | montant t >= seuil =
        t : grossesTransactions seuil ts

    | otherwise =
        grossesTransactions seuil ts


-- Recherche d'une transaction par référence
rechercherTransaction ::
    String ->
    [Transaction] ->
    Maybe Transaction

rechercherTransaction _ [] = Nothing

rechercherTransaction ref (t:ts)

    | reference t == ref =
        Just t

    | otherwise =
        rechercherTransaction ref ts
