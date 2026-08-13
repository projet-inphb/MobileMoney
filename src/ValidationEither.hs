module ValidationEither
(
    verifierMontant,
    verifierNumero,
    verifierPlafond,
    verifierSolde,
    verifierTransaction
)
where

import Types
import Validation

verifierMontant ::
    Double ->
    Either ValidationError Double

verifierMontant montant

    | montant <= 0 =
        Left MontantInvalide

    | otherwise =
        Right montant


verifierNumero ::
    String ->
    Either ValidationError String

verifierNumero numero

    | numeroValide numero =
        Right numero

    | otherwise =
        Left NumeroInvalide


verifierPlafond ::
    Double ->
    Either ValidationError Double

verifierPlafond montant

    | montant > plafondBCEAO =
        Left PlafondDepasse

    | otherwise =
        Right montant


verifierSolde ::
    Compte ->
    Double ->
    Either ValidationError Compte

verifierSolde compte montant

    | compteSuffisant compte montant =
        Right compte

    | otherwise =
        Left SoldeInsuffisant


verifierTransaction ::
    Compte ->
    String ->
    Double ->
    Either ValidationError Double

verifierTransaction compte numero montant = do

    _ <- verifierNumero numero

    _ <- verifierMontant montant

    _ <- verifierPlafond montant

    _ <- verifierSolde compte montant

    return montant
