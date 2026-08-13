module Validation
(
    plafondBCEAO,
    fraisTransaction,
    numeroValide,
    montantValide,
    compteSuffisant,
    motDePasseValide,
    operateurCompatible
)
where

import Types

-- Plafond BCEAO (modifiable facilement)
plafondBCEAO :: Double
plafondBCEAO = 500000

montantValide :: Double -> Bool
montantValide montant =
    montant > 0

numeroValide :: String -> Bool
numeroValide numero =
    length numero == 10 &&
    all (`elem` ['0'..'9']) numero

compteSuffisant :: Compte -> Double -> Bool
compteSuffisant compte montant =
    solde compte >= montant

-- Exemple de calcul de frais (1 %)
fraisTransaction :: Double -> Double
fraisTransaction montant =
    montant * 0.01

-- ============================================================
-- Mot de passe : au moins 4 caracteres
-- ============================================================

motDePasseValide :: String -> Bool
motDePasseValide motDePasse =
    length motDePasse >= 4

-- ============================================================
-- Compatibilite des operateurs sur un meme numero
-- ============================================================

operateurCompatible :: [Operateur] -> Operateur -> Bool
operateurCompatible operateursExistants nouveau
    | length operateursExistants >= 2 =
        False

    | nouveau `elem` operateursExistants =
        False

    | null operateursExistants =
        True

    | otherwise =
        Wave `elem` operateursExistants || nouveau == Wave
