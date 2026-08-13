module Chiffrement
    ( chiffrer
    , dechiffrer
    ) where

-- ============================================================
-- CLE DE CHIFFREMENT
-- ============================================================

cle :: String
cle = "SecureMoMo2026"

-- ============================================================
-- TRANSFORMATION
-- ============================================================

transformation :: String -> String
transformation texte =
    zipWith appliquer texte (cycle cle)
  where
    appliquer :: Char -> Char -> Char
    appliquer caractere cleCaractere =
        toEnum
            ((fromEnum caractere + fromEnum cleCaractere) `mod` 256)

-- ============================================================
-- CHIFFREMENT
-- ============================================================

chiffrer :: String -> String
chiffrer =
    transformation

-- ============================================================
-- DECHIFFREMENT
-- ============================================================

dechiffrer :: String -> String
dechiffrer texte =
    zipWith retirer texte (cycle cle)
  where
    retirer :: Char -> Char -> Char
    retirer caractere cleCaractere =
        toEnum
            ((fromEnum caractere - fromEnum cleCaractere) `mod` 256)
