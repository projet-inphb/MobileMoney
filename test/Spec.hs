module Main where

import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

import Types
import Validation

-- ============================================================
-- Un compte de test, reutilise dans plusieurs cas
-- ============================================================

compteTest :: Compte
compteTest =
    Compte
        { numero = "0701234567"
        , proprietaire = "Test"
        , motDePasse = "xxxx"
        , operateur = Wave
        , solde = 10000
        }

-- ============================================================
-- Tests de montantValide
-- ============================================================

testMontantPositif :: Test
testMontantPositif =
    TestCase
        (assertEqual
            "un montant positif doit etre valide"
            True
            (montantValide 100))

testMontantNul :: Test
testMontantNul =
    TestCase
        (assertEqual
            "un montant nul doit etre invalide"
            False
            (montantValide 0))

testMontantNegatif :: Test
testMontantNegatif =
    TestCase
        (assertEqual
            "un montant negatif doit etre invalide"
            False
            (montantValide (-500)))

-- ============================================================
-- Tests de numeroValide
-- ============================================================

testNumeroCorrect :: Test
testNumeroCorrect =
    TestCase
        (assertEqual
            "un numero de 10 chiffres doit etre valide"
            True
            (numeroValide "0701234567"))

testNumeroTropCourt :: Test
testNumeroTropCourt =
    TestCase
        (assertEqual
            "un numero trop court doit etre invalide"
            False
            (numeroValide "070123"))

testNumeroAvecLettres :: Test
testNumeroAvecLettres =
    TestCase
        (assertEqual
            "un numero contenant des lettres doit etre invalide"
            False
            (numeroValide "07012345AB"))

-- ============================================================
-- Tests de compteSuffisant
-- ============================================================

testSoldeSuffisant :: Test
testSoldeSuffisant =
    TestCase
        (assertEqual
            "solde suffisant pour un retrait inferieur au solde"
            True
            (compteSuffisant compteTest 5000))

testSoldeInsuffisant :: Test
testSoldeInsuffisant =
    TestCase
        (assertEqual
            "solde insuffisant pour un retrait superieur au solde"
            False
            (compteSuffisant compteTest 20000))

-- ============================================================
-- Tests de fraisTransaction
-- ============================================================

testFraisUnPourcent :: Test
testFraisUnPourcent =
    TestCase
        (assertEqual
            "les frais representent 1% du montant"
            1000.0
            (fraisTransaction 100000))

-- ============================================================
-- Tests de motDePasseValide
-- ============================================================

testMotDePasseValide :: Test
testMotDePasseValide =
    TestCase
        (assertEqual
            "un mot de passe de 4 caracteres ou plus doit etre valide"
            True
            (motDePasseValide "1234"))

testMotDePasseTropCourt :: Test
testMotDePasseTropCourt =
    TestCase
        (assertEqual
            "un mot de passe de moins de 4 caracteres doit etre invalide"
            False
            (motDePasseValide "123"))

testMotDePasseVide :: Test
testMotDePasseVide =
    TestCase
        (assertEqual
            "un mot de passe vide doit etre invalide"
            False
            (motDePasseValide ""))

-- ============================================================
-- Tests de operateurCompatible
-- (regle : un numero a au plus 2 comptes, uniquement
-- (Wave et Orange Money) ou (Wave et MTN MoMo))
-- ============================================================

testPremierCompteToujoursAutorise :: Test
testPremierCompteToujoursAutorise =
    TestCase
        (assertEqual
            "le premier compte d'un numero est toujours autorise"
            True
            (operateurCompatible [] MTNMoMo))

testWaveEtOrangeAutorise :: Test
testWaveEtOrangeAutorise =
    TestCase
        (assertEqual
            "Wave puis Orange Money doit etre autorise"
            True
            (operateurCompatible [Wave] OrangeMoney))

testWaveEtMTNAutorise :: Test
testWaveEtMTNAutorise =
    TestCase
        (assertEqual
            "Orange Money puis Wave doit etre autorise"
            True
            (operateurCompatible [OrangeMoney] Wave))

testOrangeEtMTNRefuse :: Test
testOrangeEtMTNRefuse =
    TestCase
        (assertEqual
            "Orange Money puis MTN MoMo doit etre refuse"
            False
            (operateurCompatible [OrangeMoney] MTNMoMo))

testMemeOperateurRefuse :: Test
testMemeOperateurRefuse =
    TestCase
        (assertEqual
            "deux comptes Wave sur le meme numero doit etre refuse"
            False
            (operateurCompatible [Wave] Wave))

testTroisiemeCompteRefuse :: Test
testTroisiemeCompteRefuse =
    TestCase
        (assertEqual
            "un troisieme compte sur le meme numero doit etre refuse"
            False
            (operateurCompatible [Wave, OrangeMoney] MTNMoMo))

-- ============================================================
-- Suite complete
-- ============================================================

tousLesTests :: Test
tousLesTests =
    TestList
        [ TestLabel "montantValide - montant positif"      testMontantPositif
        , TestLabel "montantValide - montant nul"           testMontantNul
        , TestLabel "montantValide - montant negatif"       testMontantNegatif
        , TestLabel "numeroValide - numero correct"         testNumeroCorrect
        , TestLabel "numeroValide - numero trop court"      testNumeroTropCourt
        , TestLabel "numeroValide - numero avec lettres"    testNumeroAvecLettres
        , TestLabel "compteSuffisant - solde suffisant"     testSoldeSuffisant
        , TestLabel "compteSuffisant - solde insuffisant"   testSoldeInsuffisant
        , TestLabel "fraisTransaction - un pourcent"        testFraisUnPourcent
        , TestLabel "motDePasseValide - valide"             testMotDePasseValide
        , TestLabel "motDePasseValide - trop court"         testMotDePasseTropCourt
        , TestLabel "motDePasseValide - vide"               testMotDePasseVide
        , TestLabel "operateurCompatible - premier compte"  testPremierCompteToujoursAutorise
        , TestLabel "operateurCompatible - Wave+Orange"     testWaveEtOrangeAutorise
        , TestLabel "operateurCompatible - Orange+Wave"     testWaveEtMTNAutorise
        , TestLabel "operateurCompatible - Orange+MTN refuse" testOrangeEtMTNRefuse
        , TestLabel "operateurCompatible - meme operateur"  testMemeOperateurRefuse
        , TestLabel "operateurCompatible - troisieme compte" testTroisiemeCompteRefuse
        ]

-- ============================================================
-- Point d'entree du test-suite
-- ============================================================

main :: IO ()
main = do
    resultats <- runTestTT tousLesTests

    if errors resultats + failures resultats > 0
        then exitFailure
        else exitSuccess
