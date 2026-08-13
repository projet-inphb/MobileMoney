module Menu
    ( startApplication
    ) where

import Types
import Compte
import Auth
import Etat
import ValidationEither
import Validation
import Pipeline
import Journal
import Utils
import Chiffrement
import Persistence
import Transfert
import Transactions

import Control.Monad.State (execState, runState)
import Text.Read (readMaybe)

-- ============================================================
-- État de l'application
-- ============================================================

data AppState = AppState
    { comptes       :: [Compte]
    , transactions  :: [Transaction]
    }

-- ============================================================
-- Initialisation
-- ============================================================

initialState :: AppState
initialState =
    AppState
        { comptes =
            [ creerCompte
                "0701234567"
                "Affoue"
                "1234"
                Wave

            , creerCompte
                "0509876543"
                "Kouakou"
                "5678"
                OrangeMoney
            ]

        , transactions = []
        }

-- ============================================================
-- Point d'entrée
-- ============================================================

startApplication :: IO ()
startApplication = do
    initialiserDonnees

    comptesCharges <- chargerComptes
    transactionsChargees <- chargerTransactions

    let stateInitial =
            if null comptesCharges
                then initialState
                else
                    AppState
                        { comptes = comptesCharges
                        , transactions = transactionsChargees
                        }

    -- Si aucun compte n'existe encore, sauvegarder les comptes de démonstration
    if null comptesCharges
        then sauvegarderComptes (comptes initialState)
        else return ()

    putStrLn ""
    putStrLn "=============================================="
    putStrLn "              SECUREMOMO"
    putStrLn "      Systeme securise Mobile Money"
    putStrLn "=============================================="
    putStrLn ""
    putStrLn "Deux comptes de demonstration sont disponibles."
    putStrLn ""
    putStrLn "Affoue : 0701234567 / 1234"
    putStrLn "Kouakou   : 0509876543 / 5678"
    putStrLn ""

    menuPrincipal stateInitial


-- ============================================================
-- Scenario automatique de demonstration
-- ============================================================

scenarioDemo :: AppState -> IO ()
scenarioDemo state = do
    putStrLn ""
    putStrLn "=============================================="
    putStrLn "       SCENARIO DE DEMONSTRATION"
    putStrLn "=============================================="

    putStrLn ""
    putStrLn "1. Deux comptes sont disponibles :"
    putStrLn "   Affoue : 0701234567"
    putStrLn "   Kouakou   : 0509876543"

    putStrLn ""
    putStrLn "2. Test d'un depot de 100000 FCFA."

    case rechercherCompte "0701234567" Wave (comptes state) of

        
        Nothing -> do
            putStrLn "Erreur : compte Affoue introuvable."
            pause
            menuPrincipal state

        Just affoue -> do
            let affoueApresDepot =
                    execState
                        (crediterCompte 100000)
                        affoue

            putStrLn
                ("Solde Affoue apres depot : "
                 ++ show (solde affoueApresDepot)
                 ++ " FCFA")

            putStrLn ""
            putStrLn "3. Test d'un retrait de 25000 FCFA."

            let (retraitOK, affoueApresRetrait) =
                    runState
                        (debiterCompte 25000)
                        affoueApresDepot

            if retraitOK
                then
                    putStrLn
                        ("Retrait accepte. Nouveau solde : "
                         ++ show (solde affoueApresRetrait)
                         ++ " FCFA")
                else
                    putStrLn "Retrait refuse."

            putStrLn ""
            putStrLn "4. Test d'un montant invalide."

            case verifierMontant (-5000) of
                Left erreur ->
                    putStrLn
                        ("Transaction refusee : "
                         ++ show erreur)

                Right montant ->
                    putStrLn
                        ("Montant accepte : "
                         ++ show montant)

            putStrLn ""
            putStrLn "5. Test du plafond BCEAO."

            case verifierPlafond (plafondBCEAO + 1) of
                Left erreur ->
                    putStrLn
                        ("Transaction refusee : "
                         ++ show erreur)

                Right montant ->
                    putStrLn
                        ("Montant accepte : "
                         ++ show montant)

            putStrLn ""
            putStrLn "6. Calcul avec le pipeline."

            let demoTransactions =
                    [ Transaction
                        { reference = "DEMO-001"
                        , expediteur = "SYSTEME"
                        , destinataire = "0701234567"
                        , montant = 100000
                        , typeTransaction = Depot
                        }

                    , Transaction
                        { reference = "DEMO-002"
                        , expediteur = "0701234567"
                        , destinataire = "SYSTEME"
                        , montant = 25000
                        , typeTransaction = Retrait
                        }

                    , Transaction
                        { reference = "DEMO-003"
                        , expediteur = "0701234567"
                        , destinataire = "0509876543"
                        , montant = 600000
                        , typeTransaction = Transfert
                        }
                    ]

            putStrLn
                ("Nombre total de transactions : "
                 ++ show (length demoTransactions))

            putStrLn
                ("Montant total : "
                 ++ show (calculerMontantTotal demoTransactions)
                 ++ " FCFA")

            putStrLn ""
            putStrLn "7. Detection des transactions suspectes."

            let suspectes =
                    transactionsSuspectes demoTransactions

            afficherTransactions suspectes

            putStrLn ""
            putStrLn "8. Demonstration de l'evaluation paresseuse."

            case referencesInfinies of
                premiereReference : deuxiemeReference : _ -> do
                    putStrLn
                        ("Premiere reference : "
                         ++ premiereReference)

                    putStrLn
                        ("Deuxieme reference : "
                         ++ deuxiemeReference)

                _ ->
                    putStrLn
                        "Impossible de generer les references."

            putStrLn ""
            putStrLn "9. Journalisation."

            ecrireJournal
                "logs/journal.txt"
                "Execution du scenario automatique"

            putStrLn
                "Journal mis a jour dans logs/journal.txt"

            putStrLn ""
            putStrLn "=============================================="
            putStrLn "     SCENARIO TERMINE AVEC SUCCES"
            putStrLn "=============================================="

            pause

            menuPrincipal state
-- ============================================================
-- Menu principal
-- ============================================================

menuPrincipal :: AppState -> IO ()
menuPrincipal state = do

    putStrLn ""
    putStrLn "============== MENU PRINCIPAL =============="
    putStrLn "1 - Se connecter"
    putStrLn "2 - Creer un compte"
    putStrLn "3 - Afficher les comptes"
    putStrLn "4 - Scenario automatique de demonstration"
    putStrLn "5 - Voir le journal d'activite"
    putStrLn "6 - Quitter"
    putStrLn "============================================"

    choix <- getLine

    case choix of

        "1" ->
            connexion state

        "2" ->
            creerCompteMenu state

        "3" -> do
            afficherComptes (comptes state)
            pause
            menuPrincipal state

        "4" ->
            scenarioDemo state

        "5" ->
            voirJournalMenu state

        "6" ->
            putStrLn "Merci d'avoir utilise SecureMoMo."

        _ -> do
            putStrLn "Choix invalide."
            menuPrincipal state

-- ============================================================
-- Creation de compte
--
-- Un numero peut avoir au plus 2 comptes, et uniquement selon
-- l'une de ces combinaisons : (Wave et Orange Money) ou
-- (Wave et MTN MoMo) — voir Validation.operateurCompatible.
-- On verifie cette compatibilite juste apres le choix de
-- l'operateur, avant de demander le reste (nom, mot de passe),
-- pour ne pas faire saisir des informations inutiles si la
-- combinaison est de toute facon impossible.
-- ============================================================

creerCompteMenu :: AppState -> IO ()
creerCompteMenu state = do

    putStrLn ""
    putStrLn "============ CREATION DE COMPTE ============="
    putStrLn "Numero de telephone (10 chiffres) :"

    numeroSaisi <- getLine

    if not (numeroValide numeroSaisi)
        then do
            putStrLn "Numero invalide : il doit contenir exactement 10 chiffres."
            pause
            menuPrincipal state

        else do

            let comptesExistants =
                    comptesDuNumero numeroSaisi (comptes state)

            if length comptesExistants >= 2
                then do
                    putStrLn "Ce numero possede deja 2 comptes (maximum atteint)."
                    pause
                    menuPrincipal state

                else do

                    operateurChoisi <- choisirOperateur

                    if not
                        (operateurCompatible
                            (map operateur comptesExistants)
                            operateurChoisi)

                        then do
                            putStrLn ""
                            putStrLn "Combinaison d'operateurs impossible sur ce numero."
                            putStrLn "Un numero ne peut avoir que : (Wave et Orange Money) ou (Wave et MTN MoMo)."
                            pause
                            menuPrincipal state

                        else do
                            putStrLn "Nom du proprietaire :"
                            nomSaisi <- getLine

                            motDePasseSaisi <- lireMotDePasseValide

                            let nouveauCompte =
                                    creerCompte
                                        numeroSaisi
                                        nomSaisi
                                        motDePasseSaisi
                                        operateurChoisi

                            let nouvelEtat =
                                    state
                                        { comptes =
                                            nouveauCompte : comptes state
                                        }

                            sauvegarderComptes (comptes nouvelEtat)

                            ecrireJournal
                                "logs/journal.txt"
                                ("Creation du compte "
                                 ++ nettoyerNumero numeroSaisi)

                            putStrLn ""
                            putStrLn "Compte cree avec succes !"
                            putStrLn ("Numero    : " ++ numeroSaisi)
                            putStrLn ("Operateur : " ++ show operateurChoisi)

                            pause

                            menuPrincipal nouvelEtat

-- ============================================================
-- Choix de l'operateur (petit sous-menu, reappele tant que
-- la saisie n'est pas valide)
-- ============================================================

choisirOperateur :: IO Operateur
choisirOperateur = do

    putStrLn "Operateur :"
    putStrLn "1 - Wave"
    putStrLn "2 - Orange Money"
    putStrLn "3 - MTN MoMo"

    choix <- getLine

    case choix of
        "1" -> return Wave
        "2" -> return OrangeMoney
        "3" -> return MTNMoMo

        _ -> do
            putStrLn "Choix invalide, reessayez."
            choisirOperateur

-- ============================================================
-- Consultation du journal d'activite (nouveau : utilise
-- lireJournal, qui existait dans Journal.hs mais n'etait
-- jamais appelee depuis le menu)
-- ============================================================

voirJournalMenu :: AppState -> IO ()
voirJournalMenu state = do

    putStrLn ""
    putStrLn "============ JOURNAL D'ACTIVITE =============="

    contenu <- lireJournal "logs/journal.txt"

    if null contenu
        then putStrLn "Le journal est vide pour le moment."
        else putStr contenu

    pause

    menuPrincipal state

-- ============================================================
-- Affichage des comptes
--
-- Comme un meme numero peut desormais avoir 2 comptes (deux
-- operateurs differents), chacun est affiche separement avec
-- son propre solde : c'est cette liste complete qui montre
-- "le solde des differents comptes rattaches a un numero".
-- ============================================================

afficherComptes :: [Compte] -> IO ()
afficherComptes [] =
    putStrLn "Aucun compte."

afficherComptes (c:cs) = do

    putStrLn ""
    putStrLn ("Numero       : " ++ numero c)
    putStrLn ("Proprietaire : " ++ proprietaire c)
    putStrLn ("Operateur    : " ++ show (operateur c))
    putStrLn ("Solde        : " ++ show (solde c))
    putStrLn "--------------------------------------------"

    afficherComptes cs

-- ============================================================
-- Authentification
--
-- Si le numero saisi correspond a 2 comptes (2 operateurs),
-- on demande d'abord lequel des deux avant le mot de passe
-- (choisirCompteParNumero).
-- ============================================================

connexion :: AppState -> IO ()
connexion state = do

    putStrLn ""
    putStrLn "=============== CONNEXION =================="
    putStrLn "Numero de telephone :"

    numeroSaisi <- getLine

    compteChoisi <- choisirCompteParNumero numeroSaisi (comptes state)

    case compteChoisi of

        Nothing -> do
            putStrLn "Compte introuvable."
            pause
            menuPrincipal state

        Just compte -> do

            putStrLn "Mot de passe :"
            motDePasse <- lireMotDePasse

            if authentifier numeroSaisi motDePasse compte

                then do
                    putStrLn "Authentification reussie."
                    menuCompte state numeroSaisi (operateur compte)

                else do
                    putStrLn "Authentification echouee."

                    ecrireJournal
                        "logs/journal.txt"
                        ("Tentative de connexion echouee pour "
                         ++ nettoyerNumero numeroSaisi)

                    pause
                    menuPrincipal state

-- ============================================================
-- Recherche récursive d'un compte
--
-- Un compte est identifie par la PAIRE (numero, operateur),
-- puisqu'un numero seul ne suffit plus a designer un compte
-- unique (il peut y en avoir 2).
-- ============================================================

rechercherCompte :: String -> Operateur -> [Compte] -> Maybe Compte
rechercherCompte _ _ [] =
    Nothing

rechercherCompte num op (c:cs)

    | numero c == num && operateur c == op =
        Just c

    | otherwise =
        rechercherCompte num op cs

-- ============================================================
-- Tous les comptes rattaches a un numero (0, 1 ou 2)
-- ============================================================

comptesDuNumero :: String -> [Compte] -> [Compte]
comptesDuNumero num =
    filter (\c -> numero c == num)

-- ============================================================
-- Choix d'un compte par numero : s'il n'y en a qu'un, il est
-- retourne directement ; s'il y en a deux, l'utilisateur choisit.
-- Reutilise a la fois pour la connexion et pour le destinataire
-- d'un transfert.
-- ============================================================

choisirCompteParNumero :: String -> [Compte] -> IO (Maybe Compte)
choisirCompteParNumero num tousLesComptes =

    case comptesDuNumero num tousLesComptes of

        [] ->
            return Nothing

        [unique] ->
            return (Just unique)

        plusieurs -> do
            putStrLn ""
            putStrLn "Plusieurs comptes existent pour ce numero, lequel choisir ?"
            afficherChoixComptes 1 plusieurs

            choix <- getLine

            case readMaybe choix >>= \i -> choisirParIndex i plusieurs of

                Just compte ->
                    return (Just compte)

                Nothing -> do
                    putStrLn "Choix invalide."
                    choisirCompteParNumero num tousLesComptes

-- Affiche une liste numerotee de comptes (pour choisirCompteParNumero)
afficherChoixComptes :: Int -> [Compte] -> IO ()
afficherChoixComptes _ [] =
    return ()

afficherChoixComptes n (c:cs) = do
    putStrLn
        (show n ++ " - " ++ show (operateur c)
         ++ " (solde : " ++ show (solde c) ++ " FCFA)")

    afficherChoixComptes (n + 1) cs

-- Recupere le compte a la position n (1 = premier) dans une liste
choisirParIndex :: Int -> [Compte] -> Maybe Compte
choisirParIndex _ [] =
    Nothing

choisirParIndex n (c:cs)
    | n == 1 =
        Just c

    | otherwise =
        choisirParIndex (n - 1) cs

-- Affiche un resume compact (operateur + solde) d'une liste de comptes
afficherComptesResume :: [Compte] -> IO ()
afficherComptesResume [] =
    return ()

afficherComptesResume (c:cs) = do
    putStrLn
        ("  - " ++ show (operateur c)
         ++ " : " ++ show (solde c) ++ " FCFA")

    afficherComptesResume cs

-- ============================================================
-- Recherche de l'index d'un compte (exemple de recursion,
-- non utilise ailleurs dans le menu)
-- ============================================================

indexCompte :: String -> [Compte] -> Maybe Int
indexCompte _ [] =
    Nothing

indexCompte num (c:cs)

    | numero c == num =
        Just 0

    | otherwise =
        case indexCompte num cs of
            Nothing -> Nothing
            Just i  -> Just (i + 1)

-- ============================================================
-- Mise à jour récursive
--
-- Comme rechercherCompte, on identifie desormais le compte a
-- remplacer par la paire (numero, operateur).
-- ============================================================

remplacerCompte :: Compte -> [Compte] -> [Compte]
remplacerCompte _ [] =
    []

remplacerCompte nouveau (c:cs)

    | numero nouveau == numero c && operateur nouveau == operateur c =
        nouveau : cs

    | otherwise =
        c : remplacerCompte nouveau cs

-- ============================================================
-- Menu compte
-- ============================================================

menuCompte :: AppState -> String -> Operateur -> IO ()
menuCompte state numeroConnecte operateurConnecte = do

    putStrLn ""
    putStrLn "=============== MENU COMPTE ================"
    putStrLn "1 - Consulter le solde"
    putStrLn "2 - Depot"
    putStrLn "3 - Retrait"
    putStrLn "4 - Transfert"
    putStrLn "5 - Historique"
    putStrLn "6 - Transactions suspectes"
    putStrLn "7 - Rechercher une transaction (par reference)"
    putStrLn "8 - Transactions superieures a un seuil"
    putStrLn "9 - Deconnexion"
    putStrLn "============================================"

    choix <- getLine

    case choix of

        "1" -> do
            consulterSoldeMenu state numeroConnecte operateurConnecte
            pause
            menuCompte state numeroConnecte operateurConnecte

        "2" ->
            depotMenu state numeroConnecte operateurConnecte

        "3" ->
            retraitMenu state numeroConnecte operateurConnecte

        "4" ->
            transfertMenu state numeroConnecte operateurConnecte

        "5" -> do
            afficherHistoriqueCompte state numeroConnecte
            pause
            menuCompte state numeroConnecte operateurConnecte

        "6" -> do
            afficherSuspectes state
            pause
            menuCompte state numeroConnecte operateurConnecte

        "7" ->
            rechercherTransactionMenu state numeroConnecte operateurConnecte

        "8" ->
            grossesTransactionsMenu state numeroConnecte operateurConnecte

        -- NOTE PEDAGOGIQUE : dans la version d'origine, cette branche
        -- se contentait d'un putStrLn et rendait la main : le
        -- programme entier se terminait au lieu de revenir au menu
        -- principal. On rappelle donc explicitement menuPrincipal.
        "9" -> do
            putStrLn "Deconnexion reussie."
            pause
            menuPrincipal state

        _ -> do
            putStrLn "Choix invalide."
            menuCompte state numeroConnecte operateurConnecte

-- ============================================================
-- Recherche d'une transaction par reference (nouveau : relie
-- au menu la fonction recursive rechercherTransaction du
-- module Transactions.hs, qui n'etait appelee nulle part)
-- ============================================================

rechercherTransactionMenu :: AppState -> String -> Operateur -> IO ()
rechercherTransactionMenu state numeroConnecte operateurConnecte = do

    putStrLn ""
    putStrLn "Reference de la transaction a rechercher (ex: TX-1234) :"

    ref <- getLine

    case rechercherTransaction ref (transactions state) of

        Nothing ->
            putStrLn "Aucune transaction trouvee avec cette reference."

        Just t -> do
            putStrLn ""
            putStrLn
                (reference t
                 ++ " | "
                 ++ show (typeTransaction t)
                 ++ " | "
                 ++ show (montant t)
                 ++ " FCFA")

            putStrLn
                ("   "
                 ++ expediteur t
                 ++ " -> "
                 ++ destinataire t)

    pause
    menuCompte state numeroConnecte operateurConnecte

-- ============================================================
-- Transactions superieures a un seuil choisi par l'utilisateur
-- (nouveau : relie grossesTransactions du module Transactions.hs
-- au menu ; l'utilisateur choisit lui-meme le seuil, ce qui rend
-- cette fonctionnalite reellement participative)
-- ============================================================

grossesTransactionsMenu :: AppState -> String -> Operateur -> IO ()
grossesTransactionsMenu state numeroConnecte operateurConnecte = do

    putStrLn ""
    putStrLn "Seuil (FCFA) : afficher les transactions superieures ou egales a :"

    seuil <- lireDouble

    let resultats =
            grossesTransactions seuil (transactions state)

    putStrLn ""
    putStrLn
        ("========== TRANSACTIONS >= "
         ++ show seuil
         ++ " FCFA ==========")

    afficherTransactions resultats

    pause
    menuCompte state numeroConnecte operateurConnecte

-- ============================================================
-- Consultation du solde
--
-- Affiche aussi les autres comptes rattaches au meme numero
-- (s'il y en a), avec leur propre solde.
-- ============================================================

consulterSoldeMenu :: AppState -> String -> Operateur -> IO ()
consulterSoldeMenu state num op =

    case rechercherCompte num op (comptes state) of

        Nothing ->
            putStrLn "Compte introuvable."

        Just compte -> do
            putStrLn ""
            putStrLn ("Proprietaire : " ++ proprietaire compte)
            putStrLn ("Operateur    : " ++ show (operateur compte))
            putStrLn ("Solde        : " ++ show (solde compte) ++ " FCFA")

            let autresComptes =
                    filter
                        (\c -> operateur c /= op)
                        (comptesDuNumero num (comptes state))

            if null autresComptes
                then return ()
                else do
                    putStrLn ""
                    putStrLn "Autre(s) compte(s) rattache(s) a ce numero :"
                    afficherComptesResume autresComptes

-- ============================================================
-- Dépôt
-- ============================================================

depotMenu :: AppState -> String -> Operateur -> IO ()
depotMenu state num op = do

    putStrLn ""
    putStrLn "Montant du depot :"

    montantDepot <- lireDouble

    case rechercherCompte num op (comptes state) of

        Nothing -> do
            putStrLn "Compte introuvable."
            pause
            menuCompte state num op

        Just compte ->

            case verifierMontant montantDepot of

                Left erreur -> do
                    putStrLn
                        ("Depot refuse : "
                         ++ show erreur)

                    ecrireJournal
                        "logs/journal.txt"
                        ("Depot invalide pour "
                         ++ nettoyerNumero num)

                    pause
                    menuCompte state num op

                Right montantValide' -> do

                    ref <- genererReference

                    let nouveauCompte =
                            execState
                                (crediterCompte montantValide')
                                compte

                    let nouvelleTransaction =
                            Transaction
                                { reference = ref
                                , expediteur = "SYSTEME"
                                , destinataire = num
                                , montant = montantValide'
                                , typeTransaction = Depot
                                }

                    let nouvelEtat =
                            state
                                { comptes =
                                    remplacerCompte
                                        nouveauCompte
                                        (comptes state)

                                , transactions =
                                    nouvelleTransaction
                                    : transactions state
                                }

                    putStrLn "Depot effectue avec succes."

                    putStrLn
                        ("Nouveau solde : "
                         ++ show (solde nouveauCompte)
                         ++ " FCFA")

                    sauvegarderTransaction nouvelleTransaction

                    sauvegarderComptes
                        (comptes nouvelEtat)

                    ecrireJournal
                        "logs/journal.txt"
                        ("Depot " ++ ref
                         ++ " : "
                         ++ nettoyerNumero num)

                    pause
                    menuCompte nouvelEtat num op

-- ============================================================
-- Retrait
-- ============================================================

retraitMenu :: AppState -> String -> Operateur -> IO ()
retraitMenu state num op = do

    putStrLn ""
    putStrLn "Montant du retrait :"

    montantRetrait <- lireDouble

    case rechercherCompte num op (comptes state) of

        Nothing -> do
            putStrLn "Compte introuvable."
            pause
            menuCompte state num op

        Just compte ->

            case verifierTransaction
                    compte
                    num
                    montantRetrait of

                Left erreur -> do

                    putStrLn
                        ("Retrait refuse : "
                         ++ show erreur)

                    ecrireJournal
                        "logs/journal.txt"
                        ("Retrait refuse : "
                         ++ nettoyerNumero num)

                    pause
                    menuCompte state num op

                Right montantValide' -> do

                    ref <- genererReference

                    let nouveauCompte =
                            execState
                                (debiterCompte montantValide')
                                compte

                    let transaction =
                            Transaction
                                { reference = ref
                                , expediteur = num
                                , destinataire = "SYSTEME"
                                , montant = montantValide'
                                , typeTransaction = Retrait
                                }

                    let nouvelEtat =
                            state
                                { comptes =
                                    remplacerCompte
                                        nouveauCompte
                                        (comptes state)

                                , transactions =
                                    transaction
                                    : transactions state
                                }

                    putStrLn "Retrait effectue avec succes."

                    putStrLn
                        ("Nouveau solde : "
                         ++ show (solde nouveauCompte)
                         ++ " FCFA")

                    sauvegarderTransaction transaction

                    sauvegarderComptes
                        (comptes nouvelEtat)

                    ecrireJournal
                        "logs/journal.txt"
                        ("Retrait " ++ ref
                         ++ " : "
                         ++ nettoyerNumero num)

                    pause
                    menuCompte nouvelEtat num op

-- ============================================================
-- Transfert
--
-- Deux regles verifiees AVANT de demander le montant :
--  1) le compte destinataire doit exister ;
--  2) il doit etre du MEME operateur que l'expediteur (Wave ->
--     Wave, jamais Wave -> Orange Money par exemple). On cherche
--     donc directement le compte destinataire avec l'operateur
--     de l'expediteur (rechercherCompte destinataireNum
--     expediteurOp ...) plutot que n'importe lequel de ses
--     comptes : s'il n'a pas de compte de ce meme operateur, le
--     transfert n'est de toute facon pas possible.
-- ============================================================

transfertMenu :: AppState -> String -> Operateur -> IO ()
transfertMenu state expediteurNum expediteurOp = do

    putStrLn ""
    putStrLn "Numero du destinataire :"

    destinataireNum <- getLine

    case rechercherCompte expediteurNum expediteurOp (comptes state) of

        Nothing -> do
            putStrLn "Compte expediteur introuvable."
            pause
            menuCompte state expediteurNum expediteurOp

        Just expediteur ->

            case rechercherCompte destinataireNum expediteurOp (comptes state) of

                Nothing -> do
                    putStrLn
                        ("Aucun compte " ++ show expediteurOp
                         ++ " trouve pour ce numero.")

                    putStrLn
                        "Les transferts ne sont possibles qu'entre comptes du meme operateur."

                    ecrireJournal
                        "logs/journal.txt"
                        "Transfert vers compte inexistant ou operateur incompatible"

                    pause
                    menuCompte state expediteurNum expediteurOp

                Just destinataire ->

                    if numero expediteur == numero destinataire

                        then do
                            putStrLn "Impossible de transferer vers le meme compte."
                            pause
                            menuCompte state expediteurNum expediteurOp

                        else do

                            putStrLn
                                ("Destinataire trouve : "
                                 ++ proprietaire destinataire
                                 ++ " ("
                                 ++ show (operateur destinataire)
                                 ++ ")")

                            putStrLn "Montant du transfert :"

                            montantTransfert <- lireDouble

                            case verifierTransaction
                                    expediteur
                                    expediteurNum
                                    montantTransfert of

                                Left erreur -> do

                                    putStrLn
                                        ("Transfert refuse : "
                                         ++ show erreur)

                                    ecrireJournal
                                        "logs/journal.txt"
                                        ("Transfert refuse pour "
                                         ++ nettoyerNumero expediteurNum)

                                    pause
                                    menuCompte state expediteurNum expediteurOp

                                Right montantValide' -> do

                                    -- Etape "participative" : on demande une
                                    -- confirmation avant d'executer un transfert,
                                    -- puisque c'est l'action la plus irreversible
                                    -- du menu.
                                    putStrLn ""
                                    putStrLn
                                        ("Confirmer le transfert de "
                                         ++ show montantValide'
                                         ++ " FCFA vers "
                                         ++ destinataireNum
                                         ++ " ? (o/n)")

                                    confirmation <- getLine

                                    if confirmation /= "o" && confirmation /= "O"
                                        then do
                                            putStrLn "Transfert annule."
                                            pause
                                            menuCompte state expediteurNum expediteurOp

                                        else do

                                            ref <- genererReference

                                            -- On reutilise la fonction pure
                                            -- effectuerTransfert (module Transfert)
                                            -- plutot que de recopier ici la logique
                                            -- "debiter puis crediter".
                                            let (nouvelExpediteur, nouveauDestinataire) =
                                                    effectuerTransfert
                                                        montantValide'
                                                        expediteur
                                                        destinataire

                                            let comptes1 =
                                                    remplacerCompte
                                                        nouvelExpediteur
                                                        (comptes state)

                                            let comptes2 =
                                                    remplacerCompte
                                                        nouveauDestinataire
                                                        comptes1

                                            let transaction =
                                                    Transaction
                                                        { reference = ref
                                                        , expediteur = expediteurNum
                                                        , destinataire = destinataireNum
                                                        , montant = montantValide'
                                                        , typeTransaction = Transfert
                                                        }

                                            let nouvelEtat =
                                                    state
                                                        { comptes = comptes2
                                                        , transactions =
                                                            transaction
                                                            : transactions state
                                                        }

                                            putStrLn ""
                                            putStrLn
                                                "Transfert effectue avec succes."

                                            putStrLn
                                                ("Reference : " ++ ref)

                                            putStrLn
                                                ("Montant : "
                                                 ++ show montantValide'
                                                 ++ " FCFA")

                                            sauvegarderTransaction transaction

                                            sauvegarderComptes
                                                (comptes nouvelEtat)

                                            ecrireJournal
                                                "logs/journal.txt"
                                                ("Transfert "
                                                 ++ ref
                                                 ++ " : "
                                                 ++ nettoyerNumero expediteurNum
                                                 ++ " -> "
                                                 ++ nettoyerNumero destinataireNum)

                                            pause
                                            menuCompte
                                                nouvelEtat
                                                expediteurNum
                                                expediteurOp

-- ============================================================
-- Historique
-- ============================================================

afficherHistoriqueCompte ::
    AppState ->
    String ->
    IO ()

afficherHistoriqueCompte state num = do

    putStrLn ""
    putStrLn "================ HISTORIQUE ==============="

    let historique =
            filter
                (\t ->
                    expediteur t == num
                    || destinataire t == num)
                (transactions state)

    afficherTransactions historique

    -- Utilisation des fonctions recursives de Transactions.hs
    -- (module "Recursivite" du plan de cours)
    putStrLn ""
    putStrLn
        ("Nombre de transactions : "
         ++ show (totalTransactions historique))

    putStrLn
        ("Montant total (calcul recursif) : "
         ++ show (sommeTransactions historique)
         ++ " FCFA")

-- ============================================================
-- Affichage récursif des transactions
-- ============================================================

afficherTransactions :: [Transaction] -> IO ()
afficherTransactions [] =
    putStrLn "Aucune transaction."

afficherTransactions (t:ts) = do

    putStrLn
        (reference t
         ++ " | "
         ++ show (typeTransaction t)
         ++ " | "
         ++ show (montant t)
         ++ " FCFA")

    putStrLn
        ("   "
         ++ expediteur t
         ++ " -> "
         ++ destinataire t)

    afficherTransactions ts

-- ============================================================
-- Transactions suspectes
-- ============================================================

afficherSuspectes :: AppState -> IO ()
afficherSuspectes state = do

    putStrLn ""
    putStrLn "========== TRANSACTIONS SUSPECTES ========="

    let suspectes =
            transactionsSuspectes
                (transactions state)

    afficherTransactions suspectes

-- ============================================================
-- Nettoyage des données destinées au journal
-- ============================================================

nettoyerNumero :: String -> String
nettoyerNumero =
    filter (`elem` ['0'..'9'])
