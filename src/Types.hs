module Types where

-- Operateurs Mobile Money
data Operateur
    = Wave
    | OrangeMoney
    | MTNMoMo
    deriving (Show, Read, Eq)

-- Compte Mobile Money
data Compte = Compte
    { numero       :: String
    , proprietaire :: String
    , motDePasse   :: String
    , operateur    :: Operateur
    , solde        :: Double
    } deriving (Show, Read, Eq)

-- Types de transactions
data TransactionType
    = Depot
    | Retrait
    | Transfert
    deriving (Show, Read, Eq)

-- Transaction
data Transaction = Transaction
    { reference       :: String
    , expediteur      :: String
    , destinataire    :: String
    , montant         :: Double
    , typeTransaction :: TransactionType
    } deriving (Show, Read, Eq)

-- Erreurs de validation
data ValidationError
    = MontantInvalide
    | SoldeInsuffisant
    | PlafondDepasse
    | NumeroInvalide
    | AuthentificationEchouee
    deriving (Show, Read, Eq)
