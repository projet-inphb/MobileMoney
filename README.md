# SecureMoMo

Application en ligne de commande simulant un système Mobile Money sécurisé (dépôt, retrait, transfert, authentification, journalisation), écrite en Haskell dans un but pédagogique.

## Installation

Le projet a besoin de GHC (le compilateur Haskell) et de Cabal (l'outil de compilation). La façon recommandée d'installer les deux est [GHCup](https://www.haskell.org/ghcup/) :

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Suivre les instructions affichées, puis vérifier l'installation :

```bash
ghc --version
cabal --version
```

Récupérer ensuite le projet (clone du dépôt Git ou simple copie du dossier), puis se placer à sa racine (le dossier contenant `securemomo.cabal`).

## Compilation

```bash
cabal build
```

Cette commande télécharge les dépendances (`directory`, `filepath`, `mtl`, `time`, `HUnit`) et compile le projet. La première exécution peut prendre quelques minutes.

## Exécution

```bash
cabal run securemomo
```

Deux comptes de démonstration sont créés automatiquement au premier lancement :

| Propriétaire | Numéro     | Mot de passe |
|---|---|---|
| Alice | 0701234567 | 1234 |
| Bob   | 0509876543 | 5678 |

Le menu principal permet de se connecter avec l'un de ces comptes, d'en créer un nouveau, de consulter la liste des comptes, ou de consulter le journal d'activité.

### Lancer le scénario de démonstration

Depuis le menu principal, choisir l'option **4 - Scenario automatique de demonstration**. Ce scénario s'exécute seul (aucune saisie requise) et illustre, dans l'ordre : un dépôt, un retrait, une validation de montant invalide, une validation de plafond dépassé, un calcul de pipeline (`map`/`filter`/`fold`), une détection de transactions suspectes, l'évaluation paresseuse d'une liste infinie de références, et une écriture dans le journal.

## Tests

```bash
cabal test
```

Exécute la suite de tests unitaires (`test/Spec.hs`, avec HUnit) sur les fonctions pures du module `Validation.hs` : validité d'un montant, validité d'un numéro de téléphone, suffisance d'un solde, calcul des frais.

## Structure du projet

```
securemomo.cabal   -- configuration du build (library / executable / test-suite)
app/
  Main.hs           -- point d'entree (main), appelle CLI.startApplication
src/
  CLI.hs            -- menu interactif
  Types.hs          -- types de donnees (Compte, Transaction, ...)
  Validation.hs      -- validations pures (montant, numero, plafond)
  ValidationEither.hs-- validations via Either et types d'erreur dedies
  Compte.hs         -- creation/credit/debit d'un compte (immuable)
  Etat.hs           -- credit/debit via Control.Monad.State
  Transactions.hs   -- fonctions recursives (total, somme, filtrage, recherche)
  Pipeline.hs       -- traitement par map/filter/fold, detection de fraude
  Transfert.hs      -- transfert entre deux comptes
  Auth.hs           -- authentification
  Chiffrement.hs     -- chiffrement/dechiffrement du mot de passe
  Journal.hs        -- journalisation des evenements
  Persistence.hs     -- sauvegarde/chargement sur disque (data/)
  Utils.hs          -- saisie utilisateur, generation de reference, pause
test/
  Spec.hs           -- tests unitaires (HUnit) sur Validation.hs
data/               -- cree automatiquement : comptes.txt, transactions.txt
logs/               -- cree automatiquement : journal.txt
```

## Limites connues

Le chiffrement du mot de passe (`Chiffrement.hs`) est un chiffre additif simple, à but pédagogique uniquement — il ne doit pas être considéré comme une sécurité réelle. La persistance sur disque (`Persistence.hs`) sérialise les champs séparés par `|` ; un caractère `|` dans une donnée pourrait, en théorie, corrompre la relecture.

## Lien executable

C:\SecureMoMo-4\dist-newstyle\build\x86_64-windows\ghc-9.10.3\securemomo-0.1.0.0\x\securemomo\build\securemomo
