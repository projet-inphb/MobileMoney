module Transfert
(
    effectuerTransfert
)
where

import Types
import Compte

-- Fonction pure qui applique un transfert entre deux comptes.
-- Elle est utilisee par CLI.hs pour eviter de dupliquer la logique
-- "debiter puis crediter" directement dans le menu.
effectuerTransfert ::

    Double ->

    Compte ->

    Compte ->

    (Compte,Compte)

effectuerTransfert montant expediteur destinataire =

    ( debiter montant expediteur
    , crediter montant destinataire
    )
