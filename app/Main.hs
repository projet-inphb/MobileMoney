module Main where

import Menu (startApplication)

-- Le veritable point d'entree du programme.
-- C'est la SEULE fonction que GHC execute automatiquement au
-- demarrage : elle doit s'appeler "main", avoir le type IO (),
-- et se trouver dans un module nomme "Main". Tout le reste du
-- travail est delegue a Menu.startApplication.
main :: IO ()
main = startApplication
