module Main where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (log)
import Data.FormURLEncoded (fromArray, encode)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax


requestBody = fromArray
                  [ Tuple "al" (Just $ show 1)
                  , Tuple "c[city]" (Just $ show 1965294)
                  , Tuple "c[country]" (Just $ show 7)
                  , Tuple "c[group]" (Just $ show 54566161)
                  , Tuple "c[name]" (Just $ show 1)
                  , Tuple "c[photo]" (Just $ show 1)
                  , Tuple "c[section]" (Just "people")
                  , Tuple "offset" (Just $ show 60)
                  ]


requestUri = "https://vk.com/al_search.php"

main = runAff (const $ pure unit) (cb) (post requestUri requestBody :: forall e. Affjax e String)

cb = (\x -> x.response) >>> show >>> log

-- curl 'https://vk.com/al_search.php'
--     --data
--         al=1
--         &c[city]=1965294
--         &c[country]=7
--         &c[group]=54566161
--         &c[name]=1
--         &c[photo]=1
--         &c[section]=people
--         &offset=60

