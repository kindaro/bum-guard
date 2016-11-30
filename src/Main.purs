module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))

import Node.Encoding (Encoding(..))
import Node.HTTP (HTTP, listen, createServer, setHeader, requestMethod, requestURL, responseAsStream,           requestAsStream, setStatusCode)
import Node.HTTP.Client as Client
import Node.Stream (Writable, end, pipe, writeString)

import Partial.Unsafe (unsafeCrashWith)

import Control.Monad.Aff (launchAff)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax
import Data.FormURLEncoded (fromArray, encode)
import Control.Monad.Eff.Class (class MonadEff, liftEff)


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

main = void $ launchAff $ do

  res <- post requestUri requestBody
  liftEff $ log res.response

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

