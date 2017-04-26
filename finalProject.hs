{-
Samantha Kacir
CS380 Final Project
Basic server implementation
-}

{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Server where

import Prelude ()
import Prelude.Compat

import Models

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import Data.Time.Calendar
import GHC.Generics
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Directory
import System.IO
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html

type UserAPI1 = "users" :> Get '[JSON] [User]

richard = User "Richard Eisenberg" 
               "rae@cs.brynmawr.edu" 
               [Email (EmailAddress "rae@cs.brynmawr.edu") (EstRespTime 48.0) ]

users1 :: [User]
users1 = [ richard ]

sam = Person ("Samantha K")
jordan = Person ("Jordan H")
nora = Person ("Nora B")
charlie = Person ("Charles K")
stranger = Unknown

haskellClass = Group "cs380" richard [sam, jordan, nora]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app1 :: Servant.Application
app1 = serve userAPI server1

port :: Int
port = 8080

main :: IO ()
main = do
  putStr "Listening on Port "
  putStr (show port)
  hFlush stdout
  run port app1
