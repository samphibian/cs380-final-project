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
import UserApi
import GroupApi

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

{-
server1 :: Server UserAPI1
server1 =
  getUsers :<|>
  getUserById :<|>
  getGroupsByUser

server2 :: Server GroupAPI1
server2 =
  getGroups :<|>
  getGroupById
-}
server1 :: Server ModelAPI1
server1 = 
  getModels :<|>
  getModelById :<|>
  getModels :<|>
  getModelById

userAPI :: Proxy UserAPI1
userAPI = Proxy

groupAPI :: Proxy GroupAPI1
groupAPI = Proxy

modelAPI :: Proxy ModelAPI1
modelAPI = Proxy

-- 'serve' comes from servant and hands you a WAI Application,
-- which you can think of as an "abstract" web application,
-- not yet a webserver.
app :: Servant.Application
app = serve modelAPI server1

{-
app1 :: Servant.Application
app1 = serve userAPI server1

app2 :: Servant.Application
app2 = serve groupAPI server2
-}

port :: Int
port = 8080

main :: IO ()
main = do
  putStr "Listening on Port "
  putStr (show port)
  hFlush stdout
  run port app
  --run port app1
  --run port app2


type ModelAPI1 =
  "users" :> Get '[JSON] [Model] :<|>
  "users" :> Capture "user_id" Integer :> Get '[JSON] Model :<|>
  "groups" :> Get '[JSON] [Model] :<|>
  "groups" :> Capture "group_id" Integer :> Get '[JSON] Model

getModels :: String -> Handler [Model]
getModels s 
  | s == "users"  = return (modelizeUsers users1)
  | s == "groups" = return (modelizeGroups groups1)

getModelById :: String -> Integer -> Handler Model
getModelById s i = case s of
                    "users"  -> case findUser i users1 of
                                  Just x  -> return (UserModel x)
                                  Nothing -> throwError err404
                    "groups" -> case findGroup i groups1 of
                                  Just x  -> return (GroupModel x)
                                  Nothing -> throwError err404