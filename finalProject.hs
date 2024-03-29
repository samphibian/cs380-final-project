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

server1 :: Server UserAPI1
server1 =
  getUsers :<|>
  getUserById :<|>
  getGroupsByUser :<|>
  getGroupByMember :<|>
  getContactsByMember

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