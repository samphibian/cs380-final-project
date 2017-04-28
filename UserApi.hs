

{-# LANGUAGE DataKinds, TypeOperators #-}

module UserApi where

import Prelude ()
import Prelude.Compat

import Models

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.Text
import Data.String.Conversions
import GHC.Generics
import Servant
  
type UserAPI1 = 
  "users" :> Get '[JSON] [User] :<|>
  "users" :> Capture "user_id" Integer :> Get '[JSON] User :<|>
  "users" :> Capture "user_id" Integer :> "groups" :> Get '[JSON] [Group] :<|>
  "users" :> Capture "user_id" Integer :> Capture "member_name" String :> Get '[JSON] Group :<|>
  "users" :> Capture "user_id" Integer :> Capture "member_name" String :> "contacts" :> Get '[JSON] [Contact]

getUsers :: Handler [User]
getUsers = return users1

getUserById :: Integer -> Handler User
getUserById i = case findUser i users1 of
  Just x -> return x
  Nothing -> throwError err404

getGroupsByUser :: Integer -> Handler [Group]
getGroupsByUser i = case findGroupsByUser i groups1 of
  Just x -> return x
  Nothing -> throwError err404         

getGroupByMember :: Integer -> String -> Handler Group
getGroupByMember i m = case findGroupsByUser i groups1 of
  Nothing -> throwError err404
  Just x  -> case findGroupByMember m x of
               Nothing -> throwError err404
               Just x  -> return x 

getContactsByMember :: Integer -> String -> Handler [Contact]
getContactsByMember i m = case findUser i users1 of
                            Nothing -> throwError err404
                            Just x  -> return (getAllContactsByMember m x now)

now = Busy weekend