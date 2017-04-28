

{-# LANGUAGE DataKinds, TypeOperators #-}

module GroupApi where

import Prelude ()
import Prelude.Compat

import Models

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson.Compat
import Data.ByteString (ByteString)
import Data.List
import Data.Maybe
import Data.String.Conversions
import GHC.Generics
import Servant

type GroupAPI =
  "groups" :> Get '[JSON] [Group] :<|>
  "groups" :> Capture "group_id" Integer :> Get '[JSON] Group

getGroups :: Handler [Group]
getGroups = return groups1

getGroupById :: Integer -> Handler Group
getGroupById i = case findGroup i groups1 of
  Just x -> return x
  Nothing -> throwError err404
