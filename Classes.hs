{-
Samantha Kacir
CS380 Final Project
-}

{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Classes where

import Data.Aeson.Compat
import GHC.Generics

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

data Contact where
  Phone :: String -> Contact
  Email :: String -> Contact
  deriving (Eq, Show, Generic)

instance ToJSON Contact

data NewContact = NewContact
  { modeOfContact :: Contact
  , estRespTime   :: Float }
  deriving (Eq, Show, Generic)

instance ToJSON NewContact

data User = User
  { name    :: String
  , contact :: [NewContact] --mode of contact, number/address, est resp time in hours
  } deriving (Eq, Show, Generic)

instance ToJSON User
