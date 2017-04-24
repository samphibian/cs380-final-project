{-
Samantha Kacir
CS380 Final Project
-}

{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module Models where

import Data.Aeson.Compat
import GHC.Generics

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

data PhoneNumber = PhoneNumber { number :: String }
  deriving (Eq, Show, Generic)

instance ToJSON PhoneNumber

data EmailAddress = EmailAddress { address :: String }
  deriving (Eq, Show, Generic)

instance ToJSON EmailAddress

data EstRespTime = EstRespTime { hours :: Float }
  deriving (Eq, Show, Generic)

instance ToJSON EstRespTime

data Contact where
  Phone :: PhoneNumber -> EstRespTime -> Contact
  Email :: EmailAddress -> EstRespTime -> Contact
  deriving (Eq, Show, Generic)

instance ToJSON Contact

data NewContact = NewContact
  { modeOfContact :: Contact
  , estRespTime   :: Float }
  deriving (Eq, Show, Generic)

instance ToJSON NewContact

data User = User
  { name    :: String
  , email   :: String
  , contact :: [Contact] --mode of contact, number/address, est resp time in hours
  } deriving (Eq, Show, Generic)

instance ToJSON User
