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

data Conflict = Conflict 
  { busy_person      :: User      --person who has the conflict
  , contact_delayed  :: Float     --hours delayed
  , forms_of_contact :: [Contact] --which contact forms are delayed 
  , group_exceptions :: [Group]   --which groups do not have contact delayed
  } deriving (Eq, Show, Generic)

instance ToJSON Conflict

data Current where
  Free :: Current
  Busy :: Conflict -> Current
  deriving (Eq, Show, Generic)

instance ToJSON Current

data Contact where
  Phone :: PhoneNumber  -> EstRespTime -> Contact
  Email :: EmailAddress -> EstRespTime -> Contact
  deriving (Eq, Show, Generic)

instance ToJSON Contact

data NewContact = NewContact
  { modeOfContact :: Contact
  , estRespTime   :: Float
  } deriving (Eq, Show, Generic)

instance ToJSON NewContact

data User = User
  { user_id   :: Integer
  , user_name :: String
  , email     :: String    --way for server to contact
  , contact   :: [Contact] --mode of contact, number/address, est resp time in hours
  } deriving (Eq, Show, Generic)

instance ToJSON User

data Member where
  Person  :: String -> Member
  Unknown :: Member
  deriving (Eq, Show, Generic)
instance ToJSON Member

data Group = Group
  { group_id   :: Integer
  , group_name :: String
  , owned_by   :: User
  , groupies   :: [Member]
  } deriving(Eq, Show, Generic)

instance ToJSON Group

--check if an element is in an array
isIn :: Eq a => a -> [a] -> Bool
isIn _ []     = False
isIn a (x : xs)
  | a == x    = True
  | otherwise = isIn a xs

--check if a member is in a group
isInGroup :: Member -> Group -> Bool
isInGroup Unknown _ = False
isInGroup a       g = isIn a (groupies g)

--check if a user is busy
isBusy :: Current -> User -> Bool
isBusy Free _   = False
isBusy (Busy a) u
  | (busy_person a) == u = True
  | otherwise            = False

--find the response time for a contact method
getResponseTime :: Contact -> Float
getResponseTime (Phone a t) = (hours t)
getResponseTime (Email a t) = (hours t)

--find how long each contact method will take to respond
getResponseTimes :: Member -> Current -> User -> [Float]
getResponseTimes p s u = case isBusy s u of
                           True -> case s of
                                     Busy a -> case any (isInGroup p) (group_exceptions a) of
                                                 False -> map (+ (contact_delayed a)) (map getResponseTime (contact u))
                                                 True  -> map getResponseTime (contact u)
                           False -> map getResponseTime (contact u)

--find the mode of contact
extractContacts :: [Contact] -> [(String, String)]
extractContacts []       = []
extractContacts (x : xs) = case x of
                             Phone a _ -> ("Phone: ", (number  a)) : extractContacts xs
                             Email a _ -> ("Email: ", (address a)) : extractContacts xs

--get all mode of contacts for a user
getContactMethods :: User -> [(String, String)]
getContactMethods u = case (contact u) of
                        x -> extractContacts x

--concat two lists
makeDuple :: [a] -> [b] -> [(a, b)]
makeDuple []       _ = []
makeDuple _        [] = []
makeDuple (x : xs) (y : ys) = (x, y) : makeDuple xs ys

--match contact methods with response times
getContactResponseTimes :: Member -> Current -> User -> [((String, String), Float)]
getContactResponseTimes m s u = makeDuple (getContactMethods u) (getResponseTimes m s u)
