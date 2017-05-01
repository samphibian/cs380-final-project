{-
Samantha Kacir
CS380 Final Project
Defining the models necessary for the api
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

import Data.Text hiding ( map, any )

data PhoneNumber = PhoneNumber { number :: String }
  deriving (Eq, Show, Generic)

instance ToJSON PhoneNumber
instance FromJSON PhoneNumber

data EmailAddress = EmailAddress { address :: String }
  deriving (Eq, Show, Generic)

instance ToJSON EmailAddress
instance FromJSON EmailAddress

data EstRespTime = EstRespTime { hours :: Float }
  deriving (Eq, Show, Generic)

instance ToJSON EstRespTime
instance FromJSON EstRespTime

data Conflict = Conflict 
  { busy_person      :: User      --person who has the conflict
  , contact_delayed  :: Float     --hours delayed
  , forms_of_contact :: [Contact] --which contact forms are delayed 
  , group_exceptions :: [Group]   --which groups do not have contact delayed
  } deriving (Eq, Show, Generic)

instance ToJSON Conflict
instance FromJSON Conflict

data Current where
  Free :: Current
  Busy :: Conflict -> Current
  deriving (Eq, Show, Generic)

instance ToJSON Current
instance FromJSON Current

data Contact where
  Phone :: PhoneNumber  -> EstRespTime -> Contact
  Email :: EmailAddress -> EstRespTime -> Contact
  deriving (Eq, Show, Generic)

instance ToJSON Contact
instance FromJSON Contact

data NewContact = NewContact
  { modeOfContact :: Contact
  , estRespTime   :: Float
  } deriving (Eq, Show, Generic)

instance ToJSON NewContact
instance FromJSON NewContact

data User = User
  { user_id   :: Integer
  , user_name :: String
  , email     :: String    --way for server to contact
  , contact   :: [Contact] --mode of contact, number/address, est resp time in hours
  } deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

data Member where
  Person  :: String -> Member
  Unknown :: Member
  deriving (Eq, Show, Generic)
  
instance ToJSON Member
instance FromJSON Member

data Group = Group
  { group_id   :: Integer
  , group_name :: String
  , owned_by   :: User
  , groupies   :: [Member]
  } deriving(Eq, Show, Generic)

instance ToJSON Group
instance FromJSON Group

--check if an element is in an array
isIn :: Eq a => a -> [a] -> Bool
isIn _ []     = False
isIn a (x : xs)
  | a == x    = True
  | otherwise = isIn a xs

findUser :: Integer -> [User] -> Maybe User
findUser i []        = Nothing
findUser i (x : xs)
  | i < 0            = Nothing
  | i == (user_id x) = Just x
  | otherwise        = findUser i xs

findGroup :: Integer -> [Group] -> Maybe Group
findGroup i []        = Nothing
findGroup i (x : xs)
  | i < 0             = Nothing
  | i == (group_id x) = Just x
  | otherwise         = findGroup i xs
  
findGroupsByUser :: Integer -> [Group] -> Maybe [Group]
findGroupsByUser i []       = Nothing
findGroupsByUser i (x : xs) = case i < 0 of
                                True   -> Nothing
                                False  -> case i == (user_id (owned_by x)) of
                                            True  -> case findGroupsByUser i xs of
                                                       Nothing -> Just (x : [])
                                                       Just y  -> Just (x : y)
                                            False -> findGroupsByUser i xs

findGroupByMember :: String -> [Group] -> Maybe Group
findGroupByMember "" _        = Nothing
findGroupByMember _  []       = Nothing
findGroupByMember t  (x : xs) = case isInGroup (Person t) x of
                                  True  -> Just x
                                  False -> findGroupByMember t xs
--check if a member is in a group
isInGroup :: Member -> Group -> Bool
isInGroup Unknown _ = False
isInGroup a       g = isIn a (groupies g)

--check if a user is busy
isBusy :: Current -> User -> Bool
isBusy Free     _        = False
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
makeDuple []       _        = []
makeDuple _        []       = []
makeDuple (x : xs) (y : ys) = (x, y) : makeDuple xs ys

newContactResponse :: Float -> Contact -> Contact
newContactResponse f c = case c of
                           Phone a o -> Phone a (EstRespTime (f + (hours o)))
                           Email a o -> Email a (EstRespTime (f + (hours o)))

--match contact methods with response times
getContactResponseTimes :: Member -> Current -> User -> [Contact]
getContactResponseTimes  p s u = case isBusy s u of
                           True -> case s of
                                     Busy a -> case any (isInGroup p) (group_exceptions a) of
                                                 False -> map (newContactResponse (contact_delayed a)) (contact u)
                                                 True  -> contact u
                           False -> contact u

getAllContactsByMember :: String -> User -> Current -> [Contact]
getAllContactsByMember "" u n = getContactResponseTimes Unknown    n u 
getAllContactsByMember m  u n = getContactResponseTimes (Person m) n u

--example models
richardContacts = [ Email (EmailAddress "rae@cs.brynmawr.edu") (EstRespTime 24.0)
                  , Email (EmailAddress "rae@brynmawr.edu") (EstRespTime 48.0) ]

richard = User 1
               "Richard Eisenberg" 
               "rae@cs.brynmawr.edu" 
               richardContacts

users1 :: [User]
users1 = [ richard ]

sam = Person ("Samantha K")
jordan = Person ("Jordan H")
nora = Person ("Nora B")
charlie = Person ("Charles K")
kid = Person ("Little E")
stranger = Unknown

haskellClass = Group 2 "cs380" richard [sam, jordan, nora]
family = Group 1 "family" richard [kid]

groups1 :: [Group]
groups1 = [ family, haskellClass ]

weekend = Conflict richard 48 richardContacts [ family ]
