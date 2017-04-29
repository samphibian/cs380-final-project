{-
Samantha Kacir
CS380 Final Project
-}

{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TypeOperators #-}

module ScheduleModel where

import Data.Aeson.Compat
import GHC.Generics

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.Time

import Models

data Time = Time
  { hour   :: Int
  , minutes :: Int
  }deriving (Eq, Show, Generic)

instance ToJSON Time

data Date where
  Monday    :: Date
  Tuesday   :: Date
  Wednesday :: Date
  Thursday  :: Date
  Friday    :: Date
  Saturday  :: Date
  Sunday    :: Date
  deriving (Eq, Show, Generic)
instance ToJSON Date

data Month where
  January   :: Month
  February  :: Month
  March     :: Month
  April     :: Month
  May       :: Month
  June      :: Month
  July      :: Month
  August    :: Month
  September :: Month
  October   :: Month
  November  :: Month
  December  :: Month
  deriving (Eq, Show, Generic)
instance ToJSON Month

data FullDate = FullDate
  { day_of_the_week :: Date 
  , month           :: Month
  , day             :: Int
  , year            :: Int
  }deriving (Eq, Show, Generic)

instance ToJSON FullDate


data Schedule = Schedule
  { start_date :: Day --Year month (Int) day
  , start_time :: Time
  , end_date   :: Day
  , end_time   :: Time
  , conflicting_event :: Conflict
  }deriving (Eq, Show, Generic)

instance ToJSON Schedule

getDiffTime :: Time -> DiffTime
getDiffTime t = secondsToDiffTime (toInteger (3600*(hour t) + 60*(minutes t)))

getUserConflict :: [Schedule] -> User -> IO Current
getUserConflict []       _ = return Free
getUserConflict (x : xs) u = do
                               now <- getCurrentTime
                               case ((UTCTime (start_date x) (getDiffTime (start_time x))) <= now && (UTCTime (end_date x) (getDiffTime (end_time x))) >= now) of
                                 False -> getUserConflict xs u
                                 True  -> case (busy_person (conflicting_event x)) == u of
                                           True  -> return (Busy (conflicting_event x))
                                           False -> getUserConflict xs u

mary = fromGregorian 2015 2 1
bob = fromGregorian 2017 3 15

hereandnow :: IO()
hereandnow = do
  fred <- getCurrentTime
  doug <- getTimeZone fred
  let barb = utctDay fred
  let rich = utcToLocalTimeOfDay doug (timeToTimeOfDay (utctDayTime fred))
  print fred
  print doug
  print barb
  print rich

richardSchedule = Schedule (fromGregorian 2017 3 28) (Time 16 0) (fromGregorian 2017 5 1) (Time 14 0) weekend