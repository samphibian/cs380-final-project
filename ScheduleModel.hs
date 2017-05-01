{-
Samantha Kacir
CS380 Final Project
Defines the schedule stuff
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
  { hour    :: Int
  , minutes :: Int
  }deriving (Eq, Show, Generic)

instance ToJSON Time
instance FromJSON Time

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
instance FromJSON Date

--ending day -> when it repeats (start day)
data Repeats where
  Daily   :: Day -> Repeats
  Weekly  :: Day -> [Date] -> Repeats
  Monthly :: Day -> [Int]  -> Repeats
  Yearly  :: Day -> [Day]  -> Repeats
  Once    :: Repeats
  deriving (Eq, Show, Generic)

instance ToJSON Repeats
instance FromJSON Repeats

data Schedule = Schedule
  { start_date        :: Day --Year month (Int) day
  , start_time        :: Time
  , end_date          :: Day
  , end_time          :: Time
  , repeats           :: Repeats
  , conflicting_event :: Conflict
  }deriving (Eq, Show, Generic)

instance ToJSON Schedule
instance FromJSON Schedule

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
bob  = fromGregorian 2017 3 15

--or at least probably of computers as we know them 
endOfTheWorld = fromGregorian 2121 2 1

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

richardSchedule = Schedule (fromGregorian 2017 5 1) (Time 12 0) (fromGregorian 2017 5 1) (Time 18 0) (Weekly endOfTheWorld [Friday]) haskellFinal
