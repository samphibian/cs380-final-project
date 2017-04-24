{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}

import Data.Text (Text)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Tutorial
   title    Text
   url      Text
   school   Bool
   deriving Show
|]

main = runSqlite ":memory:" $ runMigration migrateTables
