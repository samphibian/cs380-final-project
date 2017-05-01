{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, GeneralizedNewtypeDeriving #-}

import Data.Text (Text)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import DataTutUser
  
share [mkPersist sqlSettings, mkMigrate "migrateGroup"] [persistLowerCase|
Group
    name String
    user UserId
    deriving Show
|]

main = runSqlite ":memory:" $ runMigration migrateGroup
