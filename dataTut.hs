{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, GeneralizedNewtypeDeriving #-}

import Data.Text (Text)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

{-
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tutorial
   school   Bool
   deriving Show
|]
-}


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    deriving Show
|]
  
main = runSqlite ":memory:" $ runMigration migrateAll
