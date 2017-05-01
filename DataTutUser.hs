{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module DataTutUser where 

import Data.Text (Text)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateUser"] [persistLowerCase|
User
    name String
    email String
    deriving Show
|]
