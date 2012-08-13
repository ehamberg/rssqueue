module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time
import Data.Int (Int64)

newtype Identifier = Identifier Text
  deriving (Read, Show, Eq, PathPiece, PersistField)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
