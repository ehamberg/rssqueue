module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Time
import Text.Julius (ToJavascript)

newtype Identifier = Identifier Text
  deriving (Read, Show, Eq, PathPiece, PersistField, ToJavascript)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
