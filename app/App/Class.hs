{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App.Class (
  App(..)
, Message(..)
, EditorId(..)
) where

import           Data.Map.Strict (Map)

import           Edit            (Editor)
import qualified Edit            (Message)

newtype EditorId = EditorId { _unEditorId :: Int }
                 deriving (Enum, Eq, Ord, Show)


data App = App {
  appEditors    :: Map EditorId Editor
, appNextId     :: !EditorId
, appCurrentTab :: !EditorId
}

data Message = NewFile
             | OpenFile !FilePath
             | EditorMsg !EditorId !Edit.Message
             | RequestCloseEditor
             | CloseEditor !EditorId
             | Exit
             | Fun String
             deriving (Show)
