{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Edit (
  Editor
, Message
, View
, Edit.init
, update
, initView
, updateView
, finalize

, currentFilePath
, editorTitle
, isStale
) where

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Text as T hiding (null)
import Data.Text.IO as TIO
--import Control.Monad (when)
import System.FilePath
import System.IO.Error

import Mvc.Gtk

-- The Init state is necessary since the TextBuffer has to be constructed
-- within the IO monad.
data Editor = Init (Maybe FilePath)
  |  Editor {
      edtState :: EditorState
    , edtTextBuffer :: TextBuffer
    }

data EditorState = Unsaved { esIsStale :: Bool }
                 | OpenFile { esFilePath :: FilePath, esIsStale :: Bool }
                 | Opening { esFilePath :: FilePath }
                 | Saving { esFilePath :: FilePath, esWasStale :: Bool }
                 deriving (Eq, Show)

data Message = None
             | TextBufferCreated TextBuffer
             | FileOpened Text
             | Save
             | SaveAs FilePath
             | SavingSuccessful FilePath
             | MakeStale
             | GotError String
             deriving (Show)

instance Show TextBuffer where
  show = const "Gtk3TextBuffer"

init :: Maybe String -> (Editor, Cmd Message)

-- In the initial state we wait for the initView function to create the TextBuffer
init arg = Init arg ! none

update :: Message -> Editor -> (Editor, Cmd Message)

-- Save the empty buffer. The editor is not currently associated with a file
update (TextBufferCreated textBuffer) (Init Nothing) = Editor (Unsaved False) textBuffer ! none

-- Save the empty buffer and prepare to load a file.
update (TextBufferCreated textBuffer) (Init (Just path)) =
    Editor { edtState = Opening path, edtTextBuffer = textBuffer} ! open
  where open = single $ (flip catchIOError) handleIOError $ do
          text <- TIO.readFile path
          return (FileOpened text)

-- This should not happen
update TextBufferCreated{} editor@Editor{} = editor ! none

update (FileOpened text) editor@(edtState -> Opening path) =
    editor { edtState = OpenFile path False } ! setText
  where setText = single $ textBufferSetText (edtTextBuffer editor) text >> return None

update Save editor@(edtState -> Unsaved{}) = editor ! askForSaveFileName
  where askForSaveFileName = single $ postGUISync $ do
          fcDialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave actions
          responseId <- dialogRun fcDialog
          mFilename <- fileChooserGetFilename fcDialog
          if responseId == ResponseReject then return None
            else return $ maybe None SaveAs mFilename
        actions = [("Save", ResponseAccept), ("Cancel", ResponseReject)] :: [(Text, ResponseId)]

update Save editor@(edtState -> OpenFile path stale) =
  editor { edtState = Saving path stale } ! saveFile (edtTextBuffer editor) path

-- Do nothing. Either the editor is not initialized yet, or in progress loading or saving something.
update Save editor = editor ! none

update msg@(SaveAs path) editor@Editor{} = case edtState editor of
    Unsaved stale -> editor { edtState = Saving path stale } ! saveFile (edtTextBuffer editor) path
    -- The editor will possibly refer to another file
    OpenFile _ stale -> editor {edtState = Saving path stale } ! saveFile (edtTextBuffer editor) path
    -- When the editor is currently opening or closing a file we should postpone this message.
    _ -> editor ! postpone msg

update (SavingSuccessful path) editor@Editor{} =
  editor { edtState = OpenFile path False } ! none

update MakeStale editor@(isStale -> Just True) = case editor of
  Editor Unsaved{} _ -> editor { edtState = Unsaved False } ! none
  Editor (OpenFile path _) _ -> editor { edtState = OpenFile path True } ! none
  _ -> editor ! none

-- If there is an error saving at the destination it will be nevertheless seen as
-- the file the current editor is associated with. Subsequent savings will try
-- this path then. Several widespread editors behave similarly, for example, Atom.
update (GotError message) editor@(edtState -> Saving path wasStale) =
  editor { edtState = OpenFile path wasStale } ! showErrorMessageDialog message

-- If we fail to open a file we get an empty editor
update (GotError message) editor@(edtState -> Opening _) =
  editor { edtState = Unsaved False } ! showErrorMessageDialog message

update None editor = editor ! none

-- Ignore other messages
update msg editor = editor ! none

saveFile :: TextBuffer -> FilePath -> Cmd Message
saveFile textBuffer path = single $ (flip catchIOError) handleIOError $ do
    text <- get textBuffer textBufferText
    TIO.writeFile path text
    return (SavingSuccessful path)

handleIOError :: IOError -> IO Message
handleIOError = return . GotError . ioeGetErrorString

showErrorMessageDialog :: String -> Cmd Message
showErrorMessageDialog message = single $ do
  dialog <- messageDialogNew Nothing [] MessageError ButtonsOk message
  widgetShowAll dialog
  return None

type View = ScrolledWindow

-- Since the
initView :: Editor -> (Message -> IO ()) -> IO View
initView _editor sendMsg = do
  textBuffer <- textBufferNew Nothing
  textView <- textViewNewWithBuffer textBuffer
  sendMsg (TextBufferCreated textBuffer)

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  containerAdd scrolledWindow textView

  return scrolledWindow

updateView :: Editor -> (Message -> IO ()) -> View -> IO View
updateView _ _ = return

finalize :: Editor -> View -> IO ()
finalize _ _ = return ()

-- Functions useful for users of this module

currentFilePath :: Editor -> Maybe FilePath
currentFilePath (Init path) = path
currentFilePath (Editor Unsaved{} _) = Nothing
currentFilePath (Editor state _) = Just (esFilePath state)

editorTitle :: Editor -> String
editorTitle editor = plainTitle editor ++ stalenessIndicator editor
  where
    plainTitle = fromMaybe "Unnamed" . fmap takeFileName . Edit.currentFilePath
    stalenessIndicator :: Editor -> String
    stalenessIndicator = maybe "" ("" `bool` "*") . isStale

isStale :: Editor -> Maybe Bool
isStale Init{} = Nothing
isStale (edtState -> Unsaved staleness) = Just staleness
isStale (edtState -> OpenFile _ staleness) = Just staleness
isStale _ = Nothing

instance Show (Editor) where
  show (Init Nothing) = "Init Nothing"
  show (Init (Just path)) = "Init (Just " ++ show path ++ ")"
  show (Editor state _) = "Editor (" ++ show state ++ ")"
