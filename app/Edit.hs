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
) where

import Data.Maybe (fromJust, isNothing)
import Data.Text as T hiding (null)
import Data.Text.IO as TIO
--import Control.Monad (when)
import System.IO.Error

import Mvc.Gtk

-- The Init state is necessary since the TextBuffer has to be constructed
-- within the IO monad.
data Editor = Init (Maybe FilePath)
  |  Editor {
      edtState :: EditorState
    , edtTextBuffer :: TextBuffer
    }

data EditorState = Unsaved
                 | OpenFile FilePath
                 | Opening FilePath
                 | Saving FilePath

data Message = None
             | TextBufferCreated TextBuffer
             | FileOpened Text
             | Save
             | SaveAs FilePath
             | SavingSuccessful FilePath
             | GotError String
             deriving (Show)

instance Show TextBuffer where
  show = const "<<TextBuffer>>"

init :: Maybe String -> (Editor, Cmd Message)

-- In the initial state we wait for the initView function to create the TextBuffer
init arg = Init arg ! none

update :: Message -> Editor -> (Editor, Cmd Message)

-- Save the empty buffer. The editor is not currently associated with a file
update (TextBufferCreated textBuffer) (Init Nothing) = Editor Unsaved textBuffer ! none

-- Save the empty buffer and prepare to load a file.
update (TextBufferCreated textBuffer) (Init (Just path)) =
    Editor { edtState = Opening path, edtTextBuffer = textBuffer} ! open
  where open = single $ (flip catchIOError) handleIOError $ do
          text <- TIO.readFile path
          return (FileOpened text)

-- This should not happen
update TextBufferCreated{} editor@Editor{} = editor ! none

update (FileOpened text) editor@(edtState -> Opening path) =
    editor { edtState = OpenFile path } ! setText
  where setText = single $ textBufferSetText (edtTextBuffer editor) text >> return None

update Save editor@(edtState -> Unsaved) = editor ! askForSaveFileName
  where askForSaveFileName = single $ postGUISync $ do
          fcDialog <- fileChooserDialogNew Nothing Nothing FileChooserActionSave actions
          responseId <- dialogRun fcDialog
          filename <- fileChooserGetFilename fcDialog
          if isNothing filename || responseId == ResponseReject
            then return None
            else return (SaveAs (fromJust filename))
        actions = [("Save", ResponseAccept), ("Cancel", ResponseReject)] :: [(Text, ResponseId)]

update Save editor@(edtState -> OpenFile path) =
  editor { edtState = Saving path} ! saveFile (edtTextBuffer editor) path

-- Do nothing. Either the editor is not initialized yet, or in progress loading or saving something.
update Save editor = editor ! none

update msg@(SaveAs path) editor@Editor{} = case edtState editor of
    Unsaved -> editor { edtState = Saving path } ! saveFile (edtTextBuffer editor) path
    -- The editor will possibly refer to another file
    OpenFile _ -> editor {edtState = Saving path } ! saveFile (edtTextBuffer editor) path
    -- When the editor is currently opening or closing a file we should postpone this message.
    _ -> editor ! postpone msg

update (SavingSuccessful path) editor@Editor{} =
  editor { edtState = OpenFile path } ! none

-- If there is an error saving at the destination it will be nevertheless seen as
-- the file the current editor is associated with. Subsequent savings will try
-- this path then. Several widespread editors behave similarly, for example, Atom.
update (GotError message) editor@(edtState -> Saving path) =
  editor { edtState = OpenFile path } ! showErrorMessageDialog message

-- If we fail to open a file we get an empty editor
update (GotError message) editor@(edtState -> Opening _) =
  editor { edtState = Unsaved } ! showErrorMessageDialog message

update None editor = editor ! none

-- Ignore other messages
update msg editor = editor ! single ((Prelude.putStrLn . show $ msg) >> return None)

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

type View = TextView

-- Since the
initView :: Editor -> (Message -> IO ()) -> IO View
initView _editor sendMsg = do
  textBuffer <- textBufferNew Nothing
  textView <- textViewNewWithBuffer textBuffer
  sendMsg (TextBufferCreated textBuffer)
  return textView

updateView :: Editor -> (Message -> IO ()) -> View -> IO View
updateView _ _ = return

finalize :: Editor -> View -> IO ()
finalize _ _ = return ()
