{-# LANGUAGE OverloadedStrings #-}
module App (
  App
, App.init
, update
, initView
, updateView
, finalize
) where

import Edit (Editor)
import qualified Edit as Edit

import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

import Mvc.Gtk

type EditorId = Int

data App = App {
  appEditorTabs :: IntMap Editor
, appNextId :: !EditorId
, appCurrentTab :: !EditorId
}

data Message = NewFile
             | OpenFile !FilePath
             | EditorMsg !EditorId !Edit.Message
             | RequestCloseEditor
             | CloseEditor EditorId
             | Exit
             | Fun String
             deriving (Show)

addEditorTab :: App -> Editor -> (App, EditorId)
addEditorTab app editor =
  let tabs' = M.insert (appNextId app) editor (appEditorTabs app) in
  (app { appEditorTabs = tabs', appNextId = succ . appNextId $ app}, appNextId app)

init :: [String] -> (App, Cmd Message)
init paths = App (M.fromList editors) 1 0 ! concat cmds
  -- Initialize editors, collect editors and commands and assign editor ids
  where editorsAndCmds = [0..] `zip` map (Edit.init . Just) paths
        (editors, cmds) = unzip $ (flip map) editorsAndCmds $
          \(eId, (editor, cmd)) -> ((eId, editor), wrapInner (EditorMsg eId) cmd)

update :: Message -> App -> (App, Cmd Message)
update NewFile app =
  let (editor, cmd) = Edit.init Nothing
      (app', editorId) = app `addEditorTab` editor in
  app' ! wrapInner (EditorMsg editorId) cmd

update (OpenFile path) app =
  let (editor, cmd) = Edit.init (Just path)
      (app', editorId) = app `addEditorTab` editor in
  app' ! wrapInner (EditorMsg editorId) cmd

update (EditorMsg editorId msg) app =
  case editorId `M.lookup` appEditorTabs app of
    Nothing -> (app, none)
    Just editor -> let (editor', cmd) = Edit.update msg editor in
      app { appEditorTabs = M.insert editorId editor' (appEditorTabs app) }
        ! wrapInner (EditorMsg editorId) cmd

update RequestCloseEditor app = undefined

update (CloseEditor editorId) app =
  app { appEditorTabs = editorId `M.delete` appEditorTabs app } ! none

data View = View {
  vMainWindow :: Window
, vNotebook :: Notebook
, vTabsToEditors :: IntMap Int
}

initView :: App -> (Message -> IO ()) -> IO View
initView app sendMsg = do
  window <- windowNew
  _ <- on window objectDestroy mainQuit -- TODO: throw framework signal to quit app?
  set window [containerBorderWidth := 10]
  set window [windowDefaultWidth := 1000, windowDefaultHeight := 500]

  notebook <- notebookNew

  editorToTabs <- (`M.traverseWithKey` appEditorTabs app) $ \ eId editor -> do
    editorView <- Edit.initView editor (sendMsg . EditorMsg eId)
    let label = fromMaybe "Opening..." $ Edit.currentFilePath editor
    notebookAppendPage notebook editorView label

  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]

  boxPackStart vBox notebook PackGrow 0

  containerAdd window vBox
  widgetShowAll window

  return $ View window notebook $ M.fromList . map swap . M.toAscList $ editorToTabs

updateView :: App -> ViewM Message View ()
updateView _ = return ()

finalize :: App -> View -> IO ()
finalize _app _view = return ()
