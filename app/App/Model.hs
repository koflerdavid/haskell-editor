module App.Model (
  App.Model.init
, update
) where

import qualified Data.Map.Strict as M

import App.Class

import Mvc.Gtk

import Edit (Editor)
import qualified Edit as Edit

init :: [String] -> (App, Cmd Message)
init paths = App (M.fromList editors) (EditorId (length paths)) (EditorId 0) ! concat cmds
  -- Initialize editors, collect editors and commands and assign editor ids
  where editorsAndCmds = map EditorId [0..] `zip` map (Edit.init . Just) paths
        (editors, cmds) = unzip $ (flip map) editorsAndCmds $
          \(eId, (editor, cmd)) -> ((eId, editor), wrapInner (EditorMsg eId) cmd)

update :: Message -> App -> (App, Cmd Message)
update NewFile app =
  let (editor, cmd) = Edit.init Nothing
      (app', editorId) = app `addEditor` editor in
  app' ! wrapInner (EditorMsg editorId) cmd

update (OpenFile path) app =
  let (editor, cmd) = Edit.init (Just path)
      (app', editorId) = app `addEditor` editor in
  app' ! wrapInner (EditorMsg editorId) cmd

update (EditorMsg editorId msg) app =
  case editorId `M.lookup` appEditors app of
    Nothing -> (app, none)
    Just editor -> let (editor', cmd) = Edit.update msg editor in
      app { appEditors = M.insert editorId editor' (appEditors app) }
        ! wrapInner (EditorMsg editorId) cmd

update RequestCloseEditor app = undefined

update (CloseEditor editorId) app =
  app { appEditors = editorId `M.delete` appEditors app } ! none


addEditor :: App -> Editor -> (App, EditorId)
addEditor app editor =
  let editors' = M.insert (appNextId app) editor (appEditors app) in
  (app { appEditors = editors', appNextId = succ . appNextId $ app}, appNextId app)
