{-# LANGUAGE RecordWildCards #-}

module App.View (
  View
, initView
, updateView
, finalize
) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Tuple      (swap)

import           Graphics.UI.Gtk

import           App.Class
import           Edit            (Editor)
import qualified Edit

newtype TabId = TabId { _unTabId :: Int }
              deriving (Eq, Ord, Show)

data View = View {
  vMainWindow    :: Window
, vTabBar        :: Notebook
, vEditorsToTabs :: Map EditorId TabId
, vTabsToEditors :: Map TabId EditorId
}

initView :: App -> (Message -> IO ()) -> IO View
initView app sendMsg = do
  window <- windowNew
  _ <- on window objectDestroy mainQuit -- TODO: throw framework signal to quit app?
  set window [containerBorderWidth := 10]
  set window [windowDefaultWidth := 1000, windowDefaultHeight := 500]

  notebook <- notebookNew
  -- Tabs shopuld have equal width.
  set notebook [notebookHomogeneous := True]

  -- Initialize all editor views and place them in a tab.
  -- Save a mapping from editor IDs to tabs.
  editorsToTabs <- M.traverseWithKey (addEditorTab notebook sendMsg) (appEditors app)

  vBox <- vBoxNew False 0
  set vBox [boxHomogeneous := False]

  menuBar <- createMenuBar app sendMsg

  boxPackStart vBox menuBar PackNatural 0
  boxPackStart vBox notebook PackGrow 0

  containerAdd window vBox
  widgetShowAll window

  return $ View
    { vMainWindow = window
    , vTabBar = notebook
    , vEditorsToTabs = editorsToTabs
    , vTabsToEditors = invertedMap editorsToTabs
    }

updateView :: Message -> App -> (Message -> IO ()) -> View -> IO View
updateView message app sendMessage view@(View {..}) = do
  case message of
    NewFile -> do
      -- find out the new editor(s)
      let editors = appEditors app M.\\ vEditorsToTabs

      -- initialize their views and create tabs for them.
      mapping <- M.traverseWithKey (addEditorTab vTabBar sendMessage) editors

      -- update the mapping from tabs to editors
      return view { vEditorsToTabs = vEditorsToTabs `M.union` mapping
                  , vTabsToEditors = vTabsToEditors `M.union` invertedMap mapping
                  }

    -- Look up tabId and editor for eId. If it could not found there is some
    -- cleanup to do
    EditorMsg eId _ -> maybe (removeClosedEditorTabs app view) removeTab $ do
                editor <- eId `M.lookup` appEditors app
                tabId <- eId `M.lookup` vEditorsToTabs
                return (editor, tabId)
        where removeTab (editor, tabId) = do
                -- Lookup tab. If it is not present, do some cleanup.
                mTab <- notebookGetNthPage vTabBar (_unTabId tabId)
                case mTab of
                  Nothing -> removeClosedEditorTabs app view
                  Just tab -> do
                    notebookSetTabLabelText vTabBar tab (Edit.editorTitle editor)
                    return view

    _ -> return view

finalize :: App -> View -> IO ()
finalize _app _view = return ()


createMenuBar :: App -> (Message -> IO ()) -> IO MenuBar
createMenuBar _app sendMsg = do
  appMenuBar <- menuBarNew

  -- create and connect menu items
  newFile <- menuItemNewWithMnemonic ("_New" :: String)
  _ <- on newFile menuItemActivated $ sendMsg NewFile

  -- add menu items to menu
  fileMenu <- menuNew
  menuShellAppend fileMenu newFile

  -- create a menu opener and add it to the menu bar
  fileMenuOpener <- menuItemNewWithLabel ("File" :: String)
  menuItemSetSubmenu fileMenuOpener fileMenu
  menuShellAppend appMenuBar fileMenuOpener

  return appMenuBar

addEditorTab :: Notebook -> (Message -> IO ()) -> EditorId -> Editor -> IO TabId
addEditorTab notebook sendMsg eId editor = do
  -- Create a tab.
  -- Return the TabId (has to be wrapped).
  -- Make everything visible
  editorView <- Edit.initView editor (\m -> sendMsg (EditorMsg eId m))
  fmap TabId (notebookAppendPage notebook editorView (Edit.editorTitle editor)) <* widgetShowAll editorView

removeClosedEditorTabs :: App -> View -> IO View
removeClosedEditorTabs app view@(View {..}) = do
  -- Look for editors which are not contained anymore in the model
  let removedEditorIds = vEditorsToTabs M.\\ appEditors app
      -- Look for all tabs which have to be removed
      tabsToRemove = (`M.member` removedEditorIds) `M.filter` vTabsToEditors

  -- Remove all tabs which don't have a corresponding editor in the model
  _ <- traverse (notebookRemovePage vTabBar . _unTabId) $ M.keys tabsToRemove

  return view
    { vEditorsToTabs = vEditorsToTabs M.\\ removedEditorIds
    , vTabsToEditors = vTabsToEditors M.\\ tabsToRemove
    }

invertedMap :: (Ord a, Ord b) => Map a b -> Map b a
invertedMap = M.fromList . map swap . M.toAscList
