module Main where

import Mvc.Gtk

import App as App

main :: IO ()
main = do
  args <- initGUI
  runProgramWithFlags args App.init App.update App.initView App.updateView App.finalize
