module Main where

import GUI
import Lua

import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Exit

main = do 
	ui <- makeUI
	lua <- startLocalLua (uiPutStrTag ui "luatag")

	forever $ do
		action <- getUIAction ui
		case action of
			CloseAction -> do
				putStrLn "Closing"
				closeLua lua
				exitWith ExitSuccess
			InputAction str -> do
				writeLua lua str
			_ ->
				putStrLn $ show action
