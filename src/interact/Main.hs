module Main where

import GUI
import Lua

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.IORef
import System.Exit

main = do 
	ui <- makeUI
	luaref <- newIORef =<< startLocalLua (uiPutStrTag ui "luatag")

	forever $ do
		action <- getUIAction ui
		case action of
			CloseAction -> do
				putStrLn "Closing"
				closeLua =<< readIORef luaref
				exitWith ExitSuccess
			InputAction str -> do
				lua <- readIORef luaref
				writeLua lua str
			ConnectIPAction ip -> do
				closeLua =<< readIORef luaref
				writeIORef luaref =<< startRemoteLua ip (uiPutStrTag ui "luatag")
			_ ->
				putStrLn $ show action
