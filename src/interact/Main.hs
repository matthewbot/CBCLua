module Main where

import GUI
import Lua

import Control.Concurrent
import Control.Monad
import System.Process
import System.FilePath
import System.Directory
import Data.Maybe
import Data.IORef
import System.Exit

data InteractState = InteractState { getUI :: UI, getLuaRef :: IORef (Maybe Lua), getProgramDirRef :: IORef (Maybe String), getCurIPRef :: IORef (Maybe String) }

main = do 
	ui <- makeUI
	luaref <- newIORef Nothing
	programdirref <- newIORef Nothing
	curipref <- newIORef Nothing
	let state = InteractState ui luaref programdirref curipref
	let callbacks = makeCallbacks state
	
	startLocalInteraction state

	forever $ do
		action <- getUIAction ui
		case action of
			CloseAction -> do
				clearLua state Nothing
				exitWith ExitSuccess
			InputAction str -> do
				Just lua <- readIORef luaref
				writeLua lua str
			ConnectIPAction ip -> do
				startRemoteInteraction state ip
			DisconnectAction -> do
				startLocalInteraction state
			DownloadProgramAction dir -> do
				downloadProgram state dir
			ReloadProgramAction -> do
				Just dir <- readIORef programdirref
				downloadProgram state dir
				
handleOutput state msg = uiPutStr (getUI state) msg	
handleError state = putStrLn "handleError" >> startLocalInteraction state

makeCallbacks :: InteractState -> LuaCallbacks
makeCallbacks state = LuaCallbacks (handleOutput state) (handleError state)

clearLua :: InteractState -> Maybe String -> IO ()
clearLua state sepname = do
	lua <- readIORef $ getLuaRef state
	when (isJust lua) $ do 
		closeLua $ fromJust lua
		when (isJust sepname) $ outputUISeperator state $ fromJust sepname

outputUISeperator state name = uiPutStrTag (getUI state) "systag" $ "\n=====[" ++ name ++ "]=====\n"

startLocalInteraction state = do
	writeIORef (getCurIPRef state) Nothing
	clearLua state $ Just "LOCAL"
	writeIORef (getLuaRef state) . Just =<< (startLocalLua $ makeCallbacks state)

startRemoteInteraction state ip = do
	writeIORef (getCurIPRef state) $ Just ip
	clearLua state $ Just ip
	writeIORef (getLuaRef state) . Just =<< (startRemoteLua ip $ makeCallbacks state)
	
downloadProgram state dir = do
	let ui = getUI state
	writeIORef (getProgramDirRef state) $ Just dir
	Just ip <- readIORef (getCurIPRef state)
	uiPutSysStrLn ui "\nDownloading..."
	forkIO $ do
		let command = "tar -C \"" ++ dir ++ "\" -czf - . --exclude=\"^.*\" --exclude=\"*~\" -h -p | ssh root@" ++ ip ++ " \"rm -rf /mnt/user/code/cbclua/code/*; tar -C /mnt/user/code/cbclua/code -zxf - -p\""
		putStrLn command
		system command
		uiPutSysStr ui "Done!"
		startRemoteInteraction state ip
	return ()

