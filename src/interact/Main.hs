module Main where

import GUI
import Lua

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.IORef
import System.Exit

data InteractState = InteractState { getUI :: UI, getLuaRef :: IORef (Maybe Lua) }

makeCallbacks :: InteractState -> LuaCallbacks
makeCallbacks state = LuaCallbacks (handleOutput state) (handleError state)

clearLua :: InteractState -> Maybe String -> IO ()
clearLua state sepname = do
	lua <- readIORef $ getLuaRef state
	when (isJust lua) $ do 
		closeLua $ fromJust lua
		when (isJust sepname) $ outputUISeperator state $ fromJust sepname

outputUISeperator state name = uiPutStrTag (getUI state)"systag" $ "\n=====[" ++ name ++ "]=====\n"

startLocalInteraction state = do
	clearLua state $ Just "LOCAL"
	writeIORef (getLuaRef state) . Just =<< (startLocalLua $ makeCallbacks state)

startRemoteInteraction state ip = do
	clearLua state $ Just ip
	writeIORef (getLuaRef state) . Just =<< (startRemoteLua ip $ makeCallbacks state)

main = do 
	ui <- makeUI
	luaref <- newIORef Nothing
	let state = InteractState ui luaref
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
			_ ->
				putStrLn $ show action
				
handleOutput state msg = uiPutStr (getUI state) msg	
handleError state = putStrLn "handleError" >> startLocalInteraction state
