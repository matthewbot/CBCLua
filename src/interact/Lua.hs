module Lua where

import Control.Concurrent
import Control.Monad
import System.Process
import System.IO
import System.IO.Error
import Data.Maybe
	
data Lua = Lua { inputh :: Handle, outputh :: Handle, process :: ProcessHandle, outputthread :: ThreadId }

data LuaCallbacks = LuaCallbacks { onOutput :: String -> IO (), onTerminate :: IO () }

startLua :: String -> LuaCallbacks -> IO Lua
startLua command calls = do
	(inh, outh, _, proc) <- runInteractiveCommand command
	outputthread <- forkIO $ readProc outh calls
	return $ Lua inh outh proc outputthread
	
startLocalLua = startLua "lua -i 2>&1"

startRemoteLua ipaddr = startLua $ "ssh -o ConnectTimeout=2 -o StrictHostKeyChecking=no root@" ++ ipaddr ++ " \"/mnt/user/code/cbclua/run.sh interact 2>&1\" 2>&1"
	
closeLua :: Lua -> IO ()
closeLua lua = do	
	killThread $ outputthread lua
	try $ terminateProcess $ process lua
	forM_ (map ($ lua) [inputh, outputh]) $ try . hClose
	
writeLua :: Lua -> String -> IO ()
writeLua lua str = hPutStr handle str >> hFlush handle
	where
		handle = inputh lua
	
readProc :: Handle -> LuaCallbacks -> IO ()
readProc handle callbacks = do
	result <- try cpyloop
	case result of
		Left error -> do
		 	when (isEOFError error) $ onTerminate callbacks
		Right _ ->
			return ()
			
	where
		cpyloop = forever $ do
			str <- readHandle handle
			callbacks `onOutput` str
	
readHandle :: Handle -> IO String
readHandle handle = do
	hWaitForInput handle (-1)
	return . fromJust =<< readstr
	
	where readstr = do
		ready <- hReady handle
		if not ready then return Nothing else do
			char <- hGetChar handle
			nextstr <- readstr
			return $ Just $ case nextstr of
				Just str ->
					char : str
				Nothing ->
					[char]



				
				
		
