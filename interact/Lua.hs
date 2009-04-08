module Lua where

import Control.Concurrent
import Control.Monad
import System.Process
import System.IO
import System.IO.Error
import Data.Maybe
	
data Lua = Lua { inputh :: Handle, outputh :: Handle, process :: ProcessHandle, outputthread :: ThreadId }

type OutputCallback = String -> IO ()

startLocalLua :: OutputCallback -> IO Lua
startLocalLua outcall = do
	(Just inh, Just outh, _, proc) <- createProcess (shell "lua -i 2>&1"){ std_in=CreatePipe, std_out=CreatePipe }
	outputthread <- forkIO $ readProc outh outcall
	return $ Lua inh outh proc outputthread
	
closeLua :: Lua -> IO ()
closeLua lua = do
	forM_ (map ($ lua) [inputh, outputh]) hClose
		
	terminateProcess $ process lua
	
writeLua :: Lua -> String -> IO ()
writeLua lua str = hPutStr handle str >> hFlush handle
	where
		handle = inputh lua
	
readProc :: Handle -> OutputCallback -> IO ()
readProc handle callback = 
	try cpyloop >>= either (putStrLn . ("readProc going down: " ++) . show) (const $ return ())
	where
		cpyloop = forever $ do
			str <- readHandle handle
			callback str
	
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



				
				
		
