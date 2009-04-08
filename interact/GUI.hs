module GUI where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events
import Data.IORef

data UI = UI {
	mainwindow :: Window,
	connect :: MenuItem,
	disconnect :: MenuItem,
	openprgm :: MenuItem,
	reloadprgm :: MenuItem,	
	about :: MenuItem,
	output :: TextView,
	outputscroll :: ScrolledWindow,
	stop :: Button,
	status :: Statusbar,
	
	ipdialog :: Dialog,
	connectbtn :: Button,
	cancelbtn :: Button,
	ipentry :: Entry,
	
	aboutdialog :: AboutDialog,
	
	lastaction :: MVar GUIAction,
	keybuffer :: IORef String
}

data GUIAction = ConnectIPAction String | 
                 DisconnectAction | 
                 OpenProgramAction FilePath | 
                 ReloadProgramAction | 
                 InputAction String | 
                 StopAction | 
                 CloseAction
                 deriving (Eq, Show)
                 
makeUI :: IO UI
makeUI = do
	initGUI
	timeoutAdd (yield >> return True) 50
	forkIO mainGUI	

	lastaction <- newEmptyMVar
	
	Just xml <- xmlNew "ui.glade"
	
	[connect, disconnect, openprgm, reloadprgm, about] <-
		mapM (xmlGetWidget xml castToMenuItem)
		["connect", "disconnect", "openprgm", "reloadprgm", "about"]
		
	mainwindow <- xmlGetWidget xml castToWindow "mainwindow"
	output <- xmlGetWidget xml castToTextView "output"
	outputscroll <- xmlGetWidget xml castToScrolledWindow "outputscroll"
	stop <- xmlGetWidget xml castToButton "stop"
	status <- xmlGetWidget xml castToStatusbar "status"
	
	ipdialog <- xmlGetWidget xml castToDialog "ipdialog"
	connectbtn <- xmlGetWidget xml castToButton "connectbtn"
	cancelbtn <- xmlGetWidget xml castToButton "cancelbtn"
	ipentry <- xmlGetWidget xml castToEntry "ipentry"
	
	aboutdialog <- xmlGetWidget xml castToAboutDialog "aboutdialog"
	
	keybuffer <- newIORef ""
	
	let ui = UI mainwindow connect disconnect openprgm reloadprgm about output outputscroll stop status ipdialog connectbtn cancelbtn ipentry aboutdialog lastaction keybuffer
	
	onDestroy mainwindow      $ putMVar lastaction CloseAction >> mainQuit
	onClicked stop            $ tryPutMVar lastaction StopAction >> return ()
	onActivateLeaf disconnect $ tryPutMVar lastaction DisconnectAction >> return ()
	onActivateLeaf connect    $ entrySetText ipentry "" >> windowPresent ipdialog
	onActivateLeaf openprgm   $ onOpenPrgm ui
	onActivateLeaf reloadprgm $ tryPutMVar lastaction ReloadProgramAction >> return ()
	onActivateLeaf about      $ dialogRun aboutdialog >> widgetHide aboutdialog >> return ()
	onKeyPress output         $ onTypeOutput ui
	
	usertag <- textTagNew $ Just "usertag"
	set usertag [ textTagForeground := "blue", textTagFont := textfont ]
	luatag <- textTagNew $ Just "luatag"
	set luatag [ textTagForeground := "black", textTagFont := textfont ]
	errortag <- textTagNew $ Just "errortag"
	set errortag [ textTagForeground := "red", textTagFont := textfont ]
	tagtable <- textTagTableNew
	forM_ [ usertag, luatag, errortag] $ textTagTableAdd tagtable
	
	buf <- textBufferNew $ Just tagtable
	textViewSetBuffer output buf
	
	onDelete ipdialog         $ const $ widgetHide ipdialog >> return True
	onClicked cancelbtn       $ widgetHide ipdialog
	onClicked connectbtn      $ entryGetText ipentry >>= tryPutMVar lastaction . ConnectIPAction >> widgetHide ipdialog
	
	windowPresent mainwindow
	
	return ui
	
	where
		onOpenPrgm ui = do
			filechooser <- fileChooserDialogNew 
				(Just "Open Program") 
				(Just $ mainwindow ui) 
				FileChooserActionSelectFolder
				[("Open Folder", ResponseOk), ("Cancel", ResponseCancel)]
			windowPresent filechooser
			result <- dialogRun filechooser
			
			case result of
				ResponseOk -> do
					Just programdir <- fileChooserGetFilename filechooser
					tryPutMVar (lastaction ui) $ OpenProgramAction programdir
					return ()
				_ ->
					return ()
					
			widgetDestroy filechooser
			
		onTypeOutput ui event = 
			case getChar event of              
				Just ch   -> readIORef keybufref >>= \buf ->
				             let buf' = if ch == '\b' then
				                           if length buf > 1 then
				                              tail buf
				                           else
				                              buf
				                        else
				                           buf ++ [ch] in
				                
				             writeIORef keybufref buf' >>
				             uiPutStrTag ui "usertag" [ch] >>
				             if ch == '\n' then
				                isEmptyMVar (lastaction ui) >>= \empty ->
				                when empty sendbuf >>
					            return True	
					         else
				                return True
				Nothing   -> return False
			where
				getChar event = eventKeyChar event `mplus` maybeChoose (eventKeyVal event == 0xff0d) '\n'
				maybeChoose cond val = if cond then Just val else Nothing
				keybufref = keybuffer ui
				sendbuf = do
				    readIORef keybufref >>= \bufstr ->		                           
					(tryPutMVar (lastaction ui) $ InputAction bufstr) >>
					writeIORef keybufref "" >>
					return ()
				
		textfont = "Monospace 10"
	
	
getUIAction :: UI -> IO GUIAction
getUIAction = takeMVar . lastaction

uiPutStrTag :: UI -> String -> String -> IO ()
uiPutStrTag ui tagname msg = do
	buf <- textViewGetBuffer $ output ui
	end <- textBufferGetEndIter buf

	textBufferInsert buf end $ msg
	when (tagname /= "") $ do
		textIterBackwardChars end $ length msg
		newend <- textBufferGetEndIter buf
		textBufferApplyTagByName buf tagname end newend
	
	textViewScrollToIter (output ui) end 0 (Just (0, 1))
	return ()
	
uiPutStr :: UI -> String -> IO ()
uiPutStr ui msg = uiPutStrTag ui "luatag" msg
	
uiPutStrLn :: UI -> String -> IO ()
uiPutStrLn ui msg = uiPutStr ui $ msg ++ "\n"
	

		
		
