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
    downloadprgm :: MenuItem,
    reloadprgm :: MenuItem,    
    about :: MenuItem,
    output :: TextView,
    outputscroll :: ScrolledWindow,
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
                 DownloadProgramAction FilePath | 
                 ReloadProgramAction | 
                 InputAction String | 
                 CloseAction
                 deriving (Eq, Show)
                 
makeUI :: IO UI
makeUI = do
    initGUI
    timeoutAdd (yield >> return True) 50
    forkIO mainGUI    

    lastaction <- newEmptyMVar
    
    Just xml <- xmlNew "ui.glade"
    
    [connect, disconnect, downloadprgm, reloadprgm, about] <-
        mapM (xmlGetWidget xml castToMenuItem)
        ["connect", "disconnect", "downloadprgm", "reloadprgm", "about"]
        
    mainwindow <- xmlGetWidget xml castToWindow "mainwindow"
    output <- xmlGetWidget xml castToTextView "output"
    outputscroll <- xmlGetWidget xml castToScrolledWindow "outputscroll"
    status <- xmlGetWidget xml castToStatusbar "status"
    
    ipdialog <- xmlGetWidget xml castToDialog "ipdialog"
    connectbtn <- xmlGetWidget xml castToButton "connectbtn"
    cancelbtn <- xmlGetWidget xml castToButton "cancelbtn"
    ipentry <- xmlGetWidget xml castToEntry "ipentry"
    
    aboutdialog <- xmlGetWidget xml castToAboutDialog "aboutdialog"
    
    keybuffer <- newIORef ""
    
    let ui = UI mainwindow connect disconnect downloadprgm reloadprgm about output outputscroll status ipdialog connectbtn cancelbtn ipentry aboutdialog lastaction keybuffer
    
    onDestroy mainwindow         $ putMVar lastaction CloseAction >> mainQuit
    onActivateLeaf disconnect    $ tryPutMVar lastaction DisconnectAction >> return ()
    onActivateLeaf connect       $ entrySetText ipentry "" >> windowPresent ipdialog
    onActivateLeaf downloadprgm  $ onOpenPrgm ui
    onActivateLeaf reloadprgm    $ tryPutMVar lastaction ReloadProgramAction >> return ()
    onActivateLeaf about         $ dialogRun aboutdialog >> widgetHide aboutdialog >> return ()
    onKeyPress output            $ onTypeOutput ui
    
    usertag <- makeTag "usertag" "blue"
    luatag <- makeTag "luatag" "black"
    systag <- makeTag "systag" "red"
    tagtable <- textTagTableNew
    forM_ [ usertag, luatag, systag ] $ textTagTableAdd tagtable
    
    textViewSetBuffer output =<< (textBufferNew $ Just tagtable)
    
    onDelete ipdialog         $ const $ widgetHide ipdialog >> return True
    onClicked cancelbtn       $ widgetHide ipdialog
    onClicked connectbtn      $ entryGetText ipentry >>= tryPutMVar lastaction . ConnectIPAction >> widgetHide ipdialog
    
    windowPresent mainwindow
    
    return ui
    
    where     
        makeTag name color = do
            usertag <- textTagNew $ Just name
            set usertag [ textTagForeground := color, textTagFont := "Monospace 10" ]
            return usertag  
        
        onOpenPrgm ui = do
            filechooser <- fileChooserDialogNew 
                (Just "Open Program") 
                (Just $ mainwindow ui) 
                FileChooserActionSelectFolder
                [("Open Folder", ResponseOk), ("Cancel", ResponseCancel)]
            windowPresent filechooser
            result <- dialogRun filechooser
            
            when (result == ResponseOk) $ do
                Just programdir <- fileChooserGetFilename filechooser
                tryPutMVar (lastaction ui) $ DownloadProgramAction programdir
                return ()
                    
            widgetDestroy filechooser
            
        onTypeOutput ui event = 
            case getChar event of              
                Just ch -> do
                    buf <- readIORef keybufref -- read buffer
                    let (buf',passkey) = alterbuf buf ch -- alter it to obtain buf'
                    writeIORef keybufref buf' -- write the new buffer
                    when passkey (dispchar ui ch) -- if we should pass this key, send it to the screen
                    when (ch == '\n') (sendbuf buf') -- if its a newline, send the buffer to lua
                    return True 
                Nothing -> return False
            where
                getChar event = eventKeyChar event `mplus` 
                                maybeChoose (eventKeyVal event == 0xff0d) '\n' `mplus` 
                                maybeChoose (eventKeyVal event == 0xff08) '\b'
                                
                maybeChoose cond val | cond      = Just val
                                     | otherwise = Nothing
                                     
                keybufref = keybuffer ui
                
                sendbuf bufstr = do                          
                    tryPutMVar (lastaction ui) $ InputAction bufstr
                    writeIORef keybufref ""
                    return ()
                    
                dispchar ui ch | ch == '\b' = uiDeleteChars ui 1 
                               | otherwise  = uiPutStrTag ui "usertag" [ch]
                                     
                alterbuf buf ch | ch == '\b' = if buflen > 0 -- if there are some chars in the buffer
                                                  then (take (buflen-1) buf, True) -- take all but the last one
                                                  else (buf, False) -- no chars, then pass empty buffer and False to ignore the key
                                | otherwise  = (buf ++ [ch], True) -- normal char, just append it to the end
                                where buflen = length buf
    
    
getUIAction :: UI -> IO GUIAction
getUIAction = takeMVar . lastaction

uiDeleteChars :: UI -> Int -> IO ()
uiDeleteChars ui count = do
    buf <- textViewGetBuffer $ output ui
    end <- textBufferGetEndIter buf
    prevend <- textBufferGetEndIter buf
    textIterBackwardChars prevend count
    
    textBufferDelete buf prevend end

writeText :: (UI -> TextView) -> UI -> String -> String -> IO ()
writeText outputfield ui tagname msg = do
    buf <- textViewGetBuffer $ outputfield ui
    end <- textBufferGetEndIter buf
    textBufferInsert buf end $ msg
    newend <- textBufferGetEndIter buf
    
    when (tagname /= "") $ do
    	end <- textBufferGetEndIter buf
        textIterBackwardChars end $ length msg
        textBufferApplyTagByName buf tagname end newend
    
    endmark <- textBufferCreateMark buf Nothing newend True
    textViewScrollToMark (outputfield ui) endmark 0 $ Just (1.0, 1.0)
    return ()

uiPutStrTag :: UI -> String -> String -> IO ()
uiPutStrTag = writeText output
    
uiPutStr :: UI -> String -> IO ()
uiPutStr ui msg = uiPutStrTag ui "luatag" msg
    
uiPutStrLn :: UI -> String -> IO ()
uiPutStrLn ui msg = uiPutStr ui $ msg ++ "\n"

uiPutSysStr :: UI -> String -> IO ()
uiPutSysStr ui msg = uiPutStrTag ui "systag" msg

uiPutSysStrLn :: UI -> String -> IO ()
uiPutSysStrLn ui msg = uiPutSysStr ui $ msg ++ "\n"
    
