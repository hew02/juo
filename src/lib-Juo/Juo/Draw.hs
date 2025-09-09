{-# LANGUAGE RecordWildCards #-}

module Juo.Draw (
    paintContent,
    drawToolbar,

    setBarCursor,
    setBlockCursor,
    setUnderscoreCursor
) where

import qualified UI.HSCurses.Curses as CUR
--import qualified UI.HSCurses.CursesHelper as CURHELP
import Control.Monad (when)
import System.IO (hFlush, stdout)

import Juo
import Juo.Settings

-- HSCurses abstraction
draw :: Juo.Window -> Int -> Int -> String -> IO ()
draw Juo.Window{..} y x txt =
    CUR.mvWAddStr win y x txt

paintContent :: Juo -> UserSettings -> [DocumentLine] -> IO ()
paintContent Juo{..} settings buf = do
        --(y, _) = cursorPos juo
    CUR.wAttrSet (win editorWindow) (CUR.attr0, (CUR.Pair 2))
    go 0 winH buf
    where
        (winH, winW) = windowSize
        
        go _ 2 _  = return ()
        go row n [] = do
            CUR.mvWAddStr CUR.stdScr row 0 "~"
            go (row + 1) (n - 1) []
        go row n (line:rest) = do
            when (showLineNumbers settings) (CUR.mvWAddStr CUR.stdScr row 1 (show (row + 1)))
            draw editorWindow row 0 ((lineContent line))
            go (row + 1) (n - 1) rest

drawToolbar :: Juo -> UserSettings -> IO ()
drawToolbar Juo{..} settings = do    
    let (_, winW) = windowSize
        fileTypeStr = show documentType
        (l, c) = documentPos
        lHeight = 0
    
    let colStr = show (c + 1)
        lineStr = show (l + 1)

    let maxMultLen = 8
        modeText = show mode

    -- Left

    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 5))
    -- HACK there might be a better way of doing this
    draw toolbarWindow 0 0 (replicate winW ' ')

    -- Mode
    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 3))
    draw toolbarWindow 0 0 modeText

    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 5))
    draw toolbarWindow 1 0 (replicate winW ' ')

    -- File type
    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 2) fileTypeStr

    -- File path
    draw toolbarWindow 0 (1 + (length modeText)) filePath
    
    when editedDocument ((draw toolbarWindow) 0 (1 + (length modeText) + (length filePath)) "*")


    -- Command buffer
    when (mode == Juo.Command) ((draw toolbarWindow) 1 0 [(cursorCmd settings)])
    draw toolbarWindow 1 1 messageBuf

    -- Right
    draw toolbarWindow 1 (winW - maxMultLen) (take (maxMultLen - 1) mult)

    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 3 - (length colStr)) colStr
    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 5 - (length colStr)) "|"
    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 6 - (length lineStr) - (length colStr)) lineStr


setBarCursor :: IO ()
setBarCursor = do
    putStr "\ESC[6 q"
    hFlush stdout

setBlockCursor :: IO ()
setBlockCursor = do
    putStr "\ESC[0 q"
    hFlush stdout

setUnderscoreCursor :: IO ()
setUnderscoreCursor = do
    putStr "\ESC[4 q"
    hFlush stdout
