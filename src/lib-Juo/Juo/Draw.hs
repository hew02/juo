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
import Data.Char (ord)

import Foreign.C.Types()

import Juo
import Juo.Config

-- HSCurses abstraction
draw :: Juo.Window -> Int -> Int -> String -> IO ()
draw Juo.Window{..} y x txt =
    CUR.mvWAddStr win y x txt

drawCh :: Juo.Window -> Int -> Int -> Char -> IO ()
drawCh juoWin y x c = do
    CUR.wMove (win juoWin) y x
    _ <- CUR.waddch (win juoWin) (fromIntegral (ord c))
    return ()

drawLine :: Juo.Window -> Config -> Int -> Int -> String -> IO ()
drawLine win conf row col [] = 
    drawCh win row col ' '
drawLine win conf row col ('\t':content) = do
    draw win row col (replicate (tabDepth conf) ' ')
    drawLine win conf row (col + 4) content
drawLine win conf row col (c:content) = do
    drawCh win row col c
    drawLine win conf row (col + 1) content
            

paintContent :: Juo -> Config -> [DocumentLine] -> IO ()
paintContent Juo{..} conf buf = do
    CUR.wAttrSet (win editorWindow) (CUR.attr0, (CUR.Pair 2))
    go 0 winH (drop topOffset buf)
    where
        (winH, _) = windowSize
        
        go _ 2 _  = return ()
        go row n [] = do
            CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 8))
            CUR.mvWAddStr CUR.stdScr row 0 "~"
            go (row + 1) (n - 1) []
        go row n (line:rest) = do
            when (showLineNumbers conf) (CUR.mvWAddStr CUR.stdScr row 1 (show (row + 1)))
            _ <- drawLine editorWindow conf row 0 (lineContent line)
            go (row + 1) (n - 1) rest

drawToolbar :: Juo -> Config -> IO ()
drawToolbar Juo{..} settings = do    
    let (_, winW) = windowSize
        fileTypeStr = show documentType
        (l, c) = documentPos
        lHeight = 0
    
    let colStr = show (c + 1) ++ "C"
        lineStr = show (l + 1) ++ "L"

    let maxMultLen = 8
        modeText = show mode

    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 1))

    -- HACK there might be a better way of doing this
    draw toolbarWindow 0 0 (replicate winW ' ')
    --draw toolbarWindow 1 0 (replicate winW ' ')


    draw toolbarWindow 0 (1 + (length modeText)) filePath

    draw toolbarWindow 1 (winW - maxMultLen) (take (maxMultLen - 1) mult)

    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 3 - (length colStr)) colStr
    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 5 - (length colStr)) "|"
    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 6 - (length lineStr) - (length colStr)) lineStr

    draw toolbarWindow lHeight (winW - (length fileTypeStr) - 1) fileTypeStr

    -- Mode
    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 3))
    draw toolbarWindow 0 0 modeText

    CUR.wAttrSet (win toolbarWindow) (CUR.attr0, (CUR.Pair 5))
    when editedDocument ((draw toolbarWindow) 0 (1 + (length modeText) + (length filePath)) "*")
    draw toolbarWindow 1 1 messageBuf

    -- Command buffer
    when (mode == Juo.Command) ((draw toolbarWindow) 1 0 [(cursorCmd settings)])

-- | Sets to pipe character.
setBarCursor :: IO ()
setBarCursor = do
    putStr "\ESC[6 q"
    hFlush stdout

-- | Sets to full block character.
setBlockCursor :: IO ()
setBlockCursor = do
    putStr "\ESC[0 q"
    hFlush stdout

-- | Sets to underscore character.
setUnderscoreCursor :: IO ()
setUnderscoreCursor = do
    putStr "\ESC[4 q"
    hFlush stdout
