{-# LANGUAGE RecordWildCards #-}

module Juo.Draw
  ( paintContent,
    drawToolbar,
    setBarCursor,
    setBlockCursor,
    setUnderscoreCursor,
  )
where

-- import qualified UI.HSCurses.CursesHelper as CURHELP
import Control.Monad (when)
import Data.Char (ord)
import Foreign.C.Types ()
import Juo
import Juo.Util
import Juo.Config
import qualified UI.HSCurses.Curses as CUR

-- HSCurses abstraction
draw :: Juo.Window -> Int -> Int -> String -> IO ()
draw Juo.Window {..} y x txt = CUR.mvWAddStr win y x txt

drawCh :: Juo.Window -> Int -> Int -> Char -> IO ()
drawCh juoWin y x c = do
  CUR.wMove (win juoWin) y x
  _ <- CUR.waddch (win juoWin) (fromIntegral (ord c))
  return ()

drawLine :: Juo.Window -> Config -> Int -> Int -> String -> IO ()
drawLine win _conf row col [] =
  drawCh win row col ' '
drawLine win conf row col ('\t' : content) = do
  draw win row col ("|" ++ (replicate (tabDepth conf - 1) ' '))
  drawLine win conf row (col + 4) content
drawLine win conf row col (c : content) = do
  drawCh win row col c
  drawLine win conf row (col + 1) content

setColor :: Juo.Window -> EditorSection -> IO ()
setColor Juo.Window{..} section =
  let color = case section of
        Background -> (CUR.attr0, CUR.Pair 8)
        EditorBackground -> (CUR.attr0, CUR.Pair 6)
        Toolbar -> (CUR.attr0, CUR.Pair 1)
        LeftColumn -> (CUR.attr0, CUR.Pair 7)
  in
    CUR.wAttrSet win color


paintContent :: Juo -> Config -> [DocumentLine] -> IO ()
paintContent Juo {..} conf buf = do
  CUR.wAttrSet (win editorWindow) (CUR.attr0, CUR.Pair 2)
  go 0 winH (drop vertOffset buf)
  where
    (winH, winW) = size editorWindow

    go _ 0 _ = return ()
    go row n [] = do
      setColor editorWindow EditorBackground
      draw editorWindow row 0 (replicate winW ' ')
      CUR.wAttrSet CUR.stdScr (CUR.attr0, CUR.Pair 7)
      CUR.mvWAddStr CUR.stdScr row 0 "~ "
      go (row + 1) (n - 1) []
    go row n (line : rest) = do
      -- Draw sidebard background for this line
      CUR.wAttrSet CUR.stdScr (CUR.attr0, CUR.Pair 7)
      CUR.mvWAddStr CUR.stdScr row 0 "  "

      -- Draw background for this line
      setColor editorWindow EditorBackground
      draw editorWindow row 0 (replicate winW ' ')

      when (showLineNumbers conf) 
        (CUR.mvWAddStr CUR.stdScr row 1 (show (row + 1)))

      -- shift horizontally
      let visible = take winW $ drop horizOffset (lineContent line)

      _ <- drawLine editorWindow conf row 0 visible
      go (row + 1) (n - 1) rest


drawToolbar :: Juo -> Config -> IO ()
drawToolbar Juo {..} settings = do
  let (_, winW) = size toolbarWindow
      fileTypeStr = show (getFileType currentFile)
      lHeight = 0

  let colStr = show (dx + 1) -- ++ "C"
      lineStr = show (dy + 1) -- ++ "L"

  let maxMultLen = 8
      modeText = show mode

  setColor toolbarWindow Background
  draw toolbarWindow 1 0 (replicate (winW - 1) ' ')
  draw toolbarWindow 1 1 messageBuf
  draw toolbarWindow 1 (winW - maxMultLen) (take (maxMultLen - 1) mult)

  -- Command buffer
  when (mode == Juo.Command) (draw toolbarWindow 1 0 [cursorCmd settings])

  setColor toolbarWindow Toolbar
  draw toolbarWindow 0 0 (replicate (winW - 1) ' ')
  draw toolbarWindow 0 (1 + length modeText) (getFilePath currentFile)
  draw toolbarWindow lHeight (winW - length fileTypeStr - 4) ")"
  draw toolbarWindow lHeight (winW - length fileTypeStr - 4 - length colStr) colStr
  draw toolbarWindow lHeight (winW - length fileTypeStr - 5 - length colStr) ","
  draw toolbarWindow lHeight (winW - length fileTypeStr - 5 - length lineStr - length colStr) lineStr
  draw toolbarWindow lHeight (winW - length fileTypeStr - 6 - length lineStr - length colStr) "("

  draw toolbarWindow lHeight (winW - length fileTypeStr - 2) fileTypeStr

  -- Mode
  CUR.wAttrSet (win toolbarWindow) (CUR.attr0, CUR.Pair 3)
  draw toolbarWindow 0 0 modeText

  when editedDocument (draw toolbarWindow 0 (1 + length modeText + length (getFilePath currentFile)) "*")