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
import Juo.Types
import qualified UI.HSCurses.Curses as HC
import qualified Juo.Types as JT

-- HSCurses abstraction, eta -reduced
draw :: Window -> Int -> Int -> String -> IO ()
draw Window {..} = HC.mvWAddStr win

drawCh :: Window -> Int -> Int -> Char -> IO ()
drawCh juoWin y x c = do
  HC.wMove (win juoWin) y x
  _ <- HC.waddch (win juoWin) (fromIntegral (ord c))
  return ()

drawLine :: Window -> Config -> Int -> Int -> String -> IO ()
drawLine win _conf row col [] =
  drawCh win row col ' '
drawLine win conf row col ('\t' : content) = do
  draw win row col ("|" ++ replicate (tabDepth conf - 1) ' ')
  drawLine win conf row (col + 4) content
drawLine win conf row col (c : content) = do
  drawCh win row col c
  drawLine win conf row (col + 1) content


paintContent :: Juo -> Config -> [DocumentLine] -> IO ()
paintContent Juo {..} conf buf = do
  HC.wAttrSet (win editorWindow) (HC.attr0, HC.Pair 2)
  go 0 winH (drop vertOffset buf)
  where
    (winH, winW) = size editorWindow

    go _ 0 _ = return ()
    go row n [] = do
      setColor editorWindow EditorBackground
      draw editorWindow row 0 (replicate winW ' ')
      HC.wAttrSet HC.stdScr (HC.attr0, HC.Pair 7)
      HC.mvWAddStr HC.stdScr row 0 "~ "
      go (row + 1) (n - 1) []
    go row n (line : rest) = do
      -- Draw sidebard background for this line
      HC.wAttrSet HC.stdScr (HC.attr0, HC.Pair 7)
      HC.mvWAddStr HC.stdScr row 0 "  "

      -- Draw background for this line
      setColor editorWindow EditorBackground
      draw editorWindow row 0 (replicate winW ' ')

      when (showLineNumbers conf)
        (HC.mvWAddStr HC.stdScr row 1 (show (row + 1)))

      -- shift horizontally
      let visible = take winW $ drop horizOffset (lineContent line)

      _ <- drawLine editorWindow conf row 0 visible
      go (row + 1) (n - 1) rest


drawToolbar :: Juo -> Config -> IO ()
drawToolbar juo conf = do
  let toolbar = toolbarWindow juo
      currMode = mode juo
      exBuf = messageBuf juo
      multBuff = if multBuf juo /= 0 then show (multBuf juo) else ""
      cmdBuf = commandBuf juo
      isEdited = editedDocument juo
      file = currentFile juo
      _dx = dx juo
      _dy = dy juo

  let (_, winW) = size toolbar
      fileTypeStr = show (getFileType file)
      lHeight = 0
    
  let tabSpaces = getTabCount juo * tabDepth conf

  let colStr = show (_dx + 1 + tabSpaces) -- ++ "C"
      lineStr = show (_dy + 1) -- ++ "L"

  let maxMultLen = 8
      modeText = show currMode

  setColor toolbar Background
  draw toolbar 1 0 (replicate (winW - 1) ' ')
  draw toolbar 1 1 exBuf
  draw toolbar 1 (winW - maxMultLen - length multBuff  - length cmdBuf) (take (maxMultLen - 1) multBuff)
  draw toolbar 1 (winW - maxMultLen - length cmdBuf) (take (maxMultLen - 1) cmdBuf)


  -- Command buffer
  when (currMode == Command) (draw toolbar 1 0 [cursorCmd conf])

  setColor toolbar Toolbar
  draw toolbar 0 0 (replicate (winW - 1) ' ')
  draw toolbar 0 (1 + length modeText) (getFilePath file)
  draw toolbar lHeight (winW - length fileTypeStr - 4) ")"
  draw toolbar lHeight (winW - length fileTypeStr - 4 - length colStr) colStr
  draw toolbar lHeight (winW - length fileTypeStr - 5 - length colStr) ","
  draw toolbar lHeight (winW - length fileTypeStr - 5 - length lineStr - length colStr) lineStr
  draw toolbar lHeight (winW - length fileTypeStr - 6 - length lineStr - length colStr) "("
  draw toolbar lHeight (winW - length fileTypeStr - 2) fileTypeStr
  when isEdited (draw toolbar 0 (1 + length modeText + length (getFilePath file)) "*")

  -- Mode
  _ <- case currMode of
    JT.Insert -> setColor toolbar JT.ModeInsert
    JT.Select -> setColor toolbar JT.ModeSelect
    JT.Command -> setColor toolbar JT.ModeEx
    _ -> setColor toolbar Background
  draw toolbar 0 0 modeText
