{-# LANGUAGE RecordWildCards #-}

module Juo
  ( Juo (..),
    newJuo,
    moveCursor,
    updateCursor,
    deleteChar,
    deleteLine,
    deleteCharBefore,
    insertChar,
    newLine,
    newLinePush,
    insertTab,
    saveFile,
    addCharToMessage,
    appendChar,
    execCommand,
    exitJuo,
    insertNewMessage,
    resizeEditor,
    newWindow,

    handleNormalMode,

    initColors,

    FileDetails(..),
    setColor,
    getTabCount
  )
where

import Juo.Config
import Juo.Util
import qualified Juo.Types as JT

import qualified Data.Stack as DS

import Control.Exception (IOException, try)
import qualified Hasqtan as Hasq
import System.Environment (unsetEnv)
import System.Process (callCommand)
import qualified UI.HSCurses.Curses as HC
import qualified UI.HSCurses.CursesHelper as HCH
import Data.Char (digitToInt)

-- | height, width, y offset and x offset
--
-- Arguments:
--
-- *@h@ - Height
--
-- Returns a new `Juo` window.
newWindow :: Int -> Int -> Int -> Int -> IO JT.Window
newWindow h w yOff xOff = do
  newWin <- HC.newWin h w yOff xOff
  return
    JT.Window
      { win = newWin,
        size = (h - yOff, w - xOff),
        winHeight = h,
        winWidth = w
      }

class WindowDetails a where
  getWindow :: a -> HC.Window
  getWindowWidth :: a -> Int
  getWindowHeight :: a -> Int

instance WindowDetails JT.Window where
  getWindow       = JT.win
  getWindowWidth  = JT.winWidth
  getWindowHeight = JT.winHeight

data Juo = Juo
  { cy :: Int,
    cx :: Int,
    dy :: Int,
    dx :: Int,

    multBuf :: Int, -- Tracks numbers
    commandBuf :: String, -- Tracks letter commands

    lastCharPressed :: Maybe HC.Key,
    editorWindow :: JT.Window,
    toolbarWindow :: JT.Window,
    windowSize :: (Int, Int),
    currentFile :: JT.File,
    editedDocument :: Bool,
    mode :: JT.Mode,
    messageBuf :: String,
    fullMessageBuf :: [JT.DocumentLine],
    vertOffset :: Int,
    horizOffset :: Int,

    insertionBuffer :: String,

    hasqtanEnv :: (Hasq.OpEnv, Hasq.TypeEnv),
    hasqtanConf :: Hasq.Config,

    -- HACK for now we take the somewhat dumb approach
    -- of storing edited lines in this stack, we can slim this down to just 
    -- actions and positions of change later.
    undoHistory :: DS.Stack [(Int, JT.DocumentLine)] 

  }
  deriving (Eq)

getDX :: Juo -> Int
getDX = dx

getDY :: Juo -> Int
getDY = dy

getCX :: Juo -> Int
getCX = cx

getCY :: Juo -> Int
getCY = cy

class FileDetails a where
  getFileLength :: a -> Int
  getFileType :: a -> JT.FileType
  getFileContent :: a -> [JT.DocumentLine]
  getFilePath :: a -> String
  getLineLength :: a -> Int -> Int

  setFileLength :: a -> Int -> a
  setFileContent :: a -> [JT.DocumentLine] -> a

instance FileDetails JT.File where
  getFileLength = JT.fileLength
  getFileType = JT.fileType
  getFileContent = JT.fileContent
  getFilePath = JT.filePath

  getLineLength file line = if null (JT.fileContent file)
    then -1
    else JT.lineLength (JT.fileContent file !! line)

  setFileLength file len = file { JT.fileLength = len }
  setFileContent file content = file { JT.fileContent = content,
    JT.fileLength = length content
  }

{--- | Wrapper around `length` but we account for tab character width.
getLineLength :: String -> Int
getLineLength lineContent =
  sum $ map (\c -> if c == '\t' then 4 else 1) lineContent-}

instance FileDetails Juo where
  getFileLength = getFileLength . currentFile
  getFileType = getFileType . currentFile
  getFileContent = getFileContent . currentFile
  getFilePath = getFilePath . currentFile

  setFileLength juo len =
    juo { currentFile = setFileLength (currentFile juo) len }
  setFileContent juo content =
    juo { currentFile = setFileContent (currentFile juo) content }

newJuo :: Maybe JT.File -> IO Juo
newJuo maybeFile = do

  let dummyOffsetX = 2

  (winH, winW) <- HC.scrSize
  _toolbarWindow <- newWindow 2 winW (winH - 2) 0
  _editorWindow <- newWindow (winH - getWindowHeight _toolbarWindow) winW 0 dummyOffsetX

  HC.keypad (getWindow _editorWindow) True
  HC.noDelay (getWindow _editorWindow) True
  HC.keypad (getWindow _toolbarWindow) True
  HC.noDelay (getWindow _toolbarWindow) True

  let juo = Juo { cy = 0,
    cx = 0,
    dy = 0,
    dx = 0,
    editorWindow = _editorWindow,
    toolbarWindow = _toolbarWindow,
    windowSize = (winH, winW),
    multBuf = 0,
    commandBuf = "",
    mode = JT.Normal,
    messageBuf = "",
    fullMessageBuf = [JT.DocumentLine "" 0],
    editedDocument = False,
    vertOffset = 0,
    horizOffset = 0,

    insertionBuffer = "",

    hasqtanEnv = ([],[]),
    hasqtanConf = Hasq.Config {
      shouldShowType = False
    },

    undoHistory = DS.empty
  }

  case maybeFile of
    Nothing -> return juo
    Just file -> return juo {
      currentFile = file
    }

exitJuo :: IO ()
exitJuo = do
  -- CUR.endWin
  HCH.end
  callCommand "stty sane"
  unsetEnv "ESCDELAY"

-- | Get number of tabs before current document position.
getTabCount :: Juo -> Int
getTabCount juo =
  length $ filter (== '\t') (take (dx + 1) (JT.lineContent currLine))
  where
    dx = getDX juo
    text = getFileContent juo
    currLine =
      if null text then JT.DocumentLine "" 0
      else text !! getDY juo

updateCursor :: Juo -> Config -> IO Juo
updateCursor juo conf = do
  {-if x > lineLen || currentMode /= Juo.Insert
      then juo { cursorPos = (y, lineLen)
              , documentPos = (line, lineLen)
              }
      else juo-}

  let numOfTabsBefore = getTabCount juo

  -- Move based on # of tabs
  HC.wMove (JT.win (editorWindow juo)) (getCY juo) (getCX juo + (numOfTabsBefore * (tabDepth conf)))

  return juo

reportToMessageBuffer :: Juo -> String -> Juo
reportToMessageBuffer juo msg = do
    let newMessages =
          fullMessageBuf juo ++ [JT.DocumentLine msg (length msg)]

    juo {
      fullMessageBuf = newMessages
    }

-- TODO We need a 'setCursor' function with the same rules but not the 
-- useless computation of moving a cursor like.
moveCursor :: Juo -> JT.Direction -> Int -> Juo
moveCursor juo direction distance =
  loopMove juo direction distance
  where
    loopMove :: Juo -> JT.Direction -> Int -> Juo
    loopMove juo _ 0 = juo
    loopMove juo dir dist =
        let juo' = moveCursorOnce juo dir
        in if juo' == juo
          then juo
          else loopMove juo' dir (dist - 1)

moveCursorOnce :: Juo -> JT.Direction -> Juo
moveCursorOnce juo dir =
  let winH = getWindowHeight (editorWindow juo)
      winW = getWindowWidth (editorWindow juo)
      line = dy juo
      col = dx juo
      ls = getFileContent juo
      totalLines = length ls

      -- Clamps vertical traversal
      clampEnd :: Int -> Int -> Int
      clampEnd n len =
        if len <= 0 then 0 else min n (len - 1)

      -- Computes screen cx and new horizontal offset so that the dx is visible
      computeVisible :: Int -> Int -> (Int, Int)
      computeVisible dx' horiz
        | dx' < horiz = (0, dx')
        | dx' >= horiz + winW = (winW - 1, dx' - (winW - 1))
        | otherwise = (dx' - horiz, horiz)


   in case dir of
        JT.Up
          | line > 0 && cy juo > 0 ->
              let nextLine = line - 1
                  nextLength = JT.lineLength (ls !! nextLine)
                  newDx = clampEnd col nextLength
                  (newCx, newHoriz) = computeVisible newDx (horizOffset juo)
               in juo
                    { cy = cy juo - 1,
                      cx = newCx,
                      dy = nextLine,
                      dx = newDx,
                      horizOffset = newHoriz
                    }
          -- Scroll up viewport
          | line > 0 && cy juo == 0 ->
              let nextLine = line - 1
                  nextLength = JT.lineLength (ls !! nextLine)
                  newDx = clampEnd col nextLength
                  (newCx, newHoriz) = computeVisible newDx (horizOffset juo)
               in juo
                    { cx = newCx,
                      dy = nextLine,
                      dx = newDx,
                      horizOffset = newHoriz,
                      vertOffset = vertOffset juo - 1
                    }
          | otherwise -> juo
        JT.Down
          | line + 1 < totalLines && cy juo + 1 < winH ->
              let nextLine = line + 1
                  nextLength = JT.lineLength (ls !! nextLine)
                  dx' = clampEnd col nextLength
                  (cx', horizOffset') = computeVisible dx' (horizOffset juo)
              in juo
                    { cy = cy juo + 1,
                      cx = cx',
                      dy = nextLine,
                      dx = dx',
                      horizOffset = horizOffset'
                    }
          -- Scroll viewport down
          | line + 1 < totalLines && cy juo + 1 == winH ->
              let nextLine = line + 1
                  nextLength = JT.lineLength (ls !! nextLine)
                  dx' = clampEnd col nextLength
                  (cx', horizOffset') = computeVisible dx' (horizOffset juo)
              in juo
                { dy = nextLine,
                  dx = dx',
                  cx = cx',
                  horizOffset = horizOffset',
                  vertOffset = vertOffset juo + 1
                }

          | otherwise -> juo
        JT.Left
          | cx juo > 0 ->
            juo
              { cx = cx juo - 1,
                dx = col - 1
              }
          | cx juo == 0 && dx juo > 0 ->
            juo
              { dx = dx juo - 1,
                horizOffset = horizOffset juo - 1
              }
          | otherwise -> juo
        JT.Right
          -- Cursor moves ahead one while inserting.
          | mode juo == JT.Insert -> do
            if cx juo + 2 > winW then
                juo
                  { dx = dx juo + 1,
                    horizOffset = horizOffset juo + 1
                  }
              else
                juo
                  { cx = cx juo + 1,
                    dx = col + 1
                  }

          | cx juo + 3 < winW && col + 1 < currentLineLength -> do
            juo
              { cx = cx juo + 1,
                dx = col + 1
              }

          | col + 1 < currentLineLength -> do
            juo
              { dx = dx juo + 1,
                horizOffset = horizOffset juo + 1
              }

          | otherwise -> juo
  where
    currentLineLength = getLineLength (currentFile juo) (dy juo)

findDiff :: String -> String -> [(Char, Char, Int)]
findDiff s1 s2 =
  [ (c1, c2, i)
    | (c1, c2, i) <- zip3 s1 s2 [0 ..],
      c1 /= c2
  ]

insertNewMessage :: Juo -> String -> Juo
insertNewMessage juo msg =
  let newMessages = fullMessageBuf juo ++ [JT.DocumentLine msg (length msg)]
   in juo
        { fullMessageBuf = newMessages
        }

resizeEditor :: Juo -> IO (Juo, Bool)
resizeEditor juo = do
    HC.endWin
    HC.refresh
    (winH, winW) <- HC.scrSize

    let dummyOffsetX = 2
        offsetY = 2
        newMessage = "Resized to: " ++ show winH ++ "r x " ++ show winW ++ "c."

    _editorWindow <- newWindow (winH - offsetY) winW 0 dummyOffsetX
    _toolbarWindow <- newWindow 2 (winW + 1) (winH - 2) 0

    HC.delWin (getWindow (editorWindow juo))
    HC.delWin (getWindow (toolbarWindow juo))

    HC.wRefresh HC.stdScr
    HC.wRefresh (getWindow _editorWindow)
    HC.wRefresh (getWindow _toolbarWindow)

    -- CUR.wbkgd (win _editorWindow) (CUR.ChType ' ' (CUR.attr0, CUR.Pair 2))

    let _juo =
          insertNewMessage
              juo
              { windowSize = (winH, winW),
                  editorWindow = _editorWindow,
                  toolbarWindow = _toolbarWindow
              }
              newMessage

    HC.bkgrndSet HC.attr0 (HC.Pair 2)

    return (_juo, True)

updateLine :: String -> Maybe JT.DocumentLine  -> JT.DocumentLine
updateLine content maybeLine =
  let n = length content in
  case maybeLine of
    Just currentLine -> currentLine { -- NOTE not necessary, but could use this route for optimization later.
      JT.lineContent = content,
      JT.lineLength = n
    }
    Nothing -> JT.DocumentLine {
      JT.lineContent = content,
      JT.lineLength = n
    }

-- | Applies a transformation to the line at cursor
doAtCursor :: Juo -> (String -> Int -> String) -> Juo
doAtCursor juo f =
  let oldText = JT.lineContent currLine
      newText = f oldText x
      updatedLine = updateLine newText (Just currLine)
      newLines = take y text ++ [updatedLine] ++ drop (y + 1) text
   in setFileContent juo newLines
  where
    y = dy juo
    x = dx juo
    text = getFileContent juo
    currLine = if null text then JT.DocumentLine "" 0 else text !! y

deleteChar :: Juo -> Maybe Int -> Juo
deleteChar juoO maybeMult = 
  let text = getFileContent juoO
      prevState = text !! dy juoO
      juo = case maybeMult of
        Just mult -> loopDeleteChar juoO mult
        Nothing -> loopDeleteChar juoO 1
  in
    juo { undoHistory = DS.push [(dy juoO, prevState)] (undoHistory juo) }
  where
    loopDeleteChar :: Juo -> Int -> Juo
    loopDeleteChar juo 0 = juo
    loopDeleteChar juo mult = do
        let text = getFileContent juo
            currLine = if null text then JT.DocumentLine "" 0 else text !! dy juo
            juo' = doAtCursor juo $ \content col ->
              take col content ++ drop (col + 1) content
            --newMessage = show JT.Inserted ++ " `" ++ insertionBuffer juo ++ "` @ " ++ show (dx juo + 1) ++ ":" ++ show (dy juo + 1)
        if dx juo' + 1 == JT.lineLength currLine
          then
            loopDeleteChar (moveCursor juo' JT.Left 1) (mult - 1)
          else 
            loopDeleteChar juo' (mult - 1)


newLine :: Juo -> Juo
newLine juo = do
  let nl = updateLine "" Nothing
      nls = take (line + 1) text ++ [nl] ++ drop (line + 1) text
   in moveCursor
        (setFileContent (juo { mode = JT.Insert }) nls)
        JT.Down
        1
  where
    line = dy juo
    text = getFileContent juo

newLinePush :: Juo -> Juo
newLinePush juo = do
  let oldContent = take col (JT.lineContent currLine)
      newContent = drop col (JT.lineContent currLine)
      ol = updateLine oldContent Nothing
      nl = updateLine newContent Nothing
      newLines = take line text ++ [ol] ++ [nl] ++ drop (line + 1) text
   in setFileContent (juo { cx = 0, mode = JT.Insert }) newLines
  where
    line = dy juo
    col = dx juo
    text = getFileContent juo
    currLine = if null text then JT.DocumentLine "" 0 else text !! line

deleteLine :: Juo -> Juo
deleteLine juo = do
  let newLines = take line text ++ drop (line + 1) text
      _juo = setFileContent juo newLines

  if line == getFileLength _juo
    then
      moveCursor _juo JT.Up 1
    else
      _juo
  where
    line = dy juo
    text = getFileContent juo

deleteCharBefore :: Juo -> Juo
deleteCharBefore juo = do
  let juo' = doAtCursor juo $ \content col ->
        take (col - 1) content ++ drop col content

      line = dy juo
      text = getFileContent juo
      currLine = if null text then JT.DocumentLine "" 0 else text !! line
  moveCursor juo' JT.Left 1

insertChar :: Juo -> Char -> Juo
insertChar juo ch =
  let juo' = doAtCursor juo $ \content col ->
        take col content ++ [ch] ++ drop col content
  in
  --newMessages = fullMessageBuf juo ++ [JT.DocumentLine newMessage (length newMessage)]
  moveCursor (juo' { insertionBuffer = insertionBuffer juo ++ [ch]}) JT.Right 1

insertTab :: Juo -> Juo
insertTab juo =
  let juo' = doAtCursor juo $ \content col ->
        take col content ++ replicate 4 ' ' ++ drop col content
   in moveCursor juo' JT.Right 1

appendChar :: Juo -> Juo
appendChar juo =
  moveCursor juo {mode = JT.Insert} JT.Right 1

addCharToMessage :: Juo -> Char -> Juo
addCharToMessage juo ch = juo {messageBuf = messageBuf juo ++ [ch]}

execCommand :: Juo -> IO (Juo, Bool)
execCommand juo = do
  let (_, winW) = windowSize juo
  -- NOTE temporary, expand the usage of Hasqtan
  case removeSpaces (messageBuf juo) of
    "q" -> do
      HC.beep
      return (juo, False)
    "exit" -> return (juo, False)
    "w" -> do
      juo' <- Juo.saveFile juo
      return (juo', True)
    "write" -> do
      juo' <- Juo.saveFile juo
      return (juo', True)
    "wq" -> do
      juo' <- Juo.saveFile juo
      return (juo', False)
    "" -> do
      let juo' =
            juo
              { mode = JT.Normal
              }
      return (juo', True)
    _ -> do
      let (maybeOut, env) = Hasq.interp (messageBuf juo) (hasqtanEnv juo) (hasqtanConf juo)
      return (
        case maybeOut of
          Just out ->
                juo
                  { messageBuf = take winW (out),
                    mode = JT.Normal
                  }
          Nothing ->
            juo { messageBuf = "", mode = JT.Normal }
          , True)

getFileSize :: Juo -> Int
getFileSize juo = sum (map len (getFileContent juo))
  where
    len docLine = JT.lineLength docLine + 1 -- Add 1 for '\n'

saveFile :: Juo -> IO Juo
saveFile juo = do
  -- TODO Might be better to chunk write
  res <-
    try (writeFile (getFilePath juo) (unlines (map JT.lineContent (getFileContent juo)))) ::
      IO (Either IOException ())

  let buf = case res of
        Prelude.Left _ -> show (getFilePath juo) ++ show (getFileSize juo) ++ "B failed to write"
        Prelude.Right _ -> do
          show (getFilePath juo) ++ " " ++ show (getFileLength juo) ++ "L, " ++ show (getFileSize juo) ++ "B written"

  return
    juo
      { messageBuf = buf,
        mode = JT.Normal
      }


-- | Configure theme colors based on config.
initColors :: Config -> IO ()
initColors conf = do
  newColor (HC.Color 1) (toolbarBgColor conf)
  newColor (HC.Color 2) (toolbarBgColor conf)


  HC.initPair (HC.Pair 1) HCH.black (HC.Color 1)

  -- CUR.initPair (CUR.Pair 1) HCH.white HCH.defaultColor

  newColor (HC.Color 33) (255, 255, 255)

  newColor (HC.Color 32) (85, 85, 85)

  newColor (HC.Color 44) (57, 255, 20)

  newColor (HC.Color 69) (18, 18, 18)
  newColor (HC.Color 72) (12, 12, 12)

  newColor (HC.Color 75) (0, 0, 255)
  newColor (HC.Color 76) (0, 0, 220)


  HC.initPair (HC.Pair 2) (HC.Color 33) (HC.Color 69)

  -- Background
  HC.initPair (HC.Pair 8) HCH.black HCH.white
  -- Mode
  HC.initPair (HC.Pair 3) (HC.Color 69) (HC.Color 44)
  -- Toolbar
  HC.initPair (HC.Pair 5) HCH.white HCH.black
  -- Editor
  HC.initPair (HC.Pair 6) HCH.black HCH.white
  -- Gutter
  HC.initPair (HC.Pair 7) HCH.black HCH.white
  -- Ex Mode
  HC.initPair (HC.Pair 10) HCH.black HCH.white
  -- Insert Mode
  HC.initPair (HC.Pair 11) (HC.Color 69) (HC.Color 44)
  -- Select Mode
  HC.initPair (HC.Pair 12) HCH.black HCH.white

-- | API function for configurable theme options.
setColor :: JT.Window -> JT.EditorSection -> IO ()
setColor JT.Window{..} section =
  let color = case section of
        JT.Background -> (HC.attr0, HC.Pair 8)
        JT.EditorBackground -> (HC.attr0, HC.Pair 6)
        JT.Toolbar -> (HC.attr0, HC.Pair 5)
        JT.LeftColumn -> (HC.attr0, HC.Pair 7)
        JT.ModeEx -> (HC.attr0, HC.Pair 10)
        JT.ModeInsert -> (HC.attr0, HC.Pair 11)
        JT.ModeSelect -> (HC.attr0, HC.Pair 12)
  in
    HC.wAttrSet win color

undo :: Juo -> [(Int, JT.DocumentLine)] -> IO Juo
undo juo [] = return juo
undo juo ((y, line):undoneLines) = do
  let text = getFileContent juo
      restoredFile = take y text ++ [line] ++ drop (y + 1) text
  
  
  undo (setFileContent juo { dy = y, cy = y } restoredFile) undoneLines


-- | WARNING: Only works on positives.
addDigit :: Int -> Int -> Int
addDigit x y = 10 * x + y

-- | A monolithic function that handles normal mode interactions
handleNormalMode :: Juo -> Config -> HC.Key -> IO (Juo, Bool)
handleNormalMode juo conf ch = do
  let mult = if multBuf juo == 0 then multBuf juo + 1 else multBuf juo

  case ch of
    HC.KeyChar c

      | c == delete conf -> do
          return (deleteChar juo { commandBuf = [c], multBuf = 0} (Just mult), True)

      | c `elem` ['1' .. '9'] -> do return (juo { multBuf = addDigit (multBuf juo) (digitToInt c)}, True)

      -- HACK: Really lazy on my part :(
      | c == '0' ->
        if multBuf juo == 0
          then
            return (moveCursor juo JT.Left 999999, True)
          else
            return (juo { multBuf = addDigit (multBuf juo) (digitToInt c)}, True)
      | c == '$' -> return (moveCursor juo JT.Right 999999, True)

      | c == 'G' -> return (moveCursor juo JT.Down 999999, True)

      -- Enters a 'sub-mode', or in other words a multi character command
      | c `elem` ['d', 'g'] -> do
        case c of
          'd' ->
            setUnderscoreCursor
          _ ->
            setBlockCursor

        return (juo {commandBuf = commandBuf juo ++ [c]}, True)

      | c == 'r' -> do
          let _juo =
                juo
                  { mode = JT.Normal
                  }
          return (_juo, True)

      | c == 'u' -> do
          let (maybeChangedLines, undoHistory') = DS.pop (undoHistory juo)
          _juo <- 
            case maybeChangedLines of
              Just changedLines -> do
                undo juo { undoHistory = undoHistory', 
                  messageBuf = "undo #" ++ show (DS.size undoHistory' + 1) } changedLines
              Nothing -> do
                return juo { undoHistory = undoHistory',
                  messageBuf = "undo stack is empty!" }
          return (_juo, True)

      -- NOTE: SWITCH MODE
      | c == 'i' -> do
          let _juo =
                juo
                  { mode = JT.Insert
                  }
          return (_juo, True)
      | c == 'a' -> return (appendChar juo, True)
      | c == 'o' -> return (newLine juo, True)
      | c == 's' -> do
          --HC.wAttrSet HC.stdScr (CUR.attr0, (CUR.Pair 3))

          let _juo =
                juo
                  { mode = JT.Select
                  }
          return (_juo, True)
      | c == 'm' -> do
          let _juo =
                juo
                  { mode = JT.Message
                  }
          return (_juo, True)

      | c == ':' -> do
          return (juo
                  { mode = JT.Command,
                    messageBuf = ""
                  },
                  True)
      | c == ';' -> do
          return (juo
                  { mode = JT.Command,
                    messageBuf = ""
                  },
                  True)

      -- TODO Could add these to command buffer but doesnt seem to be necessary
      | c == up conf ->
        return (moveCursor (juo { commandBuf = [c], multBuf = 0}) JT.Up mult, True)
      | c == down conf ->
        return (moveCursor (juo { commandBuf = [c], multBuf = 0}) JT.Down mult, True)
      | c == left conf ->
        return (moveCursor (juo { commandBuf = [c], multBuf = 0}) JT.Left mult, True)
      | c == right conf ->
        return (moveCursor (juo { commandBuf = [c], multBuf = 0}) JT.Right mult, True)

      | otherwise -> return (insertNewMessage juo (keyboardCharErrMsg c), True)

    HC.KeyMouse -> do
      mEvent <- HC.getMouse
      case mEvent of
        Nothing -> return (juo, True)  -- no event info
        Just mouse -> do
          case HC.mouseEventButton mouse of
            (HC.ButtonPressed 4 : _) -> do
              -- scroll up
              return (juo, True)
            (HC.ButtonPressed 5 : _) -> do
              -- scroll down
              return (juo, True)
            (HC.ButtonReleased 4 : _) -> do
              -- scroll up (release event)
              return (juo, True)
            (HC.ButtonReleased 5 : _) -> do
              -- scroll down (release event)
              return (juo, True)
            _ -> return (juo, True)

    -- NOTE: these are not configurable via config files.
    HC.KeyUp -> return (moveCursor juo JT.Up (scrollDistance conf), True)
    HC.KeyDown -> return (moveCursor juo JT.Down (scrollDistance conf), True)
    {-HC.KeyLeft ->
          if (mult juo) == "d"
            then do
              let _juo = juo {mult = ""}
              setBlockCursor
              return (deleteChar (moveCursor _juo Left 1), True)
            else
              return ((moveCursor juo Left 1), True)
    HC.KeyRight ->
          if (mult juo) == "d"
            then do
              let _juo = juo {mult = ""}
              setBlockCursor
              return (deleteChar (moveCursor _juo JT.Right 1), True)
            else
              return ((moveCursor juo JT.Right 1), True)-}

    key -> return (insertNewMessage juo (cursesKeyErrMsg key), True)

  where
    keyboardCharErrMsg c = "Unrecognized input: _" ++ show c ++ "_"
    cursesKeyErrMsg key = "Unrecognized input: _" ++ show key ++ "_"