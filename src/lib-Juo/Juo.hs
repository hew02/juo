{-# LANGUAGE RecordWildCards #-}

module Juo
  ( Juo (..),
    newJuo,
    Direction (..),
    DocumentLine (..),
    FileType (..),
    File(..),
    Mode (..),
    Window (..),
    EditorSection(..),
    FileDetails(..),
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

    initColors
  )
where

import Juo.Config
import Juo.Util

import Control.Exception (IOException, try)
import Control.Monad (when)
import qualified Hasqtan as Hasq
import System.Environment (unsetEnv)
import System.IO
import System.Process (callCommand)
import qualified UI.HSCurses.Curses as HC
import qualified UI.HSCurses.CursesHelper as HCH

-- | Juo's builtin file types.
--
-- __Examples:__
--
-- @
-- c = 'Unknown' \"C\"
-- @
data FileType
  = Haskell
  | PlainText
  | Markdown
  | C
  | Cpp
  | Hasqtan
  | Java
  | OCaml
  | Unknown String
  deriving (Eq)

instance Show FileType where
  show Haskell = "Haskell"
  show PlainText = "Text"
  show Markdown = "Markdown"
  show C = "C"
  show Cpp = "C++"
  show Hasqtan = "Hasqtan"
  show Java = "Java"
  show OCaml = "OCaml"
  show (Unknown other) = other

-- | Possible modes for Juo.
--
-- __Note:__
--
-- Will most likely remove 'Message' when buffers are introduced.
data Mode = Normal | Insert | Select | Command | Message
  deriving (Eq)

instance Show Mode where
  show Normal = ""
  show Insert = " INS "
  show Select = " SEL "
  show Command = " EX "
  show Message = " MSG "

data Action = Inserted | Deleted
  deriving (Eq)

instance Show Action where
  show Inserted = "Insert"
  show Deleted = "Delete"

data DocumentLine = DocumentLine
  { lineContent :: String,
    lineLength :: Int
  }
  deriving (Eq)

data Window = Window
  { win :: HC.Window,
    size :: (Int, Int),
    height :: Int,
    width :: Int
  }
  deriving (Eq)

data EditorSection 
  = Background
  | EditorBackground
  | Toolbar
  | LeftColumn
  deriving (Eq)

-- | height, width, y offset and x offset
--
-- Arguments:
--
-- *@h@ - Height
--
-- Returns a new `Juo` window.
newWindow :: Int -> Int -> Int -> Int -> IO Juo.Window
newWindow h w yOff xOff = do
  newWin <- HC.newWin h w yOff xOff
  return
    Juo.Window
      { win = newWin,
        size = (h - yOff, w - xOff),
        height = h,
        width = w
      }

mvCursor :: Window -> Int -> Int -> IO ()
mvCursor Window {..} y x =
  HC.wMove win y x

data File = File 
  {
    fileContent :: [DocumentLine],
    fileType :: FileType,
    fileLength :: Int,
    filePath :: String
  }
  deriving (Eq)

data Juo = Juo
  { cy :: Int,
    cx :: Int,
    dy :: Int,
    dx :: Int,

    multBuf :: String, -- Tracks numbers
    commandBuf :: String, -- Tracks letter commands

    lastCharPressed :: Maybe HC.Key,
    editorWindow :: Juo.Window,
    toolbarWindow :: Juo.Window,
    windowSize :: (Int, Int),
    currentFile :: File,
    currentLineLength :: Int,
    editedDocument :: Bool,
    mode :: Mode,
    messageBuf :: String,
    fullMessageBuf :: [DocumentLine],
    vertOffset :: Int,
    horizOffset :: Int
  }
  deriving (Eq)

getDX :: Juo -> Int
getDX juo = dx juo

getDY :: Juo -> Int
getDY juo = dy juo

getCX :: Juo -> Int
getCX juo = cx juo

getCY :: Juo -> Int
getCY juo = cy juo

class FileDetails a where
  getFileLength :: a -> Int
  getFileType :: a -> FileType
  getFileContent :: a -> [DocumentLine]
  getFilePath :: a -> String

  setFileLength :: a -> Int -> a
  setFileContent :: a -> [DocumentLine] -> a

instance FileDetails File where
  getFileLength = fileLength
  getFileType = fileType
  getFileContent = fileContent
  getFilePath = filePath

  setFileLength = \file len -> file { fileLength = len }
  setFileContent = \file content -> file { fileContent = content,
    fileLength = length content
  }

instance FileDetails Juo where
  getFileLength = getFileLength . currentFile
  getFileType = getFileType . currentFile
  getFileContent = getFileContent . currentFile
  getFilePath = getFilePath . currentFile

  setFileLength = \juo len ->
    juo { currentFile = setFileLength (currentFile juo) len }
  setFileContent = \juo content ->
    juo { currentFile = setFileContent (currentFile juo) content }

newJuo :: Maybe File -> IO Juo
newJuo maybeFile = do

  let dummyOffsetX = 2

  (winH, winW) <- HC.scrSize
  _toolbarWindow <- newWindow 2 winW (winH - 2) 0
  _editorWindow <- newWindow (winH - height _toolbarWindow) winW 0 dummyOffsetX

  HC.keypad (win _editorWindow) True
  HC.noDelay (win _editorWindow) True
  HC.keypad (win _toolbarWindow) True
  HC.noDelay (win _toolbarWindow) True

  let juo = Juo { cy = 0,
    cx = 0,
    dy = 0,
    dx = 0,
    editorWindow = _editorWindow,
    toolbarWindow = _toolbarWindow,
    windowSize = (winH, winW),
    multBuf = "",
    commandBuf = "",
    mode = Juo.Normal,
    messageBuf = "",
    fullMessageBuf = [DocumentLine "" 0],
    editedDocument = False,
    vertOffset = 0,
    horizOffset = 0
  }

  case maybeFile of
    Nothing -> return juo
    Just file -> return juo {
      currentFile = file,
      currentLineLength = 
        let cnt = getFileContent file in
            if null cnt then
                0
            else
                lineLength (head cnt)
    }

data Direction = Up | Down | Left | Right

exitJuo :: IO ()
exitJuo = do
  -- CUR.endWin
  HCH.end
  callCommand "stty sane"
  unsetEnv "ESCDELAY"

updateCursor :: Juo -> IO Juo
updateCursor juo = do
  let dx = getDX juo
      text = getFileContent juo
      currLine =
        if null text then DocumentLine "" 0
        else text !! getDY juo
      numOfTabsBefore =
        length $ filter (== '\t') (take (dx + 1) (lineContent currLine))

  {-if x > lineLen || currentMode /= Juo.Insert
      then juo { cursorPos = (y, lineLen)
              , documentPos = (line, lineLen)
              }
      else juo-}

  

  -- Move based on # of tabs
  mvCursor (editorWindow juo) (getCY juo) ((getCX juo) + (numOfTabsBefore * 3))


  return juo

moveCursor :: Juo -> Direction -> Int -> Juo
moveCursor juo dir dist
  | dist <= 0 = juo
  | otherwise =
    let juo' = moveCursorOnce juo dir
    in if juo' == juo
      then juo
      else moveCursor juo' dir (dist - 1)

moveCursorOnce :: Juo -> Direction -> Juo
moveCursorOnce juo dir =
  let (winH, winW) = size (editorWindow juo)
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
        Juo.Up
          | line > 0 && cy juo > 0 ->
              let prevLine = line - 1
                  nextLength = lineLength (ls !! prevLine)
                  newDx = clampEnd col nextLength
                  (newCx, newHoriz) = computeVisible newDx (horizOffset juo)
               in juo
                    { cy = cy juo - 1,
                      cx = newCx,
                      dy = prevLine,
                      dx = newDx,
                      horizOffset = newHoriz,
                      currentLineLength = nextLength
                    }
          -- Scroll up viewport
          | line > 0 && cy juo == 0 ->
              let prevLine = line - 1
                  nextLength = lineLength (ls !! prevLine)
                  newDx = clampEnd col nextLength
                  (newCx, newHoriz) = computeVisible newDx (horizOffset juo)
               in juo
                    { cx = newCx,
                      dy = prevLine,
                      dx = newDx,
                      horizOffset = newHoriz,
                      vertOffset = vertOffset juo - 1,
                      currentLineLength = nextLength
                    }
          | otherwise -> juo
        Juo.Down
          | line + 1 < totalLines && cy juo + 1 < winH ->
              let nextLine = line + 1
                  nextLength = lineLength (ls !! nextLine)
                  dx' = clampEnd col nextLength
                  (cx', horizOffset') = computeVisible dx' (horizOffset juo)
              in juo
                    { cy = cy juo + 1,
                      cx = cx',
                      dy = nextLine,
                      dx = dx',
                      horizOffset = horizOffset',
                      currentLineLength = nextLength
                    }
          -- Scroll viewport down
          | line + 1 < totalLines && cy juo + 1 == winH ->
              let nextLine = line + 1
                  nextLength = lineLength (ls !! nextLine)
                  dx' = clampEnd col nextLength
                  (cx', horizOffset') = computeVisible dx' (horizOffset juo)
              in juo
                { dy = nextLine,
                  dx = dx',
                  cx = cx',
                  horizOffset = horizOffset',
                  vertOffset = vertOffset juo + 1,
                  currentLineLength = nextLength
                }

          | otherwise -> juo
        Juo.Left
          | cx juo > 0 -> 
            juo
              { cx = cx juo - 1,
                dx = col - 1
              }
          | cx juo == 0 && dx juo > 0 ->
            juo
              { dx = dx juo - 1,
                horizOffset = (horizOffset juo) - 1
              }
          | otherwise -> juo
        Juo.Right
          -- Cursor moves ahead one while inserting.
          | mode juo == Juo.Insert -> do
            if cx juo + 2 > winW then
                juo
                  { dx = dx juo + 1,
                    horizOffset = (horizOffset juo) + 1
                  }
              else 
                juo
                  { cx = cx juo + 1,
                    dx = col + 1
                  }
          
          | cx juo + 1 < winW && col + 1 < (currentLineLength juo) -> do    
            juo
              { cx = cx juo + 1,
                dx = col + 1
              }
          
          | col + 1 < (currentLineLength juo) -> do
            juo
              { dx = dx juo + 1,
                horizOffset = (horizOffset juo) + 1
              }
          | otherwise -> juo

findDiff :: String -> String -> [(Char, Char, Int)]
findDiff s1 s2 =
  [ (c1, c2, i)
    | (c1, c2, i) <- zip3 s1 s2 [0 ..],
      c1 /= c2
  ]

insertNewMessage :: Juo -> String -> Juo
insertNewMessage juo msg =
  let newMessages = (fullMessageBuf juo) ++ [DocumentLine msg (length msg)]
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

    HC.delWin (win (editorWindow juo))
    HC.delWin (win (toolbarWindow juo))

    HC.wRefresh HC.stdScr
    HC.wRefresh (win _editorWindow)
    HC.wRefresh (win _toolbarWindow)

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

-- Generalized: apply a transformation to the line at cursor
doAtCursor :: Juo -> Action -> (String -> Int -> String) -> Juo
doAtCursor juo action f =
  let newContent = f content col
      newLine =
        currLine
          { lineContent = newContent,
            lineLength = getLineLength newContent
          }

      newLines = take line text ++ [newLine] ++ drop (line + 1) text

      differences = findDiff content newContent
   in setFileContent (juo { fullMessageBuf = if null differences then fullMessageBuf juo else newMessages }) newLines
  where
    line = dy juo
    col = dx juo
    text = getFileContent juo
    currLine = if null text then (DocumentLine "" 0) else text !! line
    content = lineContent currLine
    newMessage = (show action) ++ " `" ++ "` @ " ++ (show line) ++ ":" ++ (show col)
    newMessages = (fullMessageBuf juo) ++ [DocumentLine newMessage (length newMessage)]

deleteChar :: Juo -> Juo
deleteChar juo = do
  let line = dy juo
      col = dx juo
      currLine = if null text then (DocumentLine "" 0) else text !! line
      juo' = doAtCursor juo Deleted $ \content col ->
        take col content ++ drop (col + 1) content

      text = getFileContent juo
  if (col + 1) == lineLength currLine
    then
      moveCursor juo' Juo.Left 1
    else juo'

newLine :: Juo -> Juo
newLine juo = do
  let newLine =
        DocumentLine
          { lineContent = "",
            lineLength = 0
          }
      newLines = take (line + 1) text ++ [newLine] ++ drop (line + 1) text
   in moveCursor
        (setFileContent (juo { mode = Juo.Insert }) newLines)
        Juo.Down
        1
  where
    line = dy juo
    text = getFileContent juo

newLinePush :: Juo -> Juo
newLinePush juo = do
  let oldContent = take col (lineContent currLine)
      newContent = drop col (lineContent currLine)
      oldLine =
        DocumentLine
          { lineContent = oldContent,
            lineLength = getLineLength oldContent
          }
      newLine =
        DocumentLine
          { lineContent = newContent,
            lineLength = getLineLength newContent
          }
      newLines = take line text ++ [oldLine] ++ [newLine] ++ drop (line + 1) text
   in setFileContent (juo { cx = 0, mode = Juo.Insert }) newLines
  where
    line = dy juo
    col = dx juo
    text = getFileContent juo
    currLine = if null text then DocumentLine "" 0 else text !! line

deleteLine :: Juo -> Juo
deleteLine juo = do
  let newLines = take line text ++ drop (line + 1) text
      _juo = setFileContent juo newLines

  if line == getFileLength _juo
    then
      moveCursor _juo Juo.Up 1
    else
      _juo
  where
    line = dy juo
    text = getFileContent juo

-- TODO not needed
deleteCharBefore :: Juo -> Juo
deleteCharBefore juo = do
  let juo' = doAtCursor juo Deleted $ \content col ->
        take (col - 1) content ++ drop col content

      line = dy juo
      text = getFileContent juo
      currLine = if null text then (DocumentLine "" 0) else text !! line
  moveCursor juo' Juo.Left 1

insertChar :: Juo -> Char -> Juo
insertChar juo ch =
  let juo' = doAtCursor juo Inserted $ \content col ->
        take col content ++ [ch] ++ drop col content
   in moveCursor juo' Juo.Right 1

insertTab :: Juo -> Juo
insertTab juo =
  let juo' = doAtCursor juo Inserted $ \content col ->
        take col content ++ replicate 4 ' ' ++ drop col content
   in moveCursor juo' Juo.Right 1

appendChar :: Juo -> Juo
appendChar juo =
  moveCursor juo {mode = Juo.Insert} Juo.Right 1

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
              { mode = Juo.Normal
              }
      return (juo', True)
    _ -> do
      let output = take winW (Hasq.interp (messageBuf juo))
          juo' =
            juo
              { messageBuf = output,
                mode = Juo.Normal
              }
      return (juo', True)

getFileSize :: Juo -> Int
getFileSize juo = sum (map len (getFileContent juo))
  where
    len docLine = (lineLength docLine) + 1 -- Add 1 for '\n'

saveFile :: Juo -> IO Juo
saveFile juo = do
  -- TODO Might be better to chunk write
  res <-
    try (writeFile (getFilePath juo) (unlines (map lineContent (getFileContent juo)))) ::
      IO (Either IOException ())

  let buf = case res of
        Prelude.Left _ -> (show (getFilePath juo) ++ show (getFileSize juo) ++ "B failed to write")
        Prelude.Right _ -> do
          (show (getFilePath juo) ++ " " ++ show (getFileLength juo) ++ "L, " ++ show (getFileSize juo) ++ "B written")

  return
    juo
      { messageBuf = buf,
        mode = Juo.Normal
      }
  where
    (winH, _) = windowSize juo


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

  HC.initPair (HC.Pair 8) (HC.Color 32) (HC.Color 69)

  -- Mode
  HC.initPair (HC.Pair 3) (HC.Color 69) (HC.Color 44)

  -- Toolbar
  HC.initPair (HC.Pair 5) (HC.Color 33) (HC.Color 72)

  -- Editor
  HC.initPair (HC.Pair 6) (HC.Color 33) (HC.Color 75)

  HC.initPair (HC.Pair 7) (HC.Color 33) (HC.Color 76)


-- | A monolithic function that handles normal mode interactions
handleNormalMode :: Juo -> Config -> HC.Key -> IO (Juo, Bool)
handleNormalMode juo conf ch = do
  let l = dy juo
      c = dx juo
      mult = if null (multBuf juo) then 1 else read (multBuf juo)

  case ch of
    HC.KeyChar c

      | c == (delete conf) -> do
          let _juo = deleteChar juo
          return (_juo, True)

      | c `elem` ['1' .. '9'] -> do return (juo { multBuf = multBuf juo ++ [c]}, True)

      -- HACK: Really lazy on my part :(
      | c == '0' -> 
        if null (multBuf juo) 
          then
            return ((moveCursor juo Juo.Left 999999), True)
          else
            return (juo { multBuf = multBuf juo ++ [c]}, True)
      | c == '$' -> return ((moveCursor juo Juo.Right 999999), True)

      | c == 'G' -> return ((moveCursor juo Juo.Down 999999), True)

      | c `elem` ['d', 'g'] -> do 
        case c of
          'd' -> 
            setUnderscoreCursor
          _ ->
            setBlockCursor

        return (juo {commandBuf = commandBuf juo ++ [c]}, True)
        
      -- NOTE: SWITCH MODE
      | c == 'i' -> do
          let _juo =
                juo
                  { mode = Juo.Insert
                  }
          return (_juo, True)
      | c == 'a' -> return ((appendChar juo), True)
      | c == 'o' -> return ((newLine juo), True)
      | c == 's' -> do
          --HC.wAttrSet HC.stdScr (CUR.attr0, (CUR.Pair 3))

          let _juo =
                juo
                  { mode = Juo.Select
                  }
          return (_juo, True)
      | c == 'm' -> do
          let _juo =
                juo
                  { mode = Juo.Message
                  }
          return (_juo, True)
      
      | c == ':' -> do
          return (juo
                  { mode = Juo.Command,
                    messageBuf = ""
                  }, 
                  True)
      | c == ';' -> do
          return (juo
                  { mode = Juo.Command,
                    messageBuf = ""
                  }, 
                  True)

      -- NOTE: NORMAL MODE SHORTCUTS

      | c == 'r' -> do
          let _juo =
                juo
                  { mode = Juo.Normal
                  }
          return (_juo, True)
      -- TODO Could add these to command buffer but doesnt seem to be necessary
      | c == (up conf) ->
        return ((moveCursor (juo { commandBuf = [c], multBuf = ""}) Juo.Up mult), True)
      | c == down conf -> 
        return ((moveCursor (juo { commandBuf = [c], multBuf = ""}) Juo.Down mult), True)
      | c == (left conf) -> 
        return ((moveCursor (juo { commandBuf = [c], multBuf = ""}) Juo.Left mult), True)
      | c == (right conf) -> 
        return ((moveCursor (juo { commandBuf = [c], multBuf = ""}) Juo.Right mult), True)

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
    HC.KeyUp -> return ((moveCursor juo Juo.Up (scrollDistance conf)), True)
    HC.KeyDown -> return ((moveCursor juo Juo.Down (scrollDistance conf)), True)
    {-HC.KeyLeft ->
          if (mult juo) == "d"
            then do
              let _juo = juo {mult = ""}
              setBlockCursor
              return (deleteChar (moveCursor _juo Juo.Left 1), True)
            else
              return ((moveCursor juo Juo.Left 1), True)
    HC.KeyRight ->
          if (mult juo) == "d"
            then do
              let _juo = juo {mult = ""}
              setBlockCursor
              return (deleteChar (moveCursor _juo Juo.Right 1), True)
            else
              return ((moveCursor juo Juo.Right 1), True)-}

    key -> return (insertNewMessage juo (cursesKeyErrMsg key), True)

  where
    keyboardCharErrMsg c = ("Unrecognized input: _" ++ (show c) ++ "_")
    cursesKeyErrMsg key = ("Unrecognized input: _" ++ (show key) ++ "_")
