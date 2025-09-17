module Main (main) where

import Juo
import Juo.Config
import Juo.Draw
import Juo.Util
import qualified Juo.Types as JT

import Data.Stack as DS

import Control.Concurrent
import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Char
import System.Environment (getArgs, getProgName, setEnv, unsetEnv)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO (readFile)
import System.Posix.Signals
import System.Process (callCommand)
import qualified UI.HSCurses.Curses as CUR
import qualified UI.HSCurses.CursesHelper as CURHELP

type FileName = String
type FullText = String

{-editorLoop :: [String] -> IO ()
editorLoop content = do
    putStrLn "\nCommands: ';w' to write, ';q' to quit, or type to add line."
    line <- getLine
    case line of
        ";q" -> putStrLn "Exiting."
        ";w" -> do
            putStrLn "Enter filename to save:"
            filename <- getLine
            writeFile filename (unlines content)
            putStrLn "File saved."
            editorLoop content
        _ -> editorLoop (content ++ [line])

printArgs :: [String] -> IO ()
printArgs [] = putStr "\n"
printArgs (arg:args) = putStrLn arg-}

updateLastChar :: Juo -> CUR.Key -> Juo
updateLastChar juo ch =
  juo
    { lastCharPressed = Just ch
    }

parseFile :: FileName -> FullText -> JT.File
parseFile filename text =
  let fileType = case takeExtension filename of
        ".hs" -> JT.Haskell
        ".lhs" -> JT.Haskell
        ".txt" -> JT.PlainText
        ".md" -> JT.Markdown
        ".c" -> JT.C
        ".cpp" -> JT.Cpp
        ".c++" -> JT.Cpp
        ".cc" -> JT.Cpp
        ".java" -> JT.Java
        ".ml" -> JT.OCaml
        ".mli" -> JT.OCaml
        ext -> JT.Unknown ext

      ls = lines text
      ds = parseLine ls
   in JT.File
        { JT.fileContent = ds,
          JT.fileLength = length ds,
          JT.fileType = fileType,
          JT.filePath = filename
        }
  where
    parseLine :: [String] -> [JT.DocumentLine]
    parseLine = map (\l -> JT.DocumentLine l (length l))

startJuo :: FileName -> FullText -> Config -> IO ()
startJuo filename content settings = do
  -- CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 1))

  -- let bkgrndColor  = CUR.attrPlus (CURHELP.ForegroundColor CURHELP.RedF) (CURHELP.BackgroundColor CURHELP.DarkBlueB)
  -- CUR.attrBoldOn

  -- let attr = CUR.setBold CUR.attr0 True

  CUR.bkgrndSet CUR.attr0 (CUR.Pair 8)

  CUR.keypad CUR.stdScr True
  CUR.noDelay CUR.stdScr True


  CUR.echo False

  let file = parseFile filename content

  juo <- newJuo (Just file)

  loopJuo juo settings

loopJuo :: Juo -> Config -> IO ()
loopJuo juo conf = do
  -- Main window
  CUR.werase CUR.stdScr

  -- Editor window
  CUR.werase (JT.win (editorWindow juo))
  case mode juo of
    JT.Insert -> do
      paintContent juo conf (getFileContent juo)
      setBarCursor
    JT.Message -> paintContent juo conf (fullMessageBuf juo)
    _ -> paintContent juo conf (getFileContent juo)

  -- NOTE: Always update cursor *after* drawing
  juo <- updateCursor juo conf

  CUR.werase (JT.win (toolbarWindow juo))
  drawToolbar juo conf

  CUR.wnoutRefresh CUR.stdScr
  CUR.wnoutRefresh (JT.win (toolbarWindow juo))
  CUR.wnoutRefresh (JT.win (editorWindow juo))

  CUR.update

  c <- CUR.getCh

  (juo, shouldContinue) <- case (mode juo, c) of
    (_, CUR.KeyChar '\ESC') -> do
      CUR.wAttrSet (JT.win (editorWindow juo)) (CUR.attr0, CUR.Pair 2)
      setBlockCursor

      let _juo = case mode juo of
            JT.Normal -> juo {multBuf = 0, commandBuf = []}
            JT.Command -> juo {messageBuf = "", mode = JT.Normal}
            JT.Insert ->
              if null (insertionBuffer juo)
                then
                  moveCursor juo {mode = JT.Normal} JT.Left 1
                else do
                  let newMessage = show JT.Inserted ++ " `" ++ insertionBuffer juo ++ "` @ " ++ show (dx juo + 1) ++ ":" ++ show (dy juo + 1)
                      _juo = juo {mode = JT.Normal, insertionBuffer = ""}

                  moveCursor (insertNewMessage _juo newMessage) JT.Left 1
            _ -> juo {mode = JT.Normal}
      return (_juo, True)

    -- Called upon resize event
    (_, CUR.KeyResize) -> do
      resizeEditor juo

    (JT.Normal, ch) -> do
      let buf = if null (commandBuf juo)
          then
            ""
          else
            case head (commandBuf juo) of
                c | c `elem` [up conf, down conf, left conf, right conf] ->
                    tail (commandBuf juo)
                  | otherwise -> commandBuf juo

      case buf of
        "d" -> do -- Sub-mode 'Delete'
          let _juo = juo {commandBuf = ""}
          setBlockCursor
          case ch of
            CUR.KeyChar c
              | c == 'd'        -> return (deleteLine _juo, True)
              | c == up conf    -> return (deleteLine (moveCursor _juo JT.Up 1), True)
              | c == down conf  -> return (deleteLine (deleteLine _juo), True)
              | c == left conf  -> return (deleteChar (moveCursor _juo JT.Left 1) Nothing, True)
              | c == right conf -> return (deleteChar (moveCursor _juo JT.Right 1) Nothing, True)
              | otherwise       -> return (_juo, True)
            _ -> return (_juo, True)  -- fallback if not a KeyChar
        "g" -> do -- Sub-mode 'g'
          let _juo = juo {commandBuf = ""}
          case ch of
            CUR.KeyChar c
              | c == 'g'        -> return (moveCursor _juo JT.Up 999999, True)
              | otherwise       -> return (_juo, True)
            _ -> return (_juo, True)  -- fallback if not a KeyChar

        {-_ | Juo.Util.isDigit (multBuf juo) ->
            let CUR.KeyChar c = ch
            in return (juo { multBuf = multBuf juo ++ [c]}, True)-}

        _ -> handleNormalMode (updateLastChar (juo { commandBuf = buf }) ch) conf ch


    -- NOTE: SELECT MODE BINDINGS
    (JT.Select, _) -> return (juo, True)
    -- NOTE: INSERT MODE BINDINGS
    (JT.Insert, CUR.KeyChar ch)
      -- TODO This needs to be implemented properly
      | ch == '\n' || ch == '\r' -> do
          let _juo = moveCursor (newLinePush juo) JT.Down 1
           in return (_juo { insertionBuffer = insertionBuffer juo ++ ['\n']}, True)
      | ch == '\DEL' ->
          let _juo = deleteCharBefore juo
           in return (_juo, True)
      | ch == '\t' ->
          return (Juo.insertChar juo '\t', True)
    (JT.Insert, CUR.KeyBackspace) ->
      let _juo = deleteCharBefore juo
       in return (_juo, True)
    (JT.Insert, CUR.KeyChar ch)
      | isPrint ch -> do
          let _juo = Juo.insertChar juo ch
          return (_juo, True)

    -- NOTE: COMMAND MODE BINDINGS
    (JT.Command, CUR.KeyChar ch)
      | ch == '\n' || ch == '\r' -> do
          (_juo, shouldContinue) <- execCommand juo
          return (_juo, shouldContinue)
    (JT.Command, CUR.KeyChar ch)
      | isPrint ch -> do
          let _juo = Juo.addCharToMessage juo ch
          return (_juo, True)
      | otherwise ->
          return (juo, True)
    (JT.Command, CUR.KeyBackspace) -> do
      let _juo =
            juo
              { messageBuf = if null (messageBuf juo) then "" else init (messageBuf juo)
              }
       in return (_juo, True)

    (_, key) ->
      return (insertNewMessage juo ("Unrecognized input: _" ++ show key ++ "_"), True)

  if not (editedDocument juo) && DS.size undoHistory juo == 0
    then when shouldContinue (loopJuo juo { editedDocument = True } conf)
    else when shouldContinue (loopJuo juo conf)

{-where

        backspace :: IO ()
        backspace =
            if buf == [] then
                loopJuo (Juo (height juo) (width juo) buf (DocumentPosition y x)) y x content
            else
                loopJuo (Juo (height juo) (width juo) (init buf) (DocumentPosition y x)) y x content
            where
                buf = (modifier juo)

        {-_modifier = if (modifier juo) == [] then 0
            else if (modifier juo) == ['1'] then 0
            else read (modifier juo)-}
-}

main :: IO ()
main = do
  done <- newEmptyMVar
  let handler = do
        -- putStrLn ""
        -- putStrLn "interrupt"
        -- CUR.cursSet CUR.CursorVisible
        putMVar done ()
        exitSuccess

  _ <- installHandler keyboardSignal (Catch handler) Nothing
  -- takeMVar done
  -- putStrLn "exiting"

  args <- getArgs
  if length args /= 1
    then do
      p <- getProgName
      putStrLn ("Usage: " ++ p ++ " <filename>")
      exitFailure
    else do
      setEnv "ESCDELAY" "0"

      -- CUR.initCurses
      CURHELP.start
      setBlockCursor
      has256Color <- CUR.hasColors

      if not has256Color
        then do
          putStrLn "Requires full color support (i.e. xterm-256)"
          exitSuccess
        else do
          CUR.startColor

          initColors conf

          let filename = head args

          res <- try (readFile filename) :: IO (Either IOException String)

          let content = case res of
                Prelude.Left _ -> "" -- Creating a new file!
                Prelude.Right text -> text

          startJuo filename content conf

      exitJuo

  -- isFile <- doesFileExist $ head args

  -- if isFile then
  --    let filename = head args in
  --    openThisFile filename
  -- else
  --    putStrLn "Salutations my lord, your squire at your service."
  -- putStrLn "Enter a file name:"
  -- fname <- getLine
  -- content <- readFile fname
  -- putStrLn "\nFile contents:\n"
  -- editorLoop (lines content)
  exitSuccess
  where
    conf =
      Config
        { up = 'k',
          down = 'j',
          left = 'h',
          right = 'l',
          delete = 'x',
          cursorCmd = ':',
          showLineNumbers = False,
          tabDepth = 3, -- 0-indexed, so *really* 4
          toolbarBgColor = (255, 255, 255),
          useTerminalColor = True,
          scrollDistance = 4
        }
