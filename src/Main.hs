module Main (main) where

import Juo
import Juo.Config
import Juo.Draw
import Juo.Util

-- import System.IO
-- import System.Directory
import Control.Concurrent
-- import System.Console.ANSI
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

parseFile :: FileName -> FullText -> Juo.File
parseFile filename text =
  let fileType = case takeExtension filename of
        ".hs" -> Juo.Haskell
        ".lhs" -> Juo.Haskell
        ".txt" -> Juo.PlainText
        ".md" -> Juo.Markdown
        ".c" -> Juo.C
        ".cpp" -> Juo.Cpp
        ".c++" -> Juo.Cpp
        ".cc" -> Juo.Cpp
        ".java" -> Juo.Java
        ".ml" -> Juo.OCaml
        ".mli" -> Juo.OCaml
        ext -> Juo.Unknown ext

      ls = lines text
      ds = parseLine ls
   in Juo.File
        { fileContent = ds,
          fileLength = length ds,
          fileType = fileType,
          filePath = filename
        }
  where
    parseLine :: [String] -> [DocumentLine]
    parseLine = map (\l -> DocumentLine l (getLineLength l))

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
  let l = dy juo

  _ <- CUR.cursSet CUR.CursorInvisible

  -- Main window
  CUR.werase CUR.stdScr

  -- Editor window
  CUR.werase (win (editorWindow juo))
  case mode juo of
    Juo.Insert -> do
      paintContent juo conf (getFileContent juo)
      setBarCursor
    Juo.Message -> paintContent juo conf (fullMessageBuf juo)
    _ -> paintContent juo conf (getFileContent juo)

  -- NOTE: Always update cursor *after* drawing
  juo <- updateCursor juo

  CUR.werase (win (toolbarWindow juo))
  drawToolbar juo conf

  CUR.wnoutRefresh CUR.stdScr
  CUR.wnoutRefresh (win (toolbarWindow juo))
  CUR.wnoutRefresh (win (editorWindow juo))

  CUR.update

  CUR.cursSet CUR.CursorVisible

  c <- CUR.getCh

  (juo, shouldContinue) <- case ((mode juo), c) of
    (_, CUR.KeyChar '\ESC') -> do
      CUR.wAttrSet (win (editorWindow juo)) (CUR.attr0, (CUR.Pair 2))
      setBlockCursor

      let _juo = case (mode juo) of
            Juo.Normal -> juo {mult = []}
            Juo.Command -> juo {messageBuf = "", mode = Juo.Normal}
            Juo.Insert -> do
              let _juo = juo {mode = Juo.Normal}
                in
                  (moveCursor _juo Juo.Left 1)
            _ -> juo {mode = Juo.Normal}
      return (_juo, True)

    -- Called upon resize event
    (_, CUR.KeyResize) -> do
      resizeEditor juo

    (Juo.Normal, ch) -> do
      case mult juo of
        "" -> handleNormalMode (updateLastChar juo ch) conf ch
        "d" -> do -- Sub-mode 'Delete'
          let _juo = juo {mult = ""}
          setBlockCursor
          case ch of
            CUR.KeyChar c
              | c == 'd'        -> return (deleteLine _juo, True)
              | c == up conf    -> return (deleteLine (moveCursor _juo Juo.Up 1), True)
              | c == down conf  -> return (deleteLine (deleteLine _juo), True)
              | c == left conf  -> return (deleteChar (moveCursor _juo Juo.Left 1), True)
              | c == right conf -> return (deleteChar (moveCursor _juo Juo.Right 1), True)
              | otherwise       -> return (_juo, True)
            _ -> return (_juo, True)  -- fallback if not a KeyChar
        "g" -> do -- Sub-mode 'g'
          let _juo = juo {mult = ""}
          case ch of
            CUR.KeyChar c
              | c == 'g'        -> return ((moveCursor _juo Juo.Up 999999), True)
              | otherwise       -> return (_juo, True)
            _ -> return (_juo, True)  -- fallback if not a KeyChar


    -- NOTE: SELECT MODE BINDINGS
    (Juo.Select, _) -> return (juo, True)
    -- NOTE: INSERT MODE BINDINGS
    (Juo.Insert, CUR.KeyChar ch)
      -- TODO This needs to be implemented properly
      | ch == '\n' || ch == '\r' -> do
          let _juo = moveCursor (newLinePush juo) Juo.Down 1
           in return (_juo, True)
      | ch == '\DEL' ->
          let _juo = deleteCharBefore juo
           in return (_juo, True)
      | ch == '\t' ->
          return (Juo.insertChar juo '\t', True)
    (Juo.Insert, CUR.KeyBackspace) ->
      let _juo = deleteCharBefore juo
       in return (_juo, True)
    (Juo.Insert, CUR.KeyChar ch)
      | isPrint ch -> do
          let _juo = Juo.insertChar juo ch
          return (_juo, True)

    -- NOTE: COMMAND MODE BINDINGS
    (Juo.Command, CUR.KeyChar ch)
      | ch == '\n' || ch == '\r' -> do
          (_juo, shouldContinue) <- execCommand juo
          return (_juo, shouldContinue)
    (Juo.Command, CUR.KeyChar ch)
      | isPrint ch -> do
          let _juo = Juo.addCharToMessage juo ch
          return (_juo, True)
      | otherwise ->
          return (juo, True)
    (Juo.Command, CUR.KeyBackspace) -> do
      let _juo =
            juo
              { messageBuf = if null (messageBuf juo) then "" else (init (messageBuf juo))
              }
       in return (_juo, True)

    (_, key) -> 
      return ((insertNewMessage juo ("Unrecognized input: _" ++ (show key) ++ "_")), True)

  -- Post interaction changes for next draw loop
  juo <- case (mult juo) of
    "d" -> do
      setUnderscoreCursor
      return juo
    "dd" -> do
      let _juo = juo {mult = ""}
      setBlockCursor
      return (deleteLine _juo)
    _ -> do
      return juo

  when shouldContinue (loopJuo juo conf)

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
          tabDepth = 4,
          toolbarBgColor = (255, 255, 255),
          scrollDistance = 4
        }
