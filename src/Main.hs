module Main (main) where

import Juo
import Juo.Config
import Juo.Draw
import Juo.Util

import qualified UI.HSCurses.Curses as CUR
import qualified UI.HSCurses.CursesHelper as CURHELP
--import System.IO
import System.Environment (getArgs, getProgName, setEnv, unsetEnv)
--import System.Directory
import System.FilePath
import System.Process (callCommand)
import System.Exit (exitSuccess, exitFailure)
import System.Posix.Signals
import Control.Concurrent
import Data.Char
--import System.Console.ANSI
import Control.Monad (when)

type FileName = String
type FullText = String


editorLoop :: [String] -> IO ()
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
printArgs (arg:args) = putStrLn arg

openThisFile :: String -> IO ()
openThisFile filename = do
    content <- readFile filename
    putStrLn $ content

updateLastChar :: Juo -> Char -> Juo
updateLastChar juo ch = juo { lastCharPressed = Just ch }

startJuo :: Juo -> Config -> IO ()
startJuo juo settings = do

    let dummyOffsetX = 3
        offsetY = 2
        
    (winH, winW) <- CUR.scrSize
    newWin <- CUR.newWin (winH - offsetY) (winW - dummyOffsetX) 0 dummyOffsetX
    let _editorWindow = Juo.Window {
        win = newWin
        , size = ((winH - offsetY), (winW - dummyOffsetX))
    }
    newWin <- CUR.newWin 2 (winW + 1) (winH - 2) 0
    let _toolbarWindow = Juo.Window {
        win = newWin
        , size = ((winW + 1), (winH - 2))
    }

    --CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 1))

    --let bkgrndColor  = CUR.attrPlus (CURHELP.ForegroundColor CURHELP.RedF) (CURHELP.BackgroundColor CURHELP.DarkBlueB)
    --CUR.attrBoldOn

    --let attr = CUR.setBold CUR.attr0 True

    CUR.bkgrndSet CUR.attr0 (CUR.Pair 2)

    CUR.keypad CUR.stdScr True
    CUR.noDelay CUR.stdScr True
    {-CUR.keypad _editorWindow True
    CUR.noDelay _editorWindow True
    CUR.keypad _toolbarWindow True
    CUR.noDelay _toolbarWindow True-}

    CUR.echo False

    let _juo = juo { cursorPos = (0, 0)
    , documentPos = (0, 0)
    , editorWindow = _editorWindow
    , toolbarWindow = _toolbarWindow
    , windowSize = (winH, winW)
    , mult = ""
    , shortcutBuf = ""
    , currentLineLength = lineLength ((document juo) !! 0)
    , mode = Juo.Normal
    , messageBuf = ""
    , fullMessageBuf = [DocumentLine "" 0]
    , editedDocument = False
    , topOffset = 0
    }

    loopJuo _juo settings

resizeEditor :: Juo -> IO (Juo, Bool)
resizeEditor juo = do
    (winH, winW) <- CUR.scrSize

    CUR.resizeTerminal winH winW
    CUR.wclear CUR.stdScr

    let dummyOffsetX = 3
        offsetY = 2

    newWin <- CUR.newWin (winH - offsetY) (winW - dummyOffsetX) 0 dummyOffsetX
    let _editorWindow = Juo.Window {
        win = newWin
        , size = (winH - offsetY, winW - dummyOffsetX)
    }
    newWin <- CUR.newWin 2 (winW + 1) (winH - 2) 0
    let _toolbarWindow = Juo.Window {
        win = newWin
        , size = (2, winW + 1)
    }

    CUR.delWin (win (editorWindow juo))
    CUR.delWin (win (toolbarWindow juo))

    CUR.wRefresh CUR.stdScr
    CUR.wRefresh (win _editorWindow)
    CUR.wRefresh (win _toolbarWindow)

    let _juo = juo { windowSize = (winH, winW)
    , editorWindow = _editorWindow
    , toolbarWindow = _toolbarWindow }
    
    CUR.bkgrndSet CUR.attr0 (CUR.Pair 2)

    return (_juo, True)

loopJuo :: Juo -> Config -> IO ()
loopJuo juo settings = do

    let (winH, winW) = windowSize juo
        (l, c) = documentPos juo
        (y, x) = cursorPos juo

    CUR.cursSet CUR.CursorInvisible

    -- Main window
    CUR.werase CUR.stdScr

    -- Editor window
    CUR.werase (win (editorWindow juo))
    case mode juo of
        Juo.Insert  -> do
            paintContent juo settings (document juo)
            setBarCursor

        Juo.Message -> paintContent juo settings (fullMessageBuf juo)
        _           -> paintContent juo settings (document juo)

    -- NOTE: Always update cursor *after* drawing
    updateCursor juo

    CUR.werase (win (toolbarWindow juo))
    drawToolbar juo settings

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
                    Juo.Normal  -> juo { mult = [] }
                    Juo.Command -> juo { messageBuf = "", mode = Juo.Normal }
                    Juo.Insert  -> do 
                        let _juo = juo { mode = Juo.Normal } in
                            if (lastCharPressed juo) == Just 'a' 
                                then
                                    (moveCursor _juo Juo.Left 1)
                                else 
                                    _juo
                    _           -> juo { mode = Juo.Normal }
            return (_juo, True)

        -- Called upon resize event
        (_, CUR.KeyResize) -> do
             resizeEditor juo

        -- NOTE: NORMAL MODE BINDINGS
        (Juo.Normal, CUR.KeyChar ch) -> do
            (_juo, cont) <- case ch of 
                _   | ch == (delete settings) -> do
                        let _juo = deleteChar juo in
                            return (_juo, True)

                    | ch `elem` ['1'..'9'] ->
                        let _juo = updateMult juo ch in
                        return (_juo, True)

                    | ch == '0' ->
                        let _juo = juo { cursorPos = ((y),0)
                        , documentPos = (l, 0)
                        } in
                            return (_juo, True)
                    | ch == '$' ->
                        let newCol = if (currentLineLength juo) > 0
                            then
                                (currentLineLength juo) - 1
                            else 0
                            in
                                let _juo = juo { cursorPos = ((y)
                                , newCol)
                                , documentPos = (l, newCol)
                                } in
                                    return (_juo, True)

                    | ch == 'd' -> do
                        let _juo = updateMult juo ch
                        return (_juo, True)

                    -- NOTE: SWITCH MODE
                    | ch == 'i' -> do
                        let _juo = juo { mode = Juo.Insert
                        } in
                            return (_juo, True)

                    | ch == 'a' -> return ((appendChar juo), True)

                    | ch == 'o' -> return ((newLine juo), True)


                    | ch == 's' -> do
                        CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 3))

                        let _juo = juo { mode = Juo.Select
                        } in
                            return (_juo, True)


                    | ch == 'm' -> do
                        let _juo = juo { mode = Juo.Message
                        } in
                            return (_juo, True)


                    | ch == ';' ->
                        let _juo = juo { mode = Juo.Command
                        , messageBuf = ""
                        } in
                            return (_juo, True)

                    
                    -- NOTE: NORMAL MODE SHORTCUTS
                    | ch == 'r' -> do
                        let _juo = juo { mode = Juo.Normal
                        } in
                            return (_juo, True)

                    | ch == (up settings) ->
                        if (mult juo) == "d"
                            then do
                                let _juo = juo { mult = "" }
                                setBlockCursor
                                return (deleteLine (moveCursor _juo Juo.Up 1), True)
                            else
                                return ((moveCursor juo Juo.Up 1), True)
                    | ch == down settings ->
                        if (mult juo) == "d"
                            then do
                                let _juo = juo { mult = "" }
                                setBlockCursor
                                return (deleteLine (deleteLine _juo), True)
                            else
                                return ((moveCursor juo Juo.Down 1), True)
                    | ch == (left settings) -> 
                        if (mult juo) == "d"
                            then do
                                let _juo = juo { mult = "" }
                                setBlockCursor
                                return (deleteChar (moveCursor _juo Juo.Left 1), True)
                            else
                                return ((moveCursor juo Juo.Left 1), True)
                    | ch == (right settings) -> 
                        if (mult juo) == "d"
                            then do
                                let _juo = juo { mult = "" }
                                setBlockCursor
                                return (deleteChar (moveCursor _juo Juo.Right 1), True)
                            else
                                return ((moveCursor juo Juo.Right 1), True)

                    | otherwise -> return (juo, True)
            return (updateLastChar _juo ch, cont)

        -- NOTE: SELECT MODE BINDINGS
        (Juo.Select, _) -> return (juo, True)
                
        -- NOTE: INSERT MODE BINDINGS
        (Juo.Insert, CUR.KeyChar ch)
            -- TODO This needs to be implemented properly
            | ch == '\n' || ch == '\r' -> do
                let _juo = moveCursor (newLinePush juo) Juo.Down 1 in
                    return (_juo, True)
            | ch == '\DEL' ->
                let _juo = deleteCharBefore juo in
                    return (_juo, True)
            | ch == '\t'   ->
                return (Juo.insertChar juo '\t', True)

        (Juo.Insert, CUR.KeyBackspace) ->
            let _juo = deleteCharBefore juo in
                return (_juo, True)

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
            let _juo = juo { 
                messageBuf = if null (messageBuf juo) then "" else (init (messageBuf juo))
            } in
                return (_juo, True)

        (Juo.Normal, _) -> return (juo, True)

        _ -> return (juo, False)

    juo <- case (mult juo) of
        "d" -> do
            setUnderscoreCursor
            return juo
        "dd" -> do
            let _juo = juo { mult = "" }
            setBlockCursor
            return (deleteLine _juo)
        _ -> do 
            return juo
    
    when shouldContinue (loopJuo juo settings)
            
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

_parseLine :: [String] -> [DocumentLine]
_parseLine = map (\l -> DocumentLine l (length l))

parseDocument :: FileName -> FullText -> Juo
parseDocument filename text =
    let fileType = case takeExtension filename of
            ".hs"   -> Juo.Haskell
            ".lhs"  -> Juo.Haskell
            ".txt"  -> Juo.PlainText
            ".md"   -> Juo.Markdown
            ".c"    -> Juo.C
            ".cpp"  -> Juo.Cpp
            ".c++"  -> Juo.Cpp
            ".cc"   -> Juo.Cpp
            ".java" -> Juo.Java
            ".ml"   -> Juo.OCaml
            ".mli"  -> Juo.OCaml
            ext     -> Juo.Unknown ext

        ls = lines text
        ds = _parseLine ls
    in Juo
        { document       = ds
        , documentLength = length ds
        , documentType   = fileType
        , filePath       = filename
        }

newColor :: CUR.Color -> (Int, Int, Int) -> IO ()
newColor colorNum (r, g, b) =
    let r' = round ((fromIntegral r) * ratio)
        g' = round ((fromIntegral g) * ratio)
        b' = round ((fromIntegral b) * ratio)
    in
        CUR.initColor colorNum (r', g', b')
    where
        ratio :: Double
        ratio = 3.92156862745


main :: IO ()
main = do
    done <- newEmptyMVar
    let handler = do
            --putStrLn ""
            --putStrLn "interrupt"
            --CUR.cursSet CUR.CursorVisible
            putMVar done ()
            exitSuccess
    
    _ <- installHandler keyboardSignal (Catch handler) Nothing
    --takeMVar done
    --putStrLn "exiting"

    args <- getArgs
    if length args /= 1 then
        do
            p <- getProgName
            putStrLn ("Usage: " ++ p ++ " <filename>")
            exitFailure
    else
        do
            setEnv "ESCDELAY" "0"

            --CUR.initCurses
            CURHELP.start
            setBlockCursor
            has256Color <- CUR.hasColors

            if not has256Color then do
                putStrLn "Requires full color support (i.e. xterm-256)"
                exitSuccess
            else do

                CUR.startColor


                newColor (CUR.Color 1) (toolbarBgColor settings)

                CUR.initPair (CUR.Pair 1) CURHELP.black (CUR.Color 1)


                --CUR.initPair (CUR.Pair 1) CURHELP.white CURHELP.defaultColor

                newColor (CUR.Color 33) (255, 255, 255)

                newColor (CUR.Color 32) (85, 85, 85)

                newColor (CUR.Color 44) (57, 255, 20)

                newColor (CUR.Color 69) (18, 18, 18)
                newColor (CUR.Color 72) (12, 12, 12)


                CUR.initPair (CUR.Pair 4) CURHELP.white CURHELP.blue

                CUR.initPair (CUR.Pair 2) (CUR.Color 33) (CUR.Color 69)

                CUR.initPair (CUR.Pair 8) (CUR.Color 32) (CUR.Color 69)

                -- Mode 
                CUR.initPair (CUR.Pair 3) (CUR.Color 69) (CUR.Color 44)

                -- Toolbar
                CUR.initPair (CUR.Pair 5) (CUR.Color 33) (CUR.Color 72)


                let filename = head args
                text <- readFile filename


                startJuo (parseDocument filename text) settings


            

            --CUR.endWin
            CURHELP.end
            callCommand "stty sane"
            unsetEnv "ESCDELAY"


    --isFile <- doesFileExist $ head args

    --if isFile then
    --    let filename = head args in
    --    openThisFile filename
    --else
    --    putStrLn "Salutations my lord, your squire at your service."
        -- putStrLn "Enter a file name:"
    -- fname <- getLine
    -- content <- readFile fname
    -- putStrLn "\nFile contents:\n"
    -- editorLoop (lines content)
    exitSuccess

    where
        settings = Config {
                up              = 'k',
                down            = 'j',
                left            = 'h',
                right           = 'l',
                delete          = 'x',

                cursorCmd       = ':',
                
                showLineNumbers = False,

                tabDepth        = 4,

                toolbarBgColor  = (255, 255, 255)
            }
