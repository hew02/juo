module Main (main) where

import Juo
import Juo.Settings

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
import System.IO (hFlush, stdout)

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

numDigit :: Integer -> Int
numDigit = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds

printFile :: Int -> Int -> Juo -> IO ()
printFile y h juo = do
    go y h (document juo)
    where
        go _ 0 _  = return ()
        go _ _ [] = return ()
        go row n (line:rest) = do
            Juo.write row 0 ((lineContent line))
            go (row + 1) (n - 1) rest

writeRightSideInfo :: Juo -> IO ()
writeRightSideInfo juo = do
    (winH, winW) <- CUR.scrSize
    let IVec2 winH winW = windowSize juo
        fileTypeStr = show (documentType juo)
        (l, c) = documentPos juo
        lHeight = (winH - 2)
    Juo.write lHeight (winW - (length fileTypeStr) - 2) fileTypeStr
    
    let colStr = show (c + 1)
        lineStr = show (l + 1)


    Juo.write lHeight (winW - (length fileTypeStr) - 3 - (length colStr)) colStr
    Juo.write lHeight (winW - (length fileTypeStr) - 5 - (length colStr)) "|"
    Juo.write lHeight (winW - (length fileTypeStr) - 6 - (length lineStr) - (length colStr)) lineStr

writeMode :: Juo -> IO ()
writeMode juo = do
    let IVec2 winH winW = windowSize juo
        modeStr = case (mode juo) of
            Juo.Normal  -> ""
            Juo.Insert  -> "Ins."
            Juo.Select  -> "Sel."
            Juo.Command -> "Cmd."
    
    CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 3))
    Juo.write (winH - 2) 1 modeStr
    CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 2))


writeMessage :: Juo -> IO ()
writeMessage juo = do
    let IVec2 winH winW = windowSize juo
    Juo.write (winH - 1) 1 (messageBuf juo)



startJuo :: Juo -> UserSettings -> IO ()
startJuo juo settings = do

    (winH, winW) <- CUR.scrSize

    let juo' = juo { cursorPos = (0, 0)
    , documentPos = (0, 0)
    , windowSize = IVec2 winH winW
    , mult = []
    , currentLineLength = lineLength ((document juo) !! 0)
    , mode = Juo.Normal
    , messageBuf = ""
    }

    loopJuo juo' settings

printInsertCursor :: IO ()
printInsertCursor = do
    putStr "\ESC[6 q"
    hFlush stdout

loopJuo :: Juo -> UserSettings -> IO ()
loopJuo juo settings = do

    let IVec2 winH winW = windowSize juo
        (l, c) = documentPos juo
        (y, x) = cursorPos juo

    CUR.erase -- clear curses's virtual screen but don't force a redraw

    printFile 0 winH juo
    
    multString <- getMult juo
    Juo.write (winH - 1) (winW - 8) multString

    writeRightSideInfo juo
    writeMode juo
    writeMessage juo

    when ((mode juo) == Juo.Insert) printInsertCursor
    

    -- NOTE: Always update cursor last!
    updateCursor juo
        
    CUR.wRefresh CUR.stdScr -- copy the virtual screen to the terminal
    
    c <- CUR.getCh

    (juo', shouldContinue) <- case ((mode juo), c) of



        (_, CUR.KeyChar '\ESC') -> do
            CUR.cursSet CUR.CursorVisible
            CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 2))
            putStrLn "\ESC[2 q"
            let juo' = case (mode juo) of
                    Juo.Normal  -> juo { mult = [] }
                    Juo.Command -> juo { messageBuf = "", mode = Juo.Normal }
                    Juo.Insert  -> do 
                        let juo' = juo { mode = Juo.Normal } in
                            (moveCursor juo' Juo.Left)
                    _           -> juo { mode = Juo.Normal }
            return (juo', True)

        -- Called upon resize event
        (_, CUR.KeyResize) -> do
            (winH, winW) <- CUR.scrSize
            let juo' = juo { windowSize = IVec2 winH winW }
            
            CUR.bkgrndSet CUR.attr0 (CUR.Pair 2)

            return (juo', True)

        -- NORMAL MODE BINDINGS
        (Juo.Normal, CUR.KeyChar ch)
            | ch == (delete settings) -> do
                let juo' = deleteChar juo in
                    return (juo', True)

            | ch `elem` ['1'..'9'] ->
                let juo' = updateMult juo ch in
                return (juo', True)

            | ch == '0' ->
                let juo' = juo { cursorPos = ((y),0)
                , documentPos = (l, 0)
                } in
                    return (juo', True)
            | ch == '$' ->
                let newCol = if (currentLineLength juo) > 0
                    then
                        (currentLineLength juo) - 1
                    else 0
                    in
                        let juo' = juo { cursorPos = ((y)
                        , newCol)
                        , documentPos = (l, newCol)
                        } in
                            return (juo', True)

            -- SWITCH MODE
            | ch == 'i' -> do
                let juo' = juo { mode = Juo.Insert
                } in
                    return (juo', True)

            | ch == 'o' -> do
                let juo' = moveCursor juo { mode = Juo.Insert
                } Juo.Down in
                    return (juo', True)

            | ch == 's' -> do
                CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 3))

                let juo' = juo { mode = Juo.Select
                } in
                    return (juo', True)

            | ch == ';' ->
                let juo' = juo { mode = Juo.Command
                , messageBuf = "$"
                } in
                    return (juo', True)

            | ch == 'a' -> return ((appendChar juo), True)

            -- NORMAL MODE SHORTCUTS
            | ch == 'r' -> do
                let juo' = juo { mode = Juo.Normal
                } in
                    return (juo', True)

            | ch == (up settings) ->
                let juo' = moveCursor juo Juo.Up in
                    return (juo', True)
            | ch == down settings ->
                let juo' = moveCursor juo Juo.Down in
                    return (juo', True)
            | ch == (left settings) -> 
                let juo' = moveCursor juo Juo.Left in
                    return (juo', True)
            | ch == (right settings) -> 
                let juo' = moveCursor juo Juo.Right in
                    return (juo', True)

        -- SELECT MODE BINDINGS
        (Juo.Select, _) -> return (juo, True)
                
        -- INSERT MODE BINDINGS
        (Juo.Insert, CUR.KeyChar ch)
            -- TODO This needs to be implemented properly
            | ch == '\n' || ch == '\r' -> do
                let juo' = moveCursor juo Juo.Down in
                    return (juo', True)

        (Juo.Insert, CUR.KeyChar '\DEL') ->
            let juo' = deleteCharBefore juo in
                return (juo', True)
        (Juo.Insert, CUR.KeyBackspace) ->
            let juo' = deleteCharBefore juo in
                return (juo', True)

        (Juo.Insert, CUR.KeyChar ch)
            | isPrint ch -> do
                let juo' = Juo.insertChar juo ch 
                return (juo', True)

        -- COMMAND MODE BINDINGS
        (Juo.Command, CUR.KeyChar ch)
            | ch == '\n' || ch == '\r' -> do
                return (execCommand juo)

        (Juo.Command, CUR.KeyChar ch)
            | isPrint ch -> do
                let juo' = Juo.addCharToMessage juo ch
                return (juo', True)
            | otherwise ->
                return (juo, True)

        (Juo.Command, CUR.KeyBackspace) -> do
            let juo' = juo { 
                messageBuf = if length (messageBuf juo) > 1 then (init (messageBuf juo)) else "$"
            } in
                return (juo', True)

        (Juo.Normal, _) -> return (juo, True)

        _ -> return (juo, False)

    CUR.update
    
    when shouldContinue (loopJuo juo' settings)
        
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

getScreenSize :: IO IVec2
getScreenSize = do
    (y, x) <- CUR.scrSize
    return (IVec2 y x)


_parseLine :: [String] -> [DocumentLine]
_parseLine = map (\l -> DocumentLine l (length l))

parseDocument :: FileName -> FullText -> Juo
parseDocument filename text =
    let fileType = case takeExtension filename of
            ".hs"  -> Juo.Haskell
            ".lhs" -> Juo.Haskell
            ".txt" -> Juo.PlainText
            ".md"  -> Juo.Markdown
            ext    -> Juo.Unknown ext

        ls = lines text
        ds = _parseLine ls
    in Juo
        { document       = ds
        , documentLength = length ds
        , documentType   = fileType
        }

newColor :: CUR.Color -> (Int, Int, Int) -> IO ()
newColor colorNum (r, g, b) =
    let r' = round (fromIntegral r * 3.92156862745)
        g' = round (fromIntegral g * 3.92156862745)
        b' = round (fromIntegral b * 3.92156862745)
    in
        CUR.initColor colorNum (r', g', b')


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

    out <- if length args /= 1 then
        do
            p <- getProgName
            putStrLn ("Usage: " ++ p ++ " <filename>")
            exitFailure
    else
        do
            setEnv "ESCDELAY" "0"

            --CUR.initCurses
            CURHELP.start
            has256Color <- CUR.hasColors

            if not has256Color then do
                putStrLn "Requires full color support (i.e. xterm-256)"
                exitSuccess
            else do

                CUR.startColor


                --CUR.initPair (CUR.Pair 1) CURHELP.white CURHELP.defaultColor

                newColor (CUR.Color 33) (255, 255, 255)

                newColor (CUR.Color 44) (57, 255, 20)

                newColor (CUR.Color 69) (12, 12, 12)

                CUR.initPair (CUR.Pair 4) CURHELP.white CURHELP.blue

                CUR.initPair (CUR.Pair 2) (CUR.Color 33) (CUR.Color 69)

                -- Mode 
                CUR.initPair (CUR.Pair 3) (CUR.Color 44) (CUR.Color 69)



                --CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 1))

                --let bkgrndColor  = CUR.attrPlus (CURHELP.ForegroundColor CURHELP.RedF) (CURHELP.BackgroundColor CURHELP.DarkBlueB)
                --CUR.attrBoldOn

                let attr = CUR.setBold CUR.attr0 True

                CUR.bkgrndSet CUR.attr0 (CUR.Pair 2)

                CUR.keypad CUR.stdScr True
                CUR.noDelay CUR.stdScr True
                CUR.echo False


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
        settings = UserSettings {
                up = 'k',
                down = 'j',
                left = 'h',
                right = 'l',
                delete = 'x'
            }
