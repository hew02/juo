module Main (main) where

import Juo
import Juo.Settings

import qualified UI.HSCurses.Curses as CUR
import qualified UI.HSCurses.CursesHelper as CURHELP
--import System.IO
import System.Environment (getArgs, getProgName, setEnv, unsetEnv)
--import System.Directory
import System.FilePath
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

writeFileType :: Juo -> IO ()
writeFileType juo = do
    let IVec2 winH winW = windowSize juo
        fileTypeStr = case (documentType juo) of
            Juo.Haskell -> "Haskell"
            Juo.PlainText -> "PlainText"
            Juo.Unknown ext -> ext

    Juo.write (winH - 2) (winW - (length fileTypeStr) - 1) fileTypeStr

writeMode :: Juo -> IO ()
writeMode juo = do
    let IVec2 winH winW = windowSize juo
        modeStr = case (mode juo) of
            Juo.Normal  -> "N"
            Juo.Insert  -> "I"
            Juo.Select  -> "S"
            Juo.Command -> "C"
    
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

loopJuo :: Juo -> UserSettings -> IO ()
loopJuo juo settings = do

    let IVec2 winH winW = windowSize juo
        (l, c) = documentPos juo
        (y, x) = cursorPos juo

    CUR.erase -- clear curses's virtual screen but don't force a redraw

    printFile 0 winH juo

    Juo.write (winH - 2) (winW - 16) (show (l + 1))
    Juo.write (winH - 2) (winW - 14) "|"
    Juo.write (winH - 2) (winW - 12) (show (c + 1))
    
    multString <- getMult juo
    Juo.write (winH - 1) (winW - 8) multString

    writeFileType juo
    writeMode juo
    writeMessage juo


    -- NOTE: Always update cursor last!
    updateCursor juo
        
    CUR.wRefresh CUR.stdScr -- copy the virtual screen to the terminal
    
    c <- CUR.getCh

    (juo', shouldContinue) <- case ((mode juo), c) of

        (_, CUR.KeyChar '\ESC') -> do
            let juo' = case (mode juo) of
                    Juo.Normal -> juo { mult = [] }
                    Juo.Command -> juo { messageBuf = "", mode = Juo.Normal }
                    _          -> juo { mode = Juo.Normal }
            return (juo', True)

        -- NORMAL MODE BINDINGS
        (Juo.Normal, CUR.KeyChar ch)
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

            | ch == (delete settings) -> do
                let juo' = deleteChar juo in
                    return (juo', True)

            | ch `elem` ['1'..'9'] ->
                let juo' = updateMult juo ch in
                return (juo', True)

            -- MacOS uses \DEL rather than backspace
            {-| ch == '\DEL' ->
                let juo' = handleBackspace juo in
                    return (juo', True)-}

            | ch == '0' ->
                let juo' = juo { cursorPos = ((y),0)
                } in
                    return (juo', True)
            | ch == '$' ->
                let juo' = juo { cursorPos = ((y),(currentLineLength juo) - 1)
                } in
                    return (juo', True)


            | ch == 'w' -> do
                juo' <- Juo.saveFile juo
                return (juo', True)

            | ch == 'q' -> do
                return (juo, False)

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
                let juo' = juo { mode = Juo.Select
                } in
                    return (juo', True)

            | ch == ';' ->
                let juo' = juo { mode = Juo.Command
                , messageBuf = ""
                } in
                    return (juo', True)

            | ch == 'a' -> do
                let juo' = juo { mode = Juo.Insert
                } in
                    return (juo', True)

            -- NORMAL MODE SHORTCUTS
            | ch == 'r' -> do
                let juo' = juo { mode = Juo.Normal
                } in
                    return (juo', True)
                
        -- INSERT MODE BINDINGS
        (Juo.Insert, CUR.KeyChar ch)
            -- TODO This needs to be implemented properly
            | ch == '\n' || ch == '\r' -> do
                let juo' = moveCursor juo Juo.Down in
                    return (juo', True)

        (Juo.Insert, CUR.KeyChar ch)
            | isPrint ch -> do
                let juo' = Juo.addChar juo ch 
                return (juo', True)


        -- COMMAND MODE BINDINGS
        (Juo.Command, CUR.KeyChar ch)
            | ch == '\n' || ch == '\r' -> do
                return (execCommand juo, True)

        (Juo.Command, CUR.KeyChar ch)
            | isPrint ch -> do
                let juo' = Juo.addCharToMessage juo ch
                return (juo', True)
            | otherwise ->
                return (juo, True)

        (Juo.Command, CUR.KeyBackspace) -> do
            let juo' = juo { 
                messageBuf = if (messageBuf juo) /= "" then (init (messageBuf juo)) else "" 
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
            ".hs" -> Juo.Haskell
            ".lhs" -> Juo.Haskell
            ".txt" -> Juo.PlainText
            ext    -> Juo.Unknown ext

        ls = lines text
        ds = _parseLine ls
    in Juo
        { document       = ds
        , documentLength = length ds
        , documentType   = fileType
        }

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

                CUR.initColor (CUR.Color 44) (57, 255, 20)

                CUR.initColor (CUR.Color 69) (76, 76, 76)

                CUR.initPair (CUR.Pair 2) CURHELP.white (CUR.Color 69)

                -- Mode 
                CUR.initPair (CUR.Pair 3) CURHELP.green (CUR.Color 69)



                --CUR.wAttrSet CUR.stdScr (CUR.attr0, (CUR.Pair 1))

                --let bkgrndColor  = CUR.attrPlus (CURHELP.ForegroundColor CURHELP.RedF) (CURHELP.BackgroundColor CURHELP.DarkBlueB)
                --CUR.attrBoldOn

                let attr = CUR.setBold CUR.attr0 True

                CUR.bkgrndSet attr (CUR.Pair 2)



                CUR.keypad CUR.stdScr True
                CUR.noDelay CUR.stdScr True
                CUR.echo False


                let filename = head args
                text <- readFile filename


                startJuo (parseDocument filename text) settings


            

            --CUR.endWin
            CURHELP.end

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
