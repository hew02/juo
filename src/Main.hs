module Main (main) where

import Juo
import Juo.Settings

import qualified UI.HSCurses.Curses as Curses
import System.IO
import System.Environment (getArgs, getProgName)
import System.Directory
import System.Exit (exitSuccess, exitFailure)
import System.Posix.Signals
import Control.Concurrent
import Control.Monad (when)
import System.Console.ANSI
import Data.IORef


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


printFile :: Int -> Int -> [DocumentLine] -> IO ()
printFile y h lines' = printFileLine y h 1 lines' 

    where
        printFileLine :: Int -> Int -> Int -> [DocumentLine] -> IO ()
        printFileLine _ _ _ [] = return ()
        printFileLine _ 0 _ _ = return ()
        printFileLine startY len lineNo (line:lines') = do
            Curses.mvWAddStr Curses.stdScr startY 0 ((lineContent line))
            printFileLine (startY + 1) (len - 1) (lineNo + 1)  lines'


write :: Int -> Int -> String -> IO ()
write y x txt =
    Curses.mvWAddStr Curses.stdScr y x txt


startJuo :: Document -> UserSettings -> IO ()
startJuo content settings = do
    initY <- newIORef 0
    initX <- newIORef 0
    initMult <- newIORef "1"

    IVec2 h w <- getScreenSize


    initLine <- newIORef 1
    initCol <- newIORef 1

    let cursor = Cursor { y = initY, x = initX, mult = initMult } in 
        let juo = Juo {
            height = h,
            width = w,

            cursor = cursor,

            currentDocument = content,
            documentPosition = (DocumentPosition initLine initCol)
        } in
            loopJuo juo content settings


loopJuo :: Juo -> Document -> UserSettings -> IO ()
loopJuo juo content settings = do

    Curses.erase -- clear curses's virtual screen but don't force a redraw

    printFile 0 (height juo) (documentLines content)


    l <- readIORef(line (documentPosition juo))
    c <- readIORef(col (documentPosition juo))


    write ((height juo) - 2) ((width juo) - 16) (show l)
    write ((height juo) - 2) ((width juo) - 14) "|"
    write ((height juo) - 2) ((width juo) - 12) (show c)
    
    multString <- (readIORef (mult (cursor juo)))
    write ((height juo) - 1) ((width juo) - 8) multString

    -- NOTE: Always update cursor last!
    updateCursor juo
        
    Curses.refresh -- copy the virtual screen to the terminal
    
    c <- Curses.getCh

    shouldQuit <- case (c) of
        Curses.KeyChar ch
            | ch == (up settings) ->
                moveCursor juo Juo.Up >> return False
            | ch == down settings ->
                moveCursor juo Juo.Down >> return False
            | ch == (left settings) -> 
                moveCursor juo Juo.Left >> return False
            | ch == (right settings) -> 
                moveCursor juo Juo.Right >> return False
            | ch == (delete settings) -> 
                putStrLn "Not Done" >> return False

        
        Curses.KeyChar 'q' -> return () >> return True

        Curses.KeyChar d | d `elem` ['1'..'9'] -> 
            updateMult juo d >> return False

        {-Curses.KeyChar '$' -> do
            loopJuo juo pY endOfLine content 

        Curses.KeyChar '0' -> do
            if (modifier juo) /= "" then
                updateModifier '0'
            else
                loopJuo juo pY 0 content 


        -- MacOS uses \DEL rather than backspace
        Curses.KeyChar '\DEL' -> backspace
        Curses.KeyBackspace -> backspace-}

        _ -> return False
    
    when (not shouldQuit) (loopJuo juo content settings)
        
    where
                
        -- TODO: Consider using the arrays, they are fast.
        -- HACK: `pY` is not representitive of whole doc
        {-endOfLine = 
            if (documentLength content) > 0 then
                (lineLength ((documentLines content) !! pY)) - 1
            else 0

        backspace :: IO ()
        backspace =
            if buf == [] then
                loopJuo (Juo (height juo) (width juo) buf (DocumentPosition pY pX)) pY pX content
            else
                loopJuo (Juo (height juo) (width juo) (init buf) (DocumentPosition pY pX)) pY pX content
            where
                buf = (modifier juo)

        _modifier = if (modifier juo) == [] then 0
            else if (modifier juo) == ['1'] then 0
            else read (modifier juo)
    
-}

getScreenSize :: IO IVec2
getScreenSize = do
    (y, x) <- Curses.scrSize
    return (IVec2 y x)


_parseLine :: [String] -> [DocumentLine]
_parseLine [] = []
_parseLine (line:lines) =
    DocumentLine line (length line) : _parseLine lines

parseDocument :: FullText -> Document
parseDocument text =
    let documentLines = (_parseLine (lines text)) in
        Document documentLines (length documentLines)

main :: IO ()
main = do

    done <- newEmptyMVar

    let handler = do
            --putStrLn ""
            --putStrLn "interrupt"
            --Curses.cursSet Curses.CursorVisible
            putMVar done ()
            exitSuccess
    
    installHandler keyboardSignal (Catch handler) Nothing
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
            Curses.initCurses
            Curses.keypad Curses.stdScr True
            Curses.echo False

            --Curses.cursSet Curses.CursorInvisible
            
            content <- readFile (head args) 
            startJuo (parseDocument content) settings

            Curses.endWin
            Curses.cursSet Curses.CursorVisible

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
