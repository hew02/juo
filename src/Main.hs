module Main (main) where

import qualified UI.HSCurses.Curses as Curses
import System.IO
import System.Environment (getArgs, getProgName)
import System.Directory
import System.Exit (exitSuccess, exitFailure)
import System.Posix.Signals
import Control.Concurrent
import Control.Monad (when)
import System.Console.ANSI

type FullText = String

data Vec2 = Vec2 Float Float
data IVec2 = IVec2 { x :: Int, y :: Int }

data DocumentLine = DocumentLine {
    lineContent :: String,
    lineLength :: Int
}

data Document = Document {
    documentLines :: [DocumentLine],
    documentLength :: Int
}

data DocumentPosition = DocumentPosition {
    line :: Int,
    col :: Int
}

data Juo = Juo {
    scrY :: Int,
    scrX :: Int,
    modifier :: [Char],

    documentPosition :: DocumentPosition
}


instance Eq IVec2 where
    v1 == v2 = x v1 == x v2 && y v1 == y v2

up :: Char
up = 'k'

left :: Char
left = 'h'

right :: Char
right = 'l'

down :: Char
down = 'j'

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


switchToInsertMode :: IO ()
switchToInsertMode = do
    putStr "\ESC[6 q\STX"


printFile :: Int -> Int -> [DocumentLine] -> IO ()
printFile _ 0 _ = return ()
printFile startY len (line:lines') = do
    Curses.mvWAddStr Curses.stdScr startY 0 (lineContent line) 
    printFile (startY + 1) (len - 1) lines'


write :: Int -> Int -> String -> IO ()
write y x txt =
    Curses.mvWAddStr Curses.stdScr y x txt


startJuo :: Juo -> Int -> Int -> Document -> IO ()
startJuo juo pY pX content = do

    Curses.erase -- clear curses's virtual screen but don't force a redraw

    printFile 0 6 (documentLines content)
   
    write ((scrY juo) - 2) ((scrX juo) - 16) (show (line (documentPosition juo)))
    write ((scrY juo) - 2) ((scrX juo) - 14) "|"
    write ((scrY juo) - 2) ((scrX juo) - 12) (show (col (documentPosition juo)))
    
    write ((scrY juo) - 1) ((scrX juo) - 8) (modifier juo)
    
    Curses.move pY pX 
    
    Curses.refresh -- copy the virtual screen to the terminal
    

    c <- Curses.getCh
    case (c) of
        Curses.KeyUp -> attemptToMove juo ((pY - 1) - _modifier ) pX
        Curses.KeyChar 'k' -> attemptToMove juo ((pY - 1) - _modifier ) pX

        Curses.KeyDown -> attemptToMove juo ((pY + 1) + _modifier ) pX
        Curses.KeyChar 'j' -> attemptToMove juo ((pY + 1) + _modifier )  pX

        Curses.KeyLeft -> attemptToMove juo pY ((pX - 1) - _modifier )
        Curses.KeyChar 'h' -> attemptToMove juo pY ((pX - 1) - _modifier )

        Curses.KeyRight -> attemptToMove juo pY ((pX + 1) + _modifier )
        Curses.KeyChar 'l' -> attemptToMove juo pY ((pX + 1) + _modifier )
        
        Curses.KeyChar 'q' -> return ()

        Curses.KeyChar '1' -> updateModifier '1'   
        Curses.KeyChar '2' -> updateModifier '2'   
        Curses.KeyChar '3' -> updateModifier '3'   
        Curses.KeyChar '4' -> updateModifier '4'   
        Curses.KeyChar '5' -> updateModifier '5'   
        Curses.KeyChar '6' -> updateModifier '6'   
        Curses.KeyChar '7' -> updateModifier '7'   
        Curses.KeyChar '8' -> updateModifier '8'   
        Curses.KeyChar '9' -> updateModifier '9'   

        Curses.KeyChar 'x' -> do
            putStrLn "Not Done"
            startJuo juo pY pX content

        Curses.KeyChar '$' -> do
            startJuo juo pY endOfLine content 

        Curses.KeyChar '0' -> do
            if (modifier juo) /= "" then
                updateModifier '0'
            else
                startJuo juo pY 0 content 


        --
        Curses.KeyChar '\DEL' -> backspace
        Curses.KeyBackspace -> backspace

        _ -> return ()

    where
        
        -- TODO: Consider using the arrays, they are fast.
        -- HACK: `pY` is not representitive of whole doc
        endOfLine = (lineLength ((documentLines content) !! pY)) - 1

        backspace :: IO ()
        backspace =
            if buf == [] then
                startJuo (Juo (scrY juo) (scrX juo) buf (DocumentPosition pY pX)) pY pX content
            else
                startJuo (Juo (scrY juo) (scrX juo) (init buf) (DocumentPosition pY pX)) pY pX content
            where
                buf = (modifier juo)

        _modifier = if (modifier juo) == [] then 0
            else if (modifier juo) == ['1'] then 0
            else read (modifier juo)
        
        updateModifier :: Char -> IO ()
        updateModifier ch = do
            startJuo (Juo (scrY juo) (scrX juo) ((modifier juo) ++ [ch]) (DocumentPosition pY pX)) pY pX content

        attemptToMove :: Juo -> Int -> Int -> IO ()
        attemptToMove juo y' x' = do
            let s = Juo (scrY juo) (scrX juo) [] (DocumentPosition y x) in
                startJuo s y x content 
            where
                isOnEndOfLine = 
                    if x' == endOfLine then 
                        True
                    else
                        False

                x = 
                    if x' <= 0 then 0 
                    else if x' >= endOfLine then endOfLine
                    else x'
                y = if y' <= 0 then 0 
                    else if y' >= (scrY juo) - 1 then (scrY juo) - 1
                    else y'

getScreenSize :: IO IVec2
getScreenSize = do
    (y, x) <- Curses.scrSize
    return (IVec2 x y)


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
            size <- getScreenSize
            
            let juo = Juo (y size) (x size) [] (DocumentPosition 0 0) in do 
                content <- readFile (head args) 
                startJuo juo 0 0 (parseDocument content)

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
