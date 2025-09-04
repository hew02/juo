module Juo ( 
    Vec2(..), 
    IVec2(..), 
    Juo(..),
    Direction(..),
    DocumentLine(..),

    FileType(..),
    Mode(..),

    moveCursor, 
    updateCursor, 
    updateMult,
    deleteChar,
    deleteCharBefore,
    insertChar,

    getMult,
    saveFile,
    addCharToMessage,
    write,
    appendChar,

    execCommand
) where

import qualified Hasqtan as Hasq
import qualified UI.HSCurses.Curses as CUR
import qualified UI.HSCurses.CursesHelper as CURHELP
import System.Environment ( unsetEnv)
import System.Process (callCommand)
import Data.Char (isSpace)

{-
    General Types.
-}

data Vec2 = Vec2 Float Float
    deriving (Show, Eq)

data IVec2 = IVec2 Int Int
    deriving (Show, Eq)


data FileType = Haskell | PlainText | Markdown | Unknown String

instance Show FileType where
     show Haskell         = "Haskell \xe777"
     show PlainText       = "Plaintext \xf09a8"
     show Markdown        = "Markdown \xeeab"
     show (Unknown other) = other


data Mode = Normal | Insert | Select | Command
    deriving (Eq)

data Token
    = Number Int
    | CMD String
    | Symbol Char
    deriving (Show, Eq)

data Command 
    = Print String 
    | Quit
    deriving (Show)

data DocumentLine = DocumentLine {
    lineContent :: String,
    lineLength  :: Int
}

data Juo = Juo {
    cursorPos         :: (Int, Int),
    documentPos       :: (Int, Int),
    mult              :: String,

    windowSize        :: IVec2,

    document          :: [DocumentLine],
    documentType      :: FileType,
    currentLineLength :: Int,
    documentLength    :: Int,

    mode              :: Mode,

    messageBuf        :: String
}

data Direction = Up | Down | Left | Right


-- HSCurses abstraction
write :: Int -> Int -> String -> IO ()
write y x txt =
    CUR.mvWAddStr CUR.stdScr y x txt

exitJuo :: IO ()
exitJuo = do
    CURHELP.end
    callCommand "stty sane"
    unsetEnv "ESCDELAY"

updateCursor :: Juo -> IO ()
updateCursor juo = do
    let (y, x) = cursorPos juo

    {-if x > lineLen || currentMode /= Juo.Insert
        then juo { cursorPos = (y, lineLen)
                , documentPos = (line, lineLen)
                }
        else juo-}
    CUR.move y x
    

moveCursor :: Juo -> Direction -> Juo
moveCursor juo dir =
    let (y,x) = cursorPos juo
        IVec2 winH winW = windowSize juo
        (line, col) = documentPos juo
        ls = document juo
    in case dir of
        Juo.Up ->
            if y > 0
                then
                    let prevLine = line - 1
                        nextLength = lineLength (ls !! prevLine)
                        newX = if x > nextLength
                               then max 0 (nextLength - 1)
                               else x
                    in juo { cursorPos = (y - 1, newX)
                           , documentPos = (line - 1, newX)
                           , currentLineLength = lineLength ((document juo) !! (line - 1))
                           }
                else juo

        Juo.Down ->
            if y + 1 < winH && line + 1 < length ls
                then
                    let nextLine = line + 1
                        nextLength = lineLength (ls !! nextLine)
                        newX = if x > nextLength
                               then max 0 (nextLength - 1)
                               else x
                    in juo { cursorPos = (y + 1, newX)
                           , documentPos = (line + 1, newX)
                           , currentLineLength = lineLength ((document juo) !! (line + 1))
                           }
                else juo

        Juo.Left ->
            if x > 0
                then juo { cursorPos = (y, x - 1)
                         , documentPos = (line, col - 1)
                         }
                else juo

        Juo.Right -> do
            let lineLen = lineLength (ls !! line)
                currentMode = (mode juo)
            if x + 1 < winW && ((x + 1 < lineLen) || (currentMode == Juo.Insert && x < lineLen))
                then juo { cursorPos = (y, x + 1)
                        , documentPos = (line, col + 1)
                        }
                else juo

_getCurrentLine :: Juo -> IO Int
_getCurrentLine juo = do 
    pure (fst (documentPos juo))

_getCurrentCol :: Juo -> IO Int
_getCurrentCol juo = do 
    pure (snd (documentPos juo))

getMult :: Juo -> IO String
getMult juo = do 
    pure (mult juo)

_getCurrentLineContent :: Juo -> IO String
_getCurrentLineContent juo = do
    let (line, _) = documentPos juo
    pure $ lineContent (document juo !! line)

_getLineLength :: Juo -> Int -> IO Int
_getLineLength juo line = do
    pure $ lineLength (document juo !! line)

-- Generalized: apply a transformation to the line at cursor
doAtCursor :: Juo -> (String -> Int -> String) -> Juo
doAtCursor juo f =

    let newContent = f content col
        newLine = currLine 
            { lineContent = newContent
            , lineLength = (length newContent)
            }

        newLines = take line ls ++ [newLine] ++ drop (line + 1) ls

        in juo { document = newLines
            , documentLength = (length newLines)
            }
    
    where
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
        content = lineContent currLine

deleteChar :: Juo -> Juo
deleteChar juo = do 
    let juo' = doAtCursor juo $ \content col -> take col content ++ drop (col + 1) content
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line

    if (col + 1) == (lineLength currLine) then moveCursor juo' Juo.Left else juo'

-- TODO not needed
deleteCharBefore :: Juo -> Juo
deleteCharBefore juo = do 
    let juo' = doAtCursor juo $ \content col -> take (col - 1) content ++ drop col content
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
    moveCursor juo' Juo.Left

insertChar :: Juo -> Char -> Juo
insertChar juo ch =
    let juo' = doAtCursor juo $ \content col -> 
            take col content ++ [ch] ++ drop col content
    in
        moveCursor juo' Juo.Right

appendChar :: Juo -> Juo
appendChar juo =
    moveCursor juo { mode = Juo.Insert } Juo.Right


addCharToMessage :: Juo -> Char -> Juo
addCharToMessage juo ch = juo { messageBuf = (messageBuf juo) ++ [ch] }

updateMult :: Juo -> Char -> Juo
updateMult juo ch =
   juo { mult = mult juo ++ [ch] }

removeSpaces :: String -> String
removeSpaces str = filter (not . isSpace) str

execCommand :: Juo -> (Juo, Bool)
execCommand juo = do 
    let IVec2 _ w = windowSize juo
    
    -- NOTE temporary, expand the usage of Hasqtan
        (isEditorCommand, shouldContinue) = 
            case (removeSpaces (messageBuf juo)) of
                "$q" -> (True, False)
                "$exit" -> (True, False)
                "$w" -> do

                    --juo' <- Juo.saveFile juo

                    (True, True)
                _ -> (False, True)
        
        output = if not isEditorCommand 
            then 
                take w (Hasq.interpret (tail (messageBuf juo)))
            else ""
        juo' = juo { messageBuf = output
                , mode = Juo.Normal
                }

    (juo', shouldContinue)

saveFile :: Juo -> IO Juo
saveFile juo = do
    CUR.erase
    write (winH - 1) 2 "File saved"
    CUR.refresh

    return juo

    where
        IVec2 winH winW = windowSize juo
