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
    addChar,

    getMult,
    saveFile,
    addChar,
    addCharToMessage,
    write,

    execCommand
) where

import qualified Hasqtan as Hasq

import qualified UI.HSCurses.Curses as CUR

{-
    General Types.
-}

data Vec2 = Vec2 Float Float
    deriving (Show, Eq)

data IVec2 = IVec2 Int Int
    deriving (Show, Eq)


data FileType = Haskell | PlainText | Unknown String

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

updateCursor :: Juo -> IO ()
updateCursor juo = do
    let (y, x) = cursorPos juo
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

        Juo.Right ->
            let lineLen = lineLength (ls !! line)
            in if x + 1 < winW && x + 1 < lineLen
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
    doAtCursor juo' $ \content col ->
        take col content ++ drop (col + 1) content
    where
        juo' = moveCursor juo Juo.Left

addChar :: Juo -> Char -> Juo
addChar juo ch =
    doAtCursor juo' $ \content col ->
        take col content ++ [ch] ++ drop col content
    where
        juo' = moveCursor juo Juo.Right

appendChar :: Juo -> Char -> Juo
appendChar juo ch =
    doAtCursor juo' $ \content col ->
        take col content ++ [ch] ++ drop col content
    where
        juo' = moveCursor juo Juo.Right


addCharToMessage :: Juo -> Char -> Juo
addCharToMessage juo ch = juo { messageBuf = (messageBuf juo) ++ [ch] }

updateMult :: Juo -> Char -> Juo
updateMult juo ch =
   juo { mult = mult juo ++ [ch] }

execCommand :: Juo -> Juo
execCommand juo = do 
    if True then do
        let output = take 80 (Hasq.interpret (messageBuf juo))
        juo { messageBuf = output, mode = Juo.Normal }
    else
        juo { messageBuf = "Command unavailable", mode = Juo.Normal }

saveFile :: Juo -> IO Juo
saveFile juo = do
    CUR.erase
    write (winH - 1) 2 "File saved"
    CUR.refresh

    return juo

    where
        IVec2 winH winW = windowSize juo