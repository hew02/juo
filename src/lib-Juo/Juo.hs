{-# LANGUAGE RecordWildCards #-}

module Juo ( 
    Juo(..),
    Direction(..),
    DocumentLine(..),

    FileType(..),
    Mode(..),

    Window(..),

    moveCursor, 
    updateCursor, 
    updateMult,
    deleteChar,
    deleteLine,
    deleteCharBefore,
    insertChar,
    newLine,
    newLinePush,

    insertTab,

    getMult,
    saveFile,
    addCharToMessage,
    appendChar,

    execCommand
) where

import qualified Hasqtan as Hasq
import qualified UI.HSCurses.Curses as CUR
import qualified UI.HSCurses.CursesHelper as CURHELP
import System.Environment ( unsetEnv)
import System.Process (callCommand)
import System.IO
import Control.Exception (try, IOException)
import Data.Char (isSpace)
import Control.Monad (when)

{- | Juo's builtin file types.

__Examples:__

@
c = 'Unknown' \"C\"
@
-}
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

instance Show FileType where
     show Haskell         = "Haskell"
     show PlainText       = "Text"
     show Markdown        = "Markdown"
     show C               = "C"
     show Cpp             = "C++"
     show Hasqtan         = "Hasqtan"
     show Java            = "Java"
     show OCaml           = "OCaml"
     show (Unknown other) = other

{- | Possible modes for Juo.

__Note:__

Will most likely remove 'Message' when buffers are introduced.
-}
data Mode = Normal | Insert | Select | Command | Message
    deriving (Eq)

instance Show Mode where
    show Normal  = ""
    show Insert  = " INS "
    show Select  = " SEL "
    show Command = " CMD "
    show Message = " MSG "

data Action = Inserted | Deleted
    deriving (Eq)

instance Show Action where
    show Inserted = "Insert"
    show Deleted = "Delete"


data DocumentLine = DocumentLine {
    lineContent :: String,
    lineLength  :: Int
}

data Window = Window {
    win    :: CUR.Window,
    size   :: (Int, Int)
}

mvCursor :: Window -> Int -> Int -> IO ()
mvCursor Window{..} y x =
    CUR.wMove win y x

data Juo = Juo {

    cursorPos         :: (Int, Int),

    documentPos       :: (Int, Int),
    {-line              :: Int,
    col               :: Int,-}

    mult              :: String,
    shortcutBuf       :: String,

    lastCharPressed   :: Maybe Char,

    editorWindow      :: Juo.Window,
    toolbarWindow     :: Juo.Window,

    windowSize        :: (Int, Int),

    document          :: [DocumentLine],
    documentType      :: FileType,
    currentLineLength :: Int,
    documentLength    :: Int,
    filePath          :: String,
    editedDocument    :: Bool,

    mode              :: Mode,

    messageBuf        :: String,
    fullMessageBuf    :: [DocumentLine],

    topOffset         :: Int
}

data Direction = Up | Down | Left | Right

exitJuo :: IO ()
exitJuo = do
    CURHELP.end
    callCommand "stty sane"
    unsetEnv "ESCDELAY"

updateCursor :: Juo -> IO ()
updateCursor Juo{..} = do
    let currLine = if null document then (DocumentLine "" 0) else document !! l
        currLen = lineLength currLine
        (_, c) = documentPos
        currChar = (lineContent currLine) !! c
        numOfTabsBefore = 
            length $ filter (== '\t') (take (c + 1) (lineContent currLine))

    {-if x > lineLen || currentMode /= Juo.Insert
        then juo { cursorPos = (y, lineLen)
                , documentPos = (line, lineLen)
                }
        else juo-}
    

    if x /= 0 && x >= currLen && mode /= Juo.Insert
        then
            mvCursor editorWindow y (currLen - 1)
        else
            mvCursor editorWindow y x

    -- Move based on # of tabs
    mvCursor editorWindow y (x + (numOfTabsBefore * 3))

    where
        (y, x) = cursorPos
        (l, c) = documentPos

moveCursor :: Juo -> Direction -> Int -> Juo
moveCursor juo dir dist =
    let (y, x) = cursorPos juo
        (winH, winW) = size (editorWindow juo)
        (line, col) = documentPos juo
        ls = document juo
    in case dir of
        Juo.Up
            | y > 0 ->
                let prevLine = line - 1
                    nextLength = lineLength (ls !! prevLine)
                    newX = if x > nextLength
                           then max 0 (nextLength - 1)
                           else x
                in juo { cursorPos = (y - 1, newX)
                       , documentPos = (line - 1, newX)
                       , currentLineLength = lineLength ((document juo) !! (line - 1))
                       }
            | y == 0 && line > 0 -> do
                let prevLine = line - 1
                    nextLength = lineLength (ls !! prevLine)
                    x' = if x > nextLength
                           then max 0 (nextLength - 1)
                           else x
                juo { documentPos = (line - 1, x')
                , topOffset = (topOffset juo) - 1
                }
            | otherwise -> juo

        Juo.Down
            | y + 1 < winH && line + 1 < (length ls) ->
                let nextLine = line + 1
                    nextLength = lineLength (ls !! nextLine)
                    newX = if x > nextLength
                           then max 0 (nextLength - 1)
                           else x
                in juo { cursorPos = (y + 1, newX)
                       , documentPos = (line + 1, newX)
                       , currentLineLength = lineLength ((document juo) !! (line + 1))
                       }
            | y + 1 == winH && line + 1 < (length ls) -> do
                let nextLine = line + 1
                    nextLength = lineLength (ls !! nextLine)
                    x' = if x > nextLength
                           then max 0 (nextLength - 1)
                           else x
                juo { documentPos = (line + 1, x')
                , topOffset = (topOffset juo) + 1
                }
            | otherwise -> juo

        Juo.Left ->
            if x > 0
                then juo { cursorPos = (y, x - 1)
                         , documentPos = (line, col - 1)
                         }
                else juo

        Juo.Right ->
            let lineLen = lineLength (ls !! line) in
                if x + 1 < winW && ((x + 1 < lineLen) || ((mode juo) == Juo.Insert && x < lineLen))
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


findDiff :: String -> String -> [(Char, Char, Int)]
findDiff s1 s2 =
    [ (c1, c2, i)
    | (c1, c2, i) <- zip3 s1 s2 [0..]
    , c1 /= c2
    ]

-- Generalized: apply a transformation to the line at cursor
doAtCursor :: Juo -> Action -> (String -> Int -> String) -> Juo
doAtCursor juo action f =

    let newContent = f content col
        newLine = currLine 
            { lineContent = newContent
            , lineLength = (length newContent)
            }

        newLines = take line ls ++ [newLine] ++ drop (line + 1) ls

        differences = findDiff content newContent                

        in juo { document = newLines
            , documentLength = (length newLines)
            , fullMessageBuf = (
                if null differences 
                    then
                        (fullMessageBuf juo)
                else newMessages
            )
            }
    where
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
        content = lineContent currLine
        newMessage = (show action) ++ " `" ++  "` @ " ++ (show line) ++ ":" ++ (show col)
        newMessages = (fullMessageBuf juo) ++ [DocumentLine newMessage (length newMessage)]

deleteChar :: Juo -> Juo
deleteChar juo = do
    let (line, col) = documentPos juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
        juo' = doAtCursor juo Deleted $ \content col -> 
            take col content ++ drop (col + 1) content


        ls = document juo
    if (col + 1) == (lineLength currLine) 
        then 
            moveCursor juo' Juo.Left 1
        else juo'

newLine :: Juo -> Juo
newLine juo = do
    let juo' = juo { mode = Juo.Insert }
        newLine = DocumentLine { lineContent = ""
            , lineLength = 0
            }
        newLines = take (line + 1) ls ++ [newLine] ++ drop (line + 1) ls

        in moveCursor juo { document = newLines
            , documentLength = (length newLines)
            , mode = Juo.Insert
            } Juo.Down 1

    where
        (line, _) = documentPos juo
        ls = document juo

newLinePush :: Juo -> Juo
newLinePush juo = do
    let juo' = juo { mode = Juo.Insert }
        oldContent = take col (lineContent currLine)
        newContent = drop col (lineContent currLine)
        oldLine = DocumentLine { lineContent = oldContent
        , lineLength = length oldContent
        } 
        newLine = DocumentLine { lineContent = newContent
        , lineLength = length newContent
        }
        newLines = take line ls ++ [oldLine] ++ [newLine] ++ drop (line + 1) ls

        in juo { document = newLines
            , documentLength = (length newLines)
            , mode = Juo.Insert
            , cursorPos = (y, 0)
            }

    where
        (y, x) = cursorPos juo
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line

deleteLine :: Juo -> Juo
deleteLine juo = do
    let newLines = take line ls ++ drop (line + 1) ls
        _juo = juo { document = newLines
            , documentLength = (length newLines)
            }

    if line == (documentLength _juo)
        then 
            (moveCursor _juo Juo.Up 1)
        else
            _juo

    where
        (line, _) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line 

-- TODO not needed
deleteCharBefore :: Juo -> Juo
deleteCharBefore juo = do 
    let juo' = doAtCursor juo Deleted $ \content col -> 
            take (col - 1) content ++ drop col content
            
        (line, col) = documentPos juo
        ls = document juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
    moveCursor juo' Juo.Left 1

insertChar :: Juo -> Char -> Juo
insertChar juo ch =
    let juo' = doAtCursor juo Inserted $ \content col -> 
            take col content ++ [ch] ++ drop col content
    in
        moveCursor juo' Juo.Right 1

insertTab :: Juo -> Juo
insertTab juo =
    let juo' = doAtCursor juo Inserted $ \content col -> 
            take col content ++ (replicate 4 ' ') ++ drop col content
    in
        moveCursor juo' Juo.Right 1

appendChar :: Juo -> Juo
appendChar juo =
    moveCursor juo { mode = Juo.Insert } Juo.Right 1


addCharToMessage :: Juo -> Char -> Juo
addCharToMessage juo ch = juo { messageBuf = (messageBuf juo) ++ [ch] }

updateMult :: Juo -> Char -> Juo
updateMult juo ch =
   juo { mult = mult juo ++ [ch] }

removeSpaces :: String -> String
removeSpaces str = filter (not . isSpace) str

execCommand :: Juo -> IO (Juo, Bool)
execCommand juo = do 
    let (_, winW) = windowSize juo
    -- NOTE temporary, expand the usage of Hasqtan
    case removeSpaces (messageBuf juo) of
        "q"    -> do 
            CUR.beep
            return (juo, False)
        "exit" -> return (juo, False)
        "w"    -> do
            juo' <- Juo.saveFile juo
            return (juo', True)
        "wq"   -> do
            juo' <- Juo.saveFile juo
            return (juo', False)
        ""     -> do
            let juo'   = juo { mode = Juo.Normal 
                }
            return (juo', True)
        _      ->  do
            let output = take winW (Hasq.interp (messageBuf juo))
                juo'   = juo { messageBuf = output
                , mode = Juo.Normal 
                }
            return (juo', True)


getFileSize :: Juo -> Int
getFileSize Juo{..} = sum (map len document)
    where
        len docLine = (lineLength docLine) + 1 -- Add 1 for '\n'

saveFile :: Juo -> IO Juo
saveFile juo = do
    -- TODO Might be better to chunk write
    res <- try (writeFile (filePath juo) (unlines (map lineContent (document juo))))
           :: IO (Either IOException ())

    let buf = case res of
            Prelude.Left _  -> (show (filePath juo) ++ show (getFileSize juo) ++ "B failed to write")
            Prelude.Right _ -> do
                (show (filePath juo) ++ " " ++ show (documentLength juo) ++ "L, " ++ show (getFileSize juo) ++ "B written")


    return juo { messageBuf = buf
    , mode = Juo.Normal 
    }

    where
        (winH, _) = windowSize juo
