module Juo ( 
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
    newLine,

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


data FileType = Haskell | PlainText | Markdown | Unknown String

instance Show FileType where
     show Haskell         = "\xe777 Haskell"
     show PlainText       = "\xf09a8 Text"
     show Markdown        = "\xeeab Markdown"
     show (Unknown other) = other


data Mode = Normal | Insert | Select | Command | Message
    deriving (Eq)

instance Show Mode where
    show Normal  = ""
    show Insert  = " Ins. "
    show Select  = " Sel. "
    show Command = " Cmd. "
    show Message = " Msg. "

data Action = Inserted | Deleted
    deriving (Eq)

instance Show Action where
    show Inserted = "Insert"
    show Deleted = "Delete"


data DocumentLine = DocumentLine {
    lineContent :: String,
    lineLength  :: Int
}

data Juo = Juo {

    cursorPos         :: (Int, Int),

    documentPos       :: (Int, Int),
    mult              :: String,
    lastCharPressed   :: Maybe Char,

    window            :: CUR.Window,
    editorWindow      :: CUR.Window,
    toolbarWindow     :: CUR.Window,

    windowSize        :: (Int, Int),

    document          :: [DocumentLine],
    documentType      :: FileType,
    currentLineLength :: Int,
    documentLength    :: Int,
    filePath          :: String,

    mode              :: Mode,

    messageBuf        :: String,
    fullMessageBuf    :: [DocumentLine]
}

data Direction = Up | Down | Left | Right

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
    CUR.wMove (editorWindow juo) y x

moveCursor :: Juo -> Direction -> Juo
moveCursor juo dir =
    let (y,x) = cursorPos juo
        (winH, winW) = windowSize juo
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
    let juo' = doAtCursor juo Deleted $ \content col -> 
            take col content ++ drop (col + 1) content

        (line, col) = documentPos juo
        currLine = if null ls then (DocumentLine "" 0) else ls !! line
        ls = document juo
    if (col + 1) == (lineLength currLine) 
        then 
            moveCursor juo' Juo.Left 
        else juo'

newLine :: Juo -> Juo
newLine juo = do
    let juo' = juo { mode = Juo.Insert }
        newLine = currLine 
            { lineContent = ""
            , lineLength = 0
            }
        newLines = take (line + 1) ls ++ [newLine] ++ drop (line + 1) ls

        in moveCursor juo { document = newLines
            , documentLength = (length newLines)
            , mode = Juo.Insert
            } Juo.Down

    where
        (line, col) = documentPos juo
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
    moveCursor juo' Juo.Left

insertChar :: Juo -> Char -> Juo
insertChar juo ch =
    let juo' = doAtCursor juo Inserted $ \content col -> 
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
            let output = take winW (Hasq.interpret (messageBuf juo))
                juo'   = juo { messageBuf = output
                , mode = Juo.Normal 
                }
            return (juo', True)



saveFile :: Juo -> IO Juo
saveFile juo = do
    -- TODO Might be better to chunk write
    res <- try (writeFile (filePath juo) (unlines (map lineContent (document juo))))
           :: IO (Either IOException ())

    let buf = case res of
            Prelude.Left _  -> "File busy, failed to save."
            Prelude.Right _ -> "File saved!"


    return juo { messageBuf = buf
    , mode = Juo.Normal 
    }

    where
        (winH, _) = windowSize juo
