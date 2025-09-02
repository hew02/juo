module Juo ( 
    Vec2(..), 
    IVec2(..), 
    Juo(..), 
    Cursor(..), 
    Direction(..),

    Document(..),
    DocumentLine(..),
    DocumentPosition(..),

    moveCursor, 
    updateCursor, 
    updateMult,
    deleteAtCursor
) where

import qualified UI.HSCurses.Curses as CUR
import Data.IORef
import Control.Monad

{-
    General Types.
-}


data Vec2 = Vec2 Float Float
    deriving (Show, Eq)

data IVec2 = IVec2 Int Int
    deriving (Show, Eq)


{-
    Document Types.
-}

data DocumentLine = DocumentLine {
    lineContent :: String,
    lineLength  :: Int
}

data Document = Document {
    documentLines  :: [DocumentLine],
    documentLength :: Int
}

data DocumentPosition = DocumentPosition {
    line :: IORef Int,
    col  :: IORef Int
}



data Juo = Juo {
    height           :: Int,
    width            :: Int,

    cursor           :: Cursor,

    currentDocument  :: Document,
    documentPosition :: DocumentPosition
}

data Direction = Up | Down | Left | Right

data Cursor = Cursor {
    y    :: IORef Int,
    x    :: IORef Int,
    mult :: IORef String
}

updateCursor :: Juo -> IO ()
updateCursor juo = do
    y <- readIORef (y cur)
    x <- readIORef (x cur)
    CUR.move y x

    where
        cur = (cursor juo)

moveCursor :: Juo -> Direction -> IO ()
moveCursor juo Juo.Up = do
    y' <- readIORef (y (cursor juo))
    x' <- readIORef (x (cursor juo))

    when (y' > 0) $ do
        prevLineIndex <- readIORef (line (documentPosition juo))

        
        modifyIORef (line (documentPosition juo)) (subtract 1)
        modifyIORef (y (cursor juo)) (subtract 1)

        let nextLength = getLineLength juo (prevLineIndex - 1)

        newPos <- 
            if x' > nextLength 
                then pure (nextLength + if nextLength /= 0 then 1 else 0)
                else readIORef (x (cursor juo))
            
        writeIORef (x (cursor juo)) newPos

moveCursor juo Juo.Down = do
    y' <- readIORef (y (cursor juo))
    x' <- readIORef (x (cursor juo))

    when (y' + 1 < (height juo)) $ do
        prevLineIndex <- readIORef (line (documentPosition juo))

        modifyIORef (line (documentPosition juo)) (+ 1)
        modifyIORef (y (cursor juo)) (+ 1)

        let nextLength = getLineLength juo (prevLineIndex + 1)

        newPos <- 
            if x' > nextLength 
                then pure (nextLength - if nextLength /= 0 then 1 else 0)
                else readIORef (x (cursor juo))

        writeIORef (x (cursor juo)) newPos

moveCursor juo Juo.Left = do
    x' <- readIORef (x (cursor juo))

    if x' > 0 then do
        modifyIORef (col (documentPosition juo)) (subtract 1)
        modifyIORef (x (cursor juo)) (subtract 1)
    else
        return ()
moveCursor juo Juo.Right = do
    x' <- readIORef (x (cursor juo))
    currentLineIndex' <- readIORef (line (documentPosition juo))

    if x' + 1 < (width juo) && x' + 1 < getLineLength juo currentLineIndex' then do
        modifyIORef (col (documentPosition juo)) (+ 1)
        modifyIORef (x (cursor juo)) (+ 1)
    else
        return ()

deleteAtCursor :: Juo -> IO ()
deleteAtCursor juo =
    return ()

-- Starts indexing at 1
getLineLength :: Juo -> Int -> Int
getLineLength juo it = 
    (lineLength ((documentLines (currentDocument juo)) !! (it - 1)))


updateMult :: Juo -> Char -> IO ()
updateMult juo ch =
    modifyIORef (mult (cursor juo)) (++ [ch])

--loopJuo (Juo (height juo) (width juo) ((modifier juo) ++ [ch]) (DocumentPosition pY pX)) pY pX content

{-attemptToMove :: Juo -> Int -> Int -> IO ()
attemptToMove juo y' x' = do
    let s = Juo (height juo) (width juo) [] (DocumentPosition y x) in
        loopJuo s y x content 
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
            else if y' >= (height juo) - 1 then (height juo) - 1
            else y'-}