module Juo.Cursor (Cursor(..), moveCursor, updateCursor, Direction(..)) where

import Juo
import qualified UI.HSCurses.Curses as CUR
import Data.IORef

data Direction = Up | Down | Left | Right

data Cursor = Cursor {
    y    :: IORef Int,
    x    :: IORef Int,
    mult :: IORef String
}

updateCursor :: Cursor -> IO ()
updateCursor cursor = do
    y <- readIORef (y cursor)
    x <- readIORef (x cursor)
    CUR.move y x

moveCursor :: Cursor -> Direction -> IO ()
moveCursor cursor Juo.Cursor.Up =
    modifyIORef (y cursor) (subtract 1)
moveCursor cursor Juo.Cursor.Down =
    modifyIORef (y cursor) (+ 1)
moveCursor cursor Juo.Cursor.Left =
    modifyIORef (x cursor) (subtract 1)
moveCursor cursor Juo.Cursor.Right =
    modifyIORef (x cursor) (+ 1)


updateMult :: Juo -> IO ()
updateMult juo ch =
    modifyIORef (mult (cursor juo)) (++ ch)

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