module Juo.Util (
    numOfDigit,
    setBarCursor,
    setBlockCursor,
    setUnderscoreCursor,
    newColor,
    removeSpaces,
    getLineLength
) where

import System.IO (hFlush, stdout)
import qualified UI.HSCurses.Curses as HC

import Data.Char (isSpace)

numOfDigit :: Integer -> Int
numOfDigit = go 1 . abs
    where
        go ds n = if n >= 10 then go (ds + 1) (n `div` 10) else ds


-- | Sets to pipe character.
setBarCursor :: IO ()
setBarCursor = do
  putStr "\ESC[6 q"
  hFlush stdout

-- | Sets to full block character.
setBlockCursor :: IO ()
setBlockCursor = do
  putStr "\ESC[0 q"
  hFlush stdout

-- | Sets to underscore character.
setUnderscoreCursor :: IO ()
setUnderscoreCursor = do
  putStr "\ESC[4 q"
  hFlush stdout

-- | Use RGB model based 0-255, rather than 0-1000.
newColor :: HC.Color -> (Int, Int, Int) -> IO ()
newColor colorNum (r, g, b) =
  let r' = round ((fromIntegral r) * ratio)
      g' = round ((fromIntegral g) * ratio)
      b' = round ((fromIntegral b) * ratio)
   in HC.initColor colorNum (r', g', b')
  where
    ratio :: Double
    ratio = 3.92156862745

-- | As it sounds.
removeSpaces :: String -> String
removeSpaces str = filter (not . isSpace) str

-- | Wrapper around `length` but we account for tab character width.
getLineLength :: String -> Int
getLineLength lineContent = 
  sum $ map (\c -> if c == '\t' then 4 else 1) lineContent