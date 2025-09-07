module Juo.Draw (
    paintContent,
    drawToolbar
) where

import qualified UI.HSCurses.Curses as CUR
--import qualified UI.HSCurses.CursesHelper as CURHELP
import Control.Monad (when)

import Juo
import Juo.Settings

-- HSCurses abstraction
draw :: CUR.Window -> Int -> Int -> String -> IO ()
draw win y x txt =
    CUR.mvWAddStr win y x txt

paintContent :: Juo -> UserSettings -> [DocumentLine] -> IO ()
paintContent juo settings buf = do
    let (h, _) = windowSize juo
        --(y, _) = cursorPos juo
    CUR.wAttrSet (editorWindow juo) (CUR.attr0, (CUR.Pair 2))
    CUR.wRefresh (editorWindow juo)
    go 0 h buf
    where
        go _ 2 _  = return ()
        go row n [] = do
            draw (window juo) row 0 "~"
            go (row + 1) (n - 1) []
        go row n (line:rest) = do
            when (showLineNumbers settings) (draw (window juo) row 1 (show (row + 1)))
            draw (editorWindow juo) row 0 ((lineContent line))
            go (row + 1) (n - 1) rest

drawToolbar :: Juo -> UserSettings -> IO ()
drawToolbar juo settings = do
    let (_, winW) = windowSize juo
        fileTypeStr = show (documentType juo)
        (l, c) = documentPos juo
        lHeight = 0
    
    let colStr = show (c + 1)
        lineStr = show (l + 1)

    multString <- getMult juo

    let maxMultLen = 8
        modeText = show (mode juo)

    -- Left

    CUR.wAttrSet (toolbarWindow juo) (CUR.attr0, (CUR.Pair 5))
    -- HACK there might be a better way of doing this
    draw (toolbarWindow juo) 0 0 (replicate winW ' ')

    -- Mode
    CUR.wAttrSet (toolbarWindow juo) (CUR.attr0, (CUR.Pair 3))
    draw (toolbarWindow juo) 0 0 modeText

    CUR.wAttrSet (toolbarWindow juo) (CUR.attr0, (CUR.Pair 5))
    draw (toolbarWindow juo) 1 0 (replicate winW ' ')

    -- File type
    draw (toolbarWindow juo) lHeight (winW - (length fileTypeStr) - 2) fileTypeStr

    -- File path
    draw (toolbarWindow juo) 0 (1 + (length modeText)) (filePath juo)


    -- Command buffer
    when ((mode juo) == Juo.Command) (draw (toolbarWindow juo) 1 0 [(cursorCmd settings)])
    draw (toolbarWindow juo) 1 2 (messageBuf juo)

    -- Right
    draw (toolbarWindow juo) 1 (winW - maxMultLen) (take (maxMultLen - 1) multString)

    draw (toolbarWindow juo) lHeight (winW - (length fileTypeStr) - 3 - (length colStr)) colStr
    draw (toolbarWindow juo) lHeight (winW - (length fileTypeStr) - 5 - (length colStr)) "|"
    draw (toolbarWindow juo) lHeight (winW - (length fileTypeStr) - 6 - (length lineStr) - (length colStr)) lineStr