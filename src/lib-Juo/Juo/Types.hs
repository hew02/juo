module Juo.Types (
    Direction (..),
    DocumentLine (..),
    FileType (..),
    File(..),
    Action(..),
    Mode (..),

    Window (..),

    EditorSection(..)
) where

import qualified UI.HSCurses.Curses as HC

-- | Juo's builtin file types. TODO Can be extended.
--
-- __Examples:__
--
-- @
-- c = 'Unknown' \"C\"
-- @
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
  deriving (Eq)

instance Show FileType where
  show Haskell = "Haskell"
  show PlainText = "Text"
  show Markdown = "Markdown"
  show C = "C"
  show Cpp = "C++"
  show Hasqtan = "Hasqtan"
  show Java = "Java"
  show OCaml = "OCaml"
  show (Unknown other) = other




-- | Possible modes for Juo.
--
-- __Note:__
--
-- Will most likely remove 'Message' when buffers are introduced.
data Mode = Normal | Insert | Select | Command | Message
  deriving (Eq)

instance Show Mode where
  show Normal = ""
  show Insert = " INS "
  show Select = " SEL "
  show Command = " EX "
  show Message = " MSG "




data Action = Inserted | Deleted
  deriving (Eq)

instance Show Action where
  show Inserted = "Insert"
  show Deleted = "Delete"




data DocumentLine = DocumentLine
  { lineContent :: String,
    lineLength :: Int
  }
  deriving (Eq)




-- | Possible modes for Juo.
data Window = Window
  { win :: HC.Window,
    size :: (Int, Int),
    winHeight :: Int,
    winWidth :: Int
  }
  deriving (Eq)




data EditorSection 
  = Background
  | EditorBackground
  | Toolbar
  | LeftColumn
  | ModeInsert
  | ModeEx
  | ModeSelect
  deriving (Eq)



-- | The direction the cursor is moving in.
data Direction = Up | Down | Left | Right
  deriving (Show)




data File = File 
  {
    fileContent :: [DocumentLine],
    fileType :: FileType,
    fileLength :: Int,
    filePath :: String
  }
  deriving (Eq)