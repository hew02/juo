{- | Configurable settings in Juo that are yet to be introduced. The Hasqtan
language must be built out more before we can move forward on this
area. At that point, Juo will have its own config file.
-}

module Juo.Settings (UserSettings(..)) where


data UserSettings = UserSettings {
    up     :: Char,
    down   :: Char,
    left   :: Char,
    right  :: Char,
    delete :: Char,

    cursorCmd :: Char,

    showLineNumbers :: Bool
}