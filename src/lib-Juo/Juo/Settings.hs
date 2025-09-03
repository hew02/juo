module Juo.Settings (UserSettings(..)) where

data UserSettings = UserSettings {
    up     :: Char,
    down   :: Char,
    left   :: Char,
    right  :: Char,
    delete :: Char,

    showLineNumbers :: Bool
}