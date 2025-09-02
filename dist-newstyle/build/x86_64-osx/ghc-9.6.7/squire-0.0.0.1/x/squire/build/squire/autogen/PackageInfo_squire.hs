{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_squire (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "squire"
version :: Version
version = Version [0,0,0,1] []

synopsis :: String
synopsis = "Another text editor."
copyright :: String
copyright = ""
homepage :: String
homepage = "henrywandover.com"
