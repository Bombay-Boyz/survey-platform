-- app/Main.hs
module Main (main) where

import Foundation (App (..))
import Yesod.Core (warp)

main :: IO ()
main = warp 3000 App