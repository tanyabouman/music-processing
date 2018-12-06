{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import GHC.Float (double2Float)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (readMVar, swapMVar, newMVar, MVar)
import Control.Monad (forever)

-- import PlaySine -- (openPCM, closePCM, playSound)


main :: IO ()
main = print "Hello World!"