  -- modified from https://github.com/deech/fltkhs-hello-world

{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import GHC.Float (double2Float)

import PlaySine -- (openPCM, closePCM, playSound)


buttonCb :: Ref Button -> IO ()
buttonCb b' = do
  l' <- getLabel b'
  if (l' == "Play")
    then setLabel b' "Pause"
    else setLabel b' "Play"

playTone :: {- (PCM.Access i, PCM.SampleFmt y) => (PCM.Size, PCM.SampleFreq, PCM.Handle PCM.Interleaved Float) -> -} Ref HorValueSlider -> IO ()
playTone s = do
  -- setup for alsa pcm
  settings <- openPCM 
  p <- getValue s
  playSound settings ((double2Float p)*100+400) 2
  closePCM settings


ui :: IO ()
ui = do

  window <- windowNew
           (Size (Width 515) (Height 60))
           Nothing
           Nothing
  begin window
  b' <- buttonNew
        (Rectangle (Position (X 10) (Y 10)) (Size (Width 95) (Height 30)))
        (Just "Play")
  setLabelsize b' (FontSize 10)
  setCallback b' buttonCb

  s <- horValueSliderNew -- valueSliderNew 
        (Rectangle (Position (X 120) (Y 10)) (Size (Width 350) (Height 30)))
        (Just "Pitch")
  setLabelsize s (FontSize 10)
  setCallback s playTone

  end window
  showWidget window

main :: IO ()
main = ui >> FL.run >> FL.flush

replMain :: IO ()
replMain = ui >> FL.replRun
