module Main where

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Control.Exception (bracket, )
import Control.Monad (replicateM_)
import Foreign.Marshal.Array (newArray, peekArray)
import Data.Int (Int16)
import Data.List (uncons)
import Data.Maybe (catMaybes)
import System.IO (hGetContents, Handle, openFile, IOMode(..))

import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

-- import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB


import PlaySine
import Grains

--defining some types to help with documentation
type Pitch = Double
type Duration = Double



-- for some reason, this doesn't work with the 32 bit files produced by grandorgue
-- make 16 bit files with audacity
-- the pitch is shifted.  Maybe check the frame rate (or just ignore it...)
main :: IO ()
main = do
  -- do the conversation to svl here
  -- treble <- SVL.pack SVL.defaultChunkSize <$> linearEnvelope 8000 <$> accordionTrebleGrain
  -- enveloped = zipWith envelope [0..88200] treble

  low <- SVL.pack SVL.defaultChunkSize <$> accordionBassGrain 8000
  high <- SVL.pack SVL.defaultChunkSize <$> accordionTrebleGrain 8000

  envelopedh <- linearEnvelope 20 <$> accordionTrebleGrain 80
  let sequenced = SVL.pack SVL.defaultChunkSize $ overlapSequence 20 1000 envelopedh

  bracket openPCM closePCM $ \(size,rate,h) -> do
    print rate
    print size
    -- replicateM_ 10 $ playBuffer h high
    -- playBuffer h low
    -- replicateM_ 10 $ playBuffer h envelopedh
    playBuffer h sequenced

  print "done"

-- only call this inside the bracket of openPCM and closePCM
-- playBuffer :: [Int16] -> IO ()
playBuffer h v = do
  mapM_ (write h) $ SVL.chunks v

