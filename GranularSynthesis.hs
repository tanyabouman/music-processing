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

-- this might be useful later, for now it's the base case of a fold
emptyGrain = Grain {grainLength=0, contents=[], startTime=0}

-- generalize this function for all types of grain
-- this is too slow to be feasible
-- fluteRegular :: Snd.Count -> Snd.Count -> Snd.Count -> IO Grain
-- fluteRegular grainLength toneLength frequency = do
--     base <- linearEnvelope (grainLength `div` 5) <$> fluteGrain grainLength
--     let
--       -- take at least enough start.
--       -- whatever is left over will be dealt with
--       starts = take (toneLength `div` frequency + 1) [0, frequency..]
--       inner st = Grain {grainLength=grainLength, contents=base, startTime=st}
--     return $ foldr mergeGrains emptyGrain $ map inner starts



-- for some reason, this doesn't work with the 32 bit files produced by grandorgue
-- make 16 bit PCM files with audacity
-- make sure to use mono tracks, since stereo changes the pitch
-- (and this doesn't require stereo anyways)
main :: IO ()
main = do
  -- do the conversation to svl here
  low <- SVL.pack SVL.defaultChunkSize <$> accordionBassGrain 8000
  high <- SVL.pack SVL.defaultChunkSize <$> accordionTrebleGrain 8000
  flute <- SVL.pack SVL.defaultChunkSize <$> fluteGrain 100

  -- this first one was really slow
  -- fluteMod <- fluteRegular 800 80000 100
  -- fluteMod <- fluteRegular 20 9000 20
  -- let fluteEnv = SVL.pack SVL.defaultChunkSize $ contents fluteMod

  -- now that enveloping works, this is much, much better
  -- applying the envelope to the whole things work well enough
  -- now put in some better asdr
  envelopedh <- linearEnvelope 20 <$> fluteGrain 200
  let sequenced = SVL.pack SVL.defaultChunkSize $ linearEnvelope 8000 $ overlapSequence 20 1000 envelopedh

  bracket openPCM closePCM $ \(size,rate,h) -> do
    print rate
    print size
    -- print fluteMod
    -- print fluteEnv
    -- playBuffer h fluteEnv
    -- replicateM_ 10 $ playBuffer h high
    -- replicateM_ 10 $ playBuffer h flute
    -- replicateM_ 10 $ playBuffer h low

    -- playBuffer h low
    -- replicateM_ 10 $ playBuffer h envelopedh
    playBuffer h sequenced

  print "done"

-- only call this inside the bracket of openPCM and closePCM
-- playBuffer :: [Int16] -> IO ()
playBuffer h v = do
  mapM_ (write h) $ SVL.chunks v

playBuffer' :: IO [Int16] -> IO ()
playBuffer' b = do
  svl <- svlp <$> b
  bracket openPCM closePCM $ \(size,rate,h) -> do
    mapM_ (write h) $ SVL.chunks svl

svlp = SVL.pack SVL.defaultChunkSize

-- for demonstration of the grains that I'm using
playPlainGrain :: (Snd.Count -> IO [Int16]) -> IO ()
playPlainGrain grain = do
  playBuffer' $ grain 80000
  -- playBuffer' g


-- make some examples here :: interesting parameters -> IO ()
singleNote :: (Snd.Count -> IO [Int16]) -> IO [Int16]
singleNote grainType = do
  envelopedh <- linearEnvelope 20 <$> grainType 100
  return $ linearEnvelope 10000 $ overlapSequence 20 5000 envelopedh
--TODO: better enveloping
-- make it depend on the frequency

--do something with the notes, exponentially


-- singleNote, except with multiple fading in and out



