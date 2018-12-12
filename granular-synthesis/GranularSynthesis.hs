module Main where

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Control.Exception (bracket, )
import Control.Monad (replicateM_, replicateM)
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

import System.Random

import PlaySine
import Grains

--defining some types to help with documentation
type Pitch = Double
type Duration = Double

frameRate = 44100

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
    replicateM_ 10 $ mapM_ (write h) $ SVL.chunks high
    replicateM_ 10 $ mapM_ (write h) $ SVL.chunks flute
    replicateM_ 10 $ mapM_ (write h) $ SVL.chunks low
    replicateM_ 10 $ mapM_ (write h) $ SVL.chunks sequenced

  print "done"

-- only call this inside the bracket of openPCM and closePCM
-- playBuffer :: [Int16] -> IO ()
playBuffer' h v = mapM_ (write h) $ SVL.chunks v

-- for demonstration of the grains that I'm using
playPlainGrain :: (Snd.Count -> IO [Int16]) -> IO ()
playPlainGrain grain = do
  playBuffer $ grain 80000


-- the length is in seconds, the grainLength in frames
-- grainLength controls frequency as freq = 44100/grainLength
singleNote :: (Snd.Count -> IO [Int16]) -> Int -> Snd.Count -> IO [Int16]
singleNote grainType noteLength grainLength = do
  envelopedGrain <- linearEnvelope 20 <$> grainType grainLength
  return $ overlapSequence 20 (44100*noteLength `div` grainLength) envelopedGrain

-- the length is in seconds, the grainLength in frames
-- grainLength controls frequency as freq = 44100/grainLength
harshNote :: (Snd.Count -> IO [Int16]) -> Int -> Snd.Count -> IO [Int16]
harshNote grainType noteLength grainLength = do
  envelopedGrain <- chopEnvelope 20 <$> grainType grainLength
  return $ overlapSequence 20 (44100*noteLength `div` grainLength) envelopedGrain


-- make another note that doesn't overlap as well, then it should have a more interesting effect
-- not really, the interesting thing was the mixed up decay on each envelopes filter
-- (try smaller envelopes?)
detachedNote :: (Snd.Count -> IO [Int16]) -> Int-> Snd.Count -> IO [Int16]
detachedNote grainType noteLength grainLength = do
  envelopedGrain <- linearEnvelope 10 <$> grainType grainLength
  return $ overlapSequence 0 (44100*noteLength `div` grainLength) (envelopedGrain ++ replicate 5000 0)

-- make some examples here :: interesting parameters -> IO ()
singleNoteEnv :: (Snd.Count -> IO [Int16]) -> Int -> Snd.Count -> IO [Int16]
singleNoteEnv grainType noteLength grainLength =
  linearEnvelope (noteLength*8820) <$> singleNote grainType noteLength grainLength
--TODO: better enveloping
-- make it depend on the frequency

--do something with the notes, exponentially

-- playBuffer $ expAttackEnvelope <$>


-- singleNote, except with multiple fading in and out


-- a fade in for a sound. Just a bassGrain fading in
fadeIn :: Snd.Count -> IO [Int16]
fadeIn grainLength = linearAttack (2*44100) <$> singleNote accordionBassGrain 2 grainLength

sustain :: IO [Int16]
sustain = do
  bottom <- singleNote accordionBassGrain 15 200
  buzz <- map (`div` 3) <$> singleNoteEnv accordionTrebleGrain 2 100
  let buzzes = buzz ++ replicate (1*frameRate) 0 ++ buzz ++ replicate (frameRate `div` 2) 0 ++ buzz
  return $ zipWith (+) bottom buzzes
  -- return bottom

sustain2 :: IO [Int16]
sustain2 = do
  bottom <- singleNote accordionBassGrain 15 200
  buzz <- map (`div` 3) <$> singleNoteEnv accordionTrebleGrain 2 150
  let buzzes = replicate 0 100 ++ buzz ++ replicate (1*frameRate) 0 ++ buzz ++ replicate (frameRate `div` 2) 0 ++ buzz
  return $ zipWith (+) bottom buzzes

sustain3 :: IO [Int16]
sustain3 = do
  bottom <- singleNote accordionBassGrain 15 200
  buzzLow <- map (`div` 3) <$> singleNoteEnv accordionTrebleGrain 2 150
  buzzHigh <- map (`div` 3) <$> singleNoteEnv accordionTrebleGrain 2 100
  let buzzes = buzzLow ++ replicate (1*frameRate) 0 ++ buzzHigh ++ replicate (frameRate `div` 2) 0 ++ buzzLow
  return $ zipWith (+) bottom buzzes

-- putting all the pieces together
-- make this pitch dependant later
sound :: IO [Int16]
sound = do
  a <- fadeIn 200
  s <- sustain3
  let
    r = reverse a
    d = take 200 r

  return $ concat [a,d,s,r]

-- a sequence of the same grain repeated, without any enveloping
playSequence :: (Snd.Count -> IO [Int16]) -> Int -> Snd.Count -> IO [Int16]
playSequence grainType noteLength grainLength =
  overlapSequence 0 (44100*noteLength `div` grainLength) <$> grainType grainLength


randomGrain :: Int -> IO [Int16]
randomGrain size = do
  g <- newStdGen
  replicateM size (randomRIO (-10000::Int16, 10000))


sineGrain :: Int -> IO [Int16]
sineGrain size = do
  return $ (map (round . (2330*) . sin) [0,0.1..(fromIntegral size)])
