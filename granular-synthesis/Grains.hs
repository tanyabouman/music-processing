--modified from https://stackoverflow.com/questions/5680075/bad-format-using-hsndfile-libsndfile

module Grains ( Grain(Grain,startTime, grainLength, contents)
              , linearEnvelope
              , expAttackEnvelope
              , linearAttack
              , accordionBassGrain
              , accordionTrebleGrain
              , fluteGrain
              , overlapSequence
              , overlap
              , mergeGrains
              ) where

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

--defining some types to help with documentation
type Pitch = Double
type Duration = Double

{-
data Snd.Info
  = Snd.Info {Snd.frames :: Snd.Count,
              Snd.samplerate :: Int,
              Snd.channels :: Int,
              Snd.format :: Snd.Format,
              Snd.sections :: Int,
              Snd.seekable :: Bool}
-}

-- this loads the file at the given file path into a list
-- only the number of frames specified are return, since the
-- grain itself might be smaller than the original sample
loadFile :: Snd.Count -> FilePath -> IO [Int16]
loadFile nFrames fp = do
  fileh <- Snd.openFile fp Snd.ReadMode Snd.defaultInfo

  -- so get the information first, before assigning the size
  let 
    info = Snd.hInfo fileh
    frames = Snd.frames info
  -- print frames
  ptr <- newArray $ replicate frames 0 -- (0 :: Int)

  size <- Snd.hGetBuf fileh ptr (frames)
  Snd.hClose fileh

  -- now put the buffer into an array that the alsa can use.
  -- the formal is storable vector Lazy
  -- peek works for any storage, so hopefully this can work
  buffer <- peekArray frames ptr
  -- print svlMaybe
  return $ take nFrames $ buffer

-- now the number of frames needs to be passed along as well
-- wrap this function inside a [Int16] -> [Int16]
linearEnvelope :: Snd.Count -> [Int16] -> [Int16]
linearEnvelope attack v =
  let
    slope = 1 / (fromIntegral attack)
    n = length v
    decay = n - attack
    linearEnvelope' :: Int -> Int16 -> Int16
    linearEnvelope' i v =
      if i < attack
      then round ((fromIntegral i)*slope * fromIntegral v)
      else if i > decay
      then round ((fromIntegral (decay-i))*slope * fromIntegral v) + v
      else v
  in
    zipWith linearEnvelope' [0..n] v

linearAttack :: Snd.Count -> [Int16] -> [Int16]
linearAttack attack v =
  let
    slope = 1 / (fromIntegral attack)
    n = length v
    linearAttack' :: Int -> Int16 -> Int16
    linearAttack' i v =
      if i < attack
      then round ((fromIntegral i)*slope * fromIntegral v)
      else v
  in
    zipWith linearAttack' [0..] v

-- starts the decay at the beginning of the given sample
linearDecay :: Snd.Count -> [Int16] -> [Int16]
linearDecay decay v =
  let
    slope = 1 / (fromIntegral decay)
    linearDecay' :: Int -> Int16 -> Int16
    linearDecay' i v =
      if i < decay
      then round ((fromIntegral i)*slope * fromIntegral v)
      else v
  in
    zipWith linearDecay' [0..] v

-- exponential attack
-- only does the attack portion, because we aren't really concerned about the rest
expAttackEnvelope :: Snd.Count -> [Int16] -> [Int16]
expAttackEnvelope attackLength v =
  let
    scalar = 1 / (exp (fromIntegral attackLength))
    attackEnvelope :: Int -> Int16 -> Int16
    attackEnvelope i v =
      if i < attackLength
      then round (fromIntegral v * scalar * exp (fromIntegral i))
      else v
  in
    zipWith attackEnvelope [0..] v

-- load the file, based on the number of frames necessary
accordionBassGrain :: Snd.Count -> IO [Int16]
accordionBassGrain frameCount = loadFile frameCount "accordionlow.wav"

accordionTrebleGrain :: Snd.Count -> IO [Int16]
accordionTrebleGrain frameCount = loadFile frameCount "accordionhigh.wav"

fluteGrain :: Snd.Count -> IO [Int16]
fluteGrain frameCount = loadFile frameCount "flute.wav"

data GrainContents = AccordionHigh | AccordionLow

grainContent :: GrainContents -> [Int16]
grainContent AccordionHigh = []
grainContent AccordionLow = []

data Grain
  = Grain {startTime :: Snd.Count,
           grainLength :: Snd.Count,
           -- attackLength :: Snd.Count, -- this makes this complicated
           contents :: [Int16]}
           -- grain :: GrainContents}
           deriving (Eq, Show)

-- list the grains

-- order grains by the start time
-- orderBy startTime

-- add together
-- (this might be better that what's below??? except it doesn't have all the possibilities)
mergeGrains :: [Grain] -> [[Int16]] -> Snd.Count  -> [Int16]
mergeGrains (g:gs) existingGrains currentTime =
  let
    (currents, futures) = unzip $ catMaybes $ map uncons existingGrains
  in
    if (startTime g - 1) > currentTime
    then sum currents : mergeGrains (g:gs) futures (currentTime-1)
    else mergeGrains gs (contents g : futures) currentTime



overlapSequence :: Snd.Count -> Int -> [Int16] -> [Int16]
overlapSequence o n v = foldr (overlap o) [] (replicate n v)

-- this is the correct function, and it doesn't really matter how it works on the
-- inside.  It just needs to work for all cases. and not be slow
-- mergeGrains :: Grain -> Grain -> Grain
-- mergeGrains g1 g2
--   | startTime g1 + grainLength g1 <= startTime g2 =
--     let
--       padding = startTime g2 - (startTime g1 + grainLength g1)
--       newContents = contents g1 ++ replicate padding 0 ++ contents g2
--       newLength = grainLength g1 + padding + grainLength g2
--     in
--       Grain {startTime=startTime g1, grainLength=newLength, contents=newContents}
--   | startTime g2 + grainLength g2 <= startTime g1 =
--     let
--       padding = startTime g1 - (startTime g2 + grainLength g2)
--       newContents = contents g2 ++ replicate padding 0 ++ contents g1
--       newLength = grainLength g2 + padding + grainLength g1
--     in
--       Grain {startTime=startTime g2, grainLength=newLength, contents=newContents}
--   | startTime g1 < startTime g2 =
--     let
--       (first,rest) = splitAt (startTime g2) (contents g1)
--     in
--       if startTime g1 + grainLength g1 < startTime g2 + grainLength g2
--         then
--           let
--             (second, third) = splitAt (startTime g1 + grainLength g1) (contents g2)
--             newContents = first ++ zipWith (+) second rest ++ third
--           in
--             Grain {startTime=startTime g1, grainLength = length newContents, contents=newContents}
--         else
--           let
--             (second, third) = splitAt (grainLength g2) rest
--             newContents = first ++ zipWith (+) second (contents g2) ++ third
--           in Grain {startTime=startTime g1, grainLength=length newContents, contents=newContents}
--   | otherwise =
--     let
--       (first,rest) = splitAt (startTime g1) (contents g2)
--     in
--       if startTime g2 + grainLength g2 < startTime g1 + grainLength g2
--         then
--           let
--             (second,third) = splitAt (startTime g2 + grainLength g2) (contents g1)
--             newContents = first ++ zipWith (+) second rest ++ third
--           in
--             Grain {startTime=startTime g2, grainLength=length newContents, contents=newContents}
--         else
--           let
--             (second,third) = splitAt (grainLength g1) rest
--             newContents = first ++ zipWith (+) second (contents g1) ++ third
--           in
--             Grain {startTime=startTime g2, grainLength=length newContents, contents=newContents}




-- takes two samples and puts them on top of each other, separated by an amount
-- assumes that the two grains are equal length; this could be bad
-- overlap' :: Snd.Count -> [Int16] -> [Int16] -> [Int16] 
-- overlap' offset v1 v2 =
--   if offset == 0
--   then zipWith (+) v1 v2
--   else if offset > 0
--   then
--     let
--       (first, rest) = splitAt offset v1
--     in
--       first ++ zipWith (+)

overlap :: Snd.Count -> [Int16] -> [Int16] -> [Int16]
overlap n v1 v2 =
  let
    (first,end) = splitAt (length v1 - n) v1
    (begin,last) = splitAt n v2
  in
    first ++ zipWith (+) begin end ++ last

readHandle :: IO Snd.Handle
readHandle =
  Snd.openFile "temp.wav" Snd.ReadMode Snd.defaultInfo
