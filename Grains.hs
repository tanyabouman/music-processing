--modified from https://stackoverflow.com/questions/5680075/bad-format-using-hsndfile-libsndfile

module Grains ( Grain
              , linearEnvelope
              , accordionBassGrain
              , accordionTrebleGrain
              , fluteGrain
              , overlapSequence
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

-- this loads the file at the given file path into a lazy vector
loadFile :: Snd.Count -> FilePath -> IO [Int16]
loadFile nFrames fp = do
  fileh <- Snd.openFile fp Snd.ReadMode Snd.defaultInfo

  -- so get the information first, before assigning the size
  let 
    info = Snd.hInfo fileh
    frames = Snd.frames info
  -- print frames
  ptr <- newArray $ replicate frames 0 -- (0 :: Int)
  ptrMaybe <- peekArray frames ptr
  -- print ptrMaybe
  -- read <- Snd.readFile "accordionlow"

  size <- Snd.hGetBuf fileh ptr (frames)
  Snd.hClose fileh

  -- now put the buffer into an array that the alsa can use.
  -- the formal is storable vector Lazy
  -- peek works for any storage, so hopefully this can work
  buffer <- peekArray frames ptr
  -- print svlMaybe
  -- let 
  --   svl :: SVL.Vector Int16 
  --   svl = SVL.pack SVL.defaultChunkSize $ take 88200 $ svlMaybe
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
      then round ((fromIntegral i)*slope) * v
      else if i > decay
      then round ((fromIntegral (decay-i))*slope) * v
      else v
  in
    zipWith linearEnvelope' [0..n] v

-- guassian?? envelope function


-- load the file
-- apply the envelope to it
-- the envelope will probably be the same for all the grains
-- so it will be a separate function
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
           grain :: GrainContents}


-- list the grains

-- order grains by the start time
-- orderBy startTime

-- add together

-- this is entirely unreadable.  but it should work
mergeGrains :: [Grain] -> [[Int16]] -> Snd.Count  -> [Int16]
mergeGrains (g:gs) existingGrains currentTime =
  let
    (currents, futures) = unzip $ catMaybes $ map uncons existingGrains
  in
    if (startTime g - 1) > currentTime
    then sum currents : mergeGrains (g:gs) futures (currentTime-1)
    else mergeGrains gs (grainContent (grain g) : futures) currentTime



overlapSequence :: Snd.Count -> Int -> [Int16] -> [Int16]
overlapSequence o n v = foldr (overlap o) [] (replicate n v)

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