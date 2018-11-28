--modified from https://stackoverflow.com/questions/5680075/bad-format-using-hsndfile-libsndfile

module GranSynth where

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Control.Exception (bracket, )
import Control.Monad (replicateM_)
import Foreign.Marshal.Array (newArray, peekArray)
import Data.Int (Int16)
import System.IO (hGetContents, Handle, openFile, IOMode(..))

import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

-- import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB


import PlaySine

--defining some types to help with documentation
type Pitch = Double
type Duration = Double

format :: Snd.Format
format = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

--this will be a half second sample for the first try
noteLength :: Double
noteLength = 5

volume = maxBound `div` 2 :: Int16

{-
data Snd.Info
  = Snd.Info {Snd.frames :: Snd.Count,
              Snd.samplerate :: Int,
              Snd.channels :: Int,
              Snd.format :: Snd.Format,
              Snd.sections :: Int,
              Snd.seekable :: Bool}
-}

openWavHandle :: [Int16] -> IO Snd.Handle
openWavHandle frames =
    let info = Snd.Info (length frames) 441000 1 format 1 False
    in Snd.openFile "temp.wav" Snd.WriteMode info


writeWav :: [Int16] -> IO Snd.Count
writeWav frames = do
  h <- openWavHandle frames
  ptr <- newArray frames
  c <- Snd.hPutBuf h ptr (length frames)
  Snd.hClose h
  return c

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

data GrainContents = AccordionHigh | AccordionLow

data Grain = Grain Snd.Count -- start time of the grain within the sound
                   Snd.Count -- length of the grain used
                   Snd.Count -- attack and delay times
                   GrainContents   -- contents of the grain

--there's a better way to store that...


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
