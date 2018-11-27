--modified from https://stackoverflow.com/questions/5680075/bad-format-using-hsndfile-libsndfile

module GranSynth (generateSample, Pitch, Duration) where

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Foreign.Marshal.Array (newArray, peekArray)
import Data.Int (Int16)
import System.IO (hGetContents, Handle, openFile, IOMode(..))

import qualified Sound.ALSA.PCM.Node.ALSA as PCM
import qualified Sound.ALSA.PCM.Parameters.Software as SwParam
import qualified Sound.ALSA.PCM.Parameters.Hardware as HwParam

-- import qualified Sound.ALSA.PCM.Debug as Debug

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector.Base as SVB
import Control.Exception (bracket, )

import PlaySine

--defining some types to help with documentation
type Pitch = Double
type Duration = Double

--for now, this doesn't return anything,
--but it's possible that information about the number of frames might
--become necessary
--I intend for now only to use the standard frame rate used here
generateSample :: Pitch -> Duration -> FilePath -> IO ()
generateSample p d fp =
  let
    sample = noteToSample p d
    info = Snd.Info (length sample) 441000 1 format 1 False
  in do
    h <- Snd.openFile fp Snd.WriteMode info
    ptr <- newArray sample
    c <- Snd.hPutBuf h ptr (length sample)
    Snd.hClose h

format :: Snd.Format
format = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

frameRate :: Int
frameRate = 16000

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

noteToSample :: Double -> Double -> [Int16]
noteToSample freq noteLength =
    take (round $ noteLength * fromIntegral frameRate) $
    map ((round . (* fromIntegral volume)) . sin) 
    [0.0, (freq * 2 * pi / fromIntegral frameRate)..]

writeWav :: [Int16] -> IO Snd.Count
writeWav frames = do
  h <- openWavHandle frames
  ptr <- newArray frames
  c <- Snd.hPutBuf h ptr (length frames)
  Snd.hClose h
  return c

makeWavFile :: IO ()
makeWavFile = writeWav (noteToSample 440 noteLength) >>= \c ->
          putStrLn $ "Frames written: " ++ show c


-- this loads the file at the given file path into a lazy vector
loadFile :: FilePath -> IO [Int16]
loadFile fp = do
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
  return $ take 88200 $ buffer

-- envelope :: SVL.Vector Int16 -> SVL.Vector Int16
-- envelope vs = 
--   where
--     envelope' = [0,0.005..1] ++ replicate 87800 1 ++ [1,0.995..0]


-- the first number is the index, the second the value
-- FIXME: don't hardcode the values of the slope, etc.
envelope :: Int -> Int16 -> Int16
envelope i v 
  | i < 200 = round ((fromIntegral i)*(0.005)) * v
  | i > 87800 = round ((fromIntegral (88200-i))*0.005) * v
  | otherwise = v


-- load the file
-- apply the envelope to it
-- the envelope will probably be the same for all the grains
-- so it will be a separate function
accordionBassGrain :: IO [Int16]
accordionBassGrain = loadFile "accordionlownew.wav"

accordionTrebleGrain :: IO [Int16]
accordionTrebleGrain = loadFile "accordionhighnew.wav"


-- for some reason, this doesn't work with the 32 bit files produced by grandorgue
-- make 16 bit files with audacity
-- the pitch is shifted.  Maybe check the frame rate (or just ignore it...)
main :: IO ()
main = do
  -- do the conversation to svl here
  treble <- SVL.pack SVL.defaultChunkSize <$> zipWith envelope [0..88200] <$> accordionTrebleGrain
  -- enveloped = zipWith envelope [0..88200] treble

  low <- SVL.pack SVL.defaultChunkSize <$> accordionBassGrain
  high <- SVL.pack SVL.defaultChunkSize <$> accordionTrebleGrain


  bracket openPCM closePCM $ \(size,rate,h) -> do
    print rate
    print size
    mapM_ (write h) $ 
      SVL.chunks high
    -- mapM_ (write h) $ 
    --   SVL.chunks low
    mapM_ (write h) $ 
      SVL.chunks high

  print "done"

filter :: Int -> Float
filter i 
  | i < 200 = (fromIntegral i)*0.005
  | i > 87800 = (fromIntegral (87800 - i))*0.005
  | otherwise = fromIntegral i


readHandle :: IO Snd.Handle
readHandle =
  Snd.openFile "temp.wav" Snd.ReadMode Snd.defaultInfo

readFile :: IO ()
readFile = do
  h <- readHandle
  let cnt = round $ noteLength * fromIntegral frameRate
  ptr <- newArray $ replicate cnt (0 :: Int16)
  size <- Snd.hGetBuf h ptr cnt
  buffer <- peekArray cnt ptr
  print size


