--modified from https://stackoverflow.com/questions/5680075/bad-format-using-hsndfile-libsndfile

import qualified Sound.File.Sndfile as Snd
import Control.Applicative
import Foreign.Marshal.Array
import Data.Int (Int16)
import System.IO (hGetContents, Handle, openFile, IOMode(..))

format :: Snd.Format
format = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile

frameRate :: Int
frameRate = 16000

noteLength :: Double
noteLength = 3

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

main :: IO ()
main = makeWavFile

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