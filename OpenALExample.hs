-- modified from https://wiki.haskell.org/OpenAL

import Control.Monad ( unless )
import Control.Concurrent ( threadDelay )
import System.IO ( hPutStrLn, stderr )
import Sound.OpenAL

main :: IO ()
main = do
   maybeDevice <- openDevice Nothing -- (Just "'( ( devices '( native null ) ) )")
   case maybeDevice of
      Nothing -> hPutStrLn stderr "openDevice failed"
      Just device -> do
        createContext device []
        threadDelay 1
        ok <- closeDevice device
        unless ok $
          hPutStrLn stderr "closeDevice failed"
