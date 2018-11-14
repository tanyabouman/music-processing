module Main where

import GenerateSamples (generateSample)
import PlayFile (playFile)

{-
This is the main program, which calls everything else and runs it.
Hopefully this file is quite short, since everything else comes
from other modules.
If this file gets too long, please make another module for the extra things.
-}


main = do
  --start with the setup of the application
  --this should probably also include a call to an fltk setup function

  --I don't actually know what is a good amount of samples to generate
  --it might be bettter to also generate samples of mixed frequencies,
  --rather than just editing the one base frequency
  let file = "new.wav"
  generateSample 440 10 file

  --some kind of loop which runs the program
  --look into jack or something
  
  --whatever teardown is necessary

