# granular-synthesis

The project in this folder does granular synthesis.
The main project file is GranularSynthesis.hs.
Probably the easiest way to run it is with ghci, and try out the functions within ghci.
    > ghci GranularSynthesis
Within the prompt, run a command like:
    > playBuffer sound
Inside GranularSynthesis, sound is a list of Int16 that describes the sound what sound we play.
The function playBuffer takes that list of Int16 and plays it back.
This function comes from the module PlaySine, which has the infrastructure for using the alsa-pcm Haskell library to play back sounds.
It is modified from the example given at https://archives.haskell.org/code.haskell.org/alsa/pcm/examples/sine.hs
There is also an example function, playSines, which plays back sine waves of various frequencies.

## Grains

The base unit of a sound created with granular synthesis is the grain.
This grain can be from any kind of sound and is usually on the scale of a tenth of a second or less.
For my example grains, I started off with two accordion notes, one higher and one lower, as well as an organ flute.
Those sound sample files are wav files about two seconds long and are also found in this folder.
Additionally, there are two functions in GranularSynthesis, randomGrain and sineGrain, which generate grains from random noise and a sine wave, respectively.
The Grains module takes care of loading the wave information from the file, and taking a small portion of the file to be the grain.
The length of the grain is about 100 to 1000 samples, played back with a sample rate of 44100 samples per second.
Due to the way that the code works, the grain sample files must be 16 bit PCM and in mono.
I tried some files in stereo originally, and it halved the frequency of the playback, since both channels were being played interleaved.
To play back the sound grains by themselves:
    > playPlainGrain fluteGrain
    > playPlainGrain randomGrain

## Creation of a sound with ADSR

The first attempt that I made at creating a sound with grains was to put together a sequence of the same grain, repeated at regular intervals.
The result of this can be heard with:
    > playBuffer $ playSequence fluteGrain 5 200
The break between each grain and the subsequent one creates is what makes the main frequency.
This means that adjusting the grain size from the given size of 200 will change the pitch.
Notice that while replacing fluteGrain with randomGrain or another grain slightly changes the quality of the sound, it does not effect the pitch.
The frequency content from the original grain is not that important any more.

Next, there are envelopes on the grains to modify the sound.
The first one is a basic linear envelope, which makes each grain have a linear attack at the beginning and a linear decay at the end.
    > playBuffer $ singleNote fluteGrain 5 200

A more interesting envelope has the same linear attack, but an inverted linear decay, so that there is a break in the sound.
It gives the sound a harsher quality, because of the higher frequencies that come from the break in the sound.
    > playBuffer $ harshNote fluteGrain 5 200

Finally, making a complete note involves using ADSR, attack, decay, sustain and release.
This means that the sound has four parts.
On an electronic keyboard, the attack and decay would happen when pressing the key, the sustain happens as long as the key remains pressed, and the release happens when the key is released.
While the attack, decay and release are a fixed length, the sustain lasted however long the key is pressed.
By changing the parameters of each portion, different sounds can be made.