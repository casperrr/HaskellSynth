module Synth where

import Synth.Types

----------- Constants -----------------

masterVolume :: Float
masterVolume = 0.3

sampleRate :: Hz
sampleRate = 48000.0 

pitchStd :: Hz
pitchStd = 440.0 -- A4 standard pitch

----------- Note Generation -----------

wave :: Hz -> Seconds -> Wave
wave hz duration = map (sin . (*step)) [0.0 .. sampleRate * duration]
    where
        step = (hz*2*pi)/sampleRate

volume :: Float -> Wave -> Wave
volume v = map (*v)

