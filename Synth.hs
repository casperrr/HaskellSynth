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

wave :: Hz -> Seconds -> Audio
wave hz duration = map (sin . (*step)) [0.0 .. sampleRate * duration]
    where
        step = (hz*2*pi)/sampleRate

-- Maybe move this to another module with other effects like limiter and stuff
volume :: Float -> Audio -> Audio
volume v = map (*v)

