module Synth.Util where

import Data.ByteString.Builder (floatLE, toLazyByteString, word32LE, word16LE, int16LE, string8)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)
import System.Process (runCommand)
import Text.Printf (printf)
import Data.Int (Int16)
import Data.Word (Word32)

import Synth.Types ( Audio, Sample )
import Synth (wave, sampleRate)

----------- File Creation -----------

-- Saves the audio as a raw 32-bit float little-endian file
saveRaw :: FilePath -> Audio -> IO ()
saveRaw filePath audio = writeFile filePath $ toLazyByteString $ foldMap floatLE audio

playRaw :: Audio -> IO ()
playRaw audio = do
    saveRaw "output.bin" audio
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate "output.bin"
    return ()

------ WAV File Creation ------

clamp :: Sample -> Sample
clamp x = max (-1.0) (min 1.0 x)

floatToInt16 :: Sample -> Int16
floatToInt16 s = round (clamp s * 32767.0)

saveWav :: FilePath -> Audio -> IO ()
saveWav filePath samples = writeFile filePath $ toLazyByteString builder
    where
        builder = mconcat [
            ---- RIFF Header ----
            string8 "RIFF", -- ChunkID
            word32LE (4 + (8 + 16) + (8 + dataBytes)), -- ChunkSize, Overall size of file 32-Int
            string8 "WAVE", -- Format
            ---- Format Chunk ----
            string8 "fmt ", -- Subchunk1ID
            word32LE 16, -- Subchunk1Size, PCM = 16
            word16LE 1, -- AudioFormat, PCM = 1
            word16LE 1, -- NumChannels, Mono = 1
            word32LE sr, -- SampleRate
            word32LE (sr * (16 `div` 8)), -- ByteRate = SampleRate * NumChannels * BitsPerSample/8
            word16LE (16 `div` 8), -- BlockAlign = NumChannels * BitsPerSample/8
            word16LE 16, -- BitsPerSample
            ---- Data Chunk ----
            string8 "data", -- Subchunk2ID
            word32LE dataBytes, -- Subchunk2Size = NumSamples * NumChannels * BitsPerSample/8
            foldMap (int16LE . floatToInt16) samples -- Actual sound data
         ]

        -- The size of the data section in bytes
        dataBytes :: Word32
        dataBytes = fromIntegral $ length samples * (16 `div` 8)

        sr :: Word32
        sr = round sampleRate

play :: Audio -> IO ()
play audio = do
    saveWav "output.wav" audio
    _ <- runCommand $ printf "ffplay -showmode 1 -f wav %s" "output.wav"
    return ()