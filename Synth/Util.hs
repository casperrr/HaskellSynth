module Synth.Util where

import Data.ByteString.Builder (floatLE, toLazyByteString)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)
import System.Process (runCommand)

import Synth.Types ( Audio )
import Synth (wave, sampleRate)
import Text.Printf (printf)

----------- File Creation -----------

save :: FilePath -> Audio -> IO ()
save filePath audio = writeFile filePath $ toLazyByteString $ foldMap floatLE audio

play :: Audio -> IO ()
play audio = do
    save "output.bin" audio
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate "output.bin"
    return ()