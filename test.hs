import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import System.Process
import Text.Printf
import Data.List (sort)

type Pulse = Float
type Level = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type TimeSig = (Int, Int)
type ADSR = (Seconds, Seconds, Level, Seconds)
type Beats = Float
type Note = [Pulse]
type Song = [Pulse]

----------- Vairables -------------

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.4

sampleRate :: Samples
sampleRate = 48000.0

pitchStd :: Hz
pitchStd = 220.0

bpm :: Beats
bpm = 120

timeSig :: TimeSig
timeSig = (4,4)

beatDuration :: Seconds
beatDuration = 60.0/bpm

adsr :: ADSR -> [Pulse] -> [Pulse]
adsr (a, d, s, r) n = applyEnd (ramp r ++ sus 1.0) $ apply ads n 
  where
    ads          = ramp a ++ reverse (rampRange d s 1.0) ++ sus s
    apply        = zipWith (*)
    applyEnd f w = reverse $ apply f $ reverse w
    sus      l   = l:sus l

testADSR :: ADSR
testADSR = (0.01, 0.0, 1.0, 0.02)

ramp :: Seconds -> [Pulse]
ramp 0 = []
ramp t = [i / (sampleRate*t) | i <- [0.0..sampleRate*t]]

rampRange :: Seconds -> Float -> Float -> [Pulse]
rampRange t n m = map ((+n).(*(m-n))) $ ramp t

note :: Semitones -> Beats -> Note
note n b = freq (pitch n) $ b*beatDuration

rest :: Beats -> Note
rest b = replicate (floor $ sampleRate*b*beatDuration+1) 0.0

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = adsr testADSR $ map ((*volume) . sin . (*step)) [0.0 .. sampleRate * duration]
  where
    step = (hz*2*pi)/sampleRate

pitch :: Semitones -> Hz
pitch n = pitchStd*(2**(1.0/12.0))**n

aMajorScale :: Song
aMajorScale = concat [note i 1.0 | i <- concat (replicate 3 [0, 2, 4, 5, 7, 9, 11, 12])]

(>*) :: Int -> [a] -> [a]
n >* xs = concat $ replicate n xs
infixr 5 >*
(|>) :: Monoid m => m -> m -> m
(|>) = mappend
infixr 4 |>

-- .. .. - .. | .. .. - .. .. .. - | 
song1 :: Song
song1 = ssp1 |> 8 >* ssp2

ssp1 :: Song
ssp1 = 
  (2 >* ((4 >* note 12 0.25) |> note 12 0.5 |> rest 5.5 |> note 15 1.0)) |> 
  (4 >* ((4 >* note 12 0.25) |> note 12 0.5 |> rest 0.5)) |>
  (8 >* (4 >* note 12 0.25))

ssp2 :: Song
ssp2 = 
    s2 12 |> s1 12 |> -- 3.5
    s3 17 |> s1 17 |> -- 2
    s3 15 |> s1 15 |> -- 2
    note 10 0.5 |>    -- 0.5
    2>*(s2 12 |> s1 12 |> -- 3.5
    note 17 0.5)     
  where
    s1 n = 4 >* note n 0.25 |> note n 0.5 -- 1.5  .... -
    s2 n = s1 n |> (2 >* note n 0.25) -- 2  .... - ..
    s3 n = 2>*note n 0.25 -- 0.5

save :: FilePath -> Song -> IO ()
save filePath song = BSL.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE song

play :: Song -> IO ()
play song = do
    save outputFilePath song
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()