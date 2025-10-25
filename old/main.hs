import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import System.Process
import Text.Printf
import Data.List (sort)

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beat = Float
type TimeSig = (Int, Int)

----------- Vairables -------------

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.3

sampleRate :: Samples
sampleRate = 48000.0

pitchStd :: Hz
pitchStd = 440.0

bpm :: Float
bpm = 60

timeSig :: TimeSig
timeSig = (4,4)

major :: [Float]
major = [0, 2, 4, 5, 7, 9, 11, 12]

minor :: [Float]
minor = shiftRoot major 4

seventh :: [Float] -> [Float]
seventh []          = []
seventh [n,m]       = [n,m]
seventh (n:m:ns)    = n:seventh ns

shiftRoot :: [a] -> Int -> [a]
shiftRoot xs     0 = xs
shiftRoot (x:xs) n = shiftRoot (xs++[x]) (n-1)

note :: Semitones -> Seconds -> [Pulse]
note n = freq (pitch n)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration = map ((*volume) . sin . (*step)) [0.0 .. sampleRate * duration]
  where
    step = (hz*2*pi)/sampleRate

wave :: [Pulse]
wave = concat [note i duration | i <- major]
  where
    duration = 0.5

trans :: Hz -> Hz
trans hz | hz > (pitchStd*2) = trans $ hz/2
         | otherwise         = hz

harmonicSeries :: [Hz]
harmonicSeries = map (*pitchStd) [1..]

-- A 440, A# 466.16, B 493.88, C 523.25, C# 554.37, D 587.33, D# 622.25, E 659.26, F 698.46, F# 739.99, G 783.99, G# 830.61, A 880.00
-- [440.0,880.0,    660.0,     880.0,       550.0,    660.0,    770.0]
-- [440.0,880.0,    1320.0,    1760.0,      2200.0,   2640.0,   3080.0]

testScale :: Int -> [Hz]
testScale n = sort $ map trans $ take n harmonicSeries

chromatic :: Int -> [Hz]
chromatic n 
    | n <= 1 = [pitchStd]
    | otherwise = [pitchStd + (pitchStd * i / fromIntegral (n-1)) | i <- [0..fromIntegral (n-1)]]

pitch :: Semitones -> Hz
pitch n = pitchStd*(2**(1.0/12.0))**n

edo :: [Hz]
edo = map pitch [0..12]


-- (440.0 * 2 * pi) / sampleRate

save :: FilePath -> IO ()
save filePath = BSL.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE wave

-- ffplay -f f32le -ar 48000 output.bin

play :: IO ()
play = do
    save outputFilePath
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()