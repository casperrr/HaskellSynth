import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import System.Process
import Text.Printf
import Data.List (sort)
import System.Random

----------- Types -----------------

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
type WaveForm = Float -> Float

-- data Instrument = Instrument 

-- I want a music data type and an instrument data type which i can use music to describe the music the notes and whatnot and then apply and instrument to it to create the actual sound.
-- data Music = Paralel [Music] | Seq Music Music | Loop Int Music | Note (Float -> Float -> Instrument -> Note)

-- testMusic :: Music
-- testMusic = 

-- note'' :: Float -> Float -> Instrument -> Note
-- note'' 

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

inf :: t -> [t]
inf x = x:inf x

--------------- Envolope Generation -----------------

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

--------------- Note Generation ------------------

note :: Semitones -> Beats -> Note
note n b = wave (pitch n) $ b*beatDuration

-- note' :: Semitones -> Beats -> WaveForm -> Note
-- note' st b = wave' (pitch st) (b*beatDuration)


rest :: Beats -> Note
rest b = replicate (floor $ sampleRate*b*beatDuration+1) 0.0

noise :: Beats -> Note
noise b = take (floor $ sampleRate*b*beatDuration) $ randomRs (-1.0, 1.0) (mkStdGen 42)

chord :: [Semitones] -> Beats -> Note
chord ns b = combine $ map (`note` b) ns


wave :: Hz -> Seconds -> [Pulse]
wave hz duration = adsr testADSR $ map ((*volume) . squareWaveRec 8 . (*step)) [0.0 .. sampleRate * duration]
  where
    step = (hz*2*pi)/sampleRate

-- wave' :: Hz -> Seconds -> WaveForm -> [Pulse]
-- wave' hz d wf = map ((*volume) . wf . step) [0.0 .. sampleRate*d]
--   where step = (*((hz*2*pi)/sampleRate))

pitch :: Semitones -> Hz
pitch n = pitchStd*(2**(1.0/12.0))**n

squareWaveRec :: Float -> WaveForm
squareWaveRec 0 x = sin x
squareWaveRec n x = ((1/(n*2+1))*sin(x*(n*2+1))) + squareWaveRec (n-1) x

squareWave :: WaveForm
squareWave x = if sin x >= 0 then 1 else -1

------------- Mixing -----------------

pad :: (Ord a, Num a) => [[a]] -> [[a]]
pad xs = map (\x -> (++ replicate ((length . maximum $ xs)-length x) 0) x) xs

mix :: (Num a, Ord a) => [[a]] -> [a]
mix = foldr (zipWith (+)) (inf 0) . pad

norm :: (Fractional b, Ord b) => [b] -> [b]
norm xs = map (/ maximum xs) xs

combine :: [Song] -> Song
combine = norm . mix

------------- Filtering ------------------

-- lowPass :: Float -> Song -> Song
lowPass _     []     = []
lowPass alpha (x:xs) = result
  where
    result = scanl next x xs
    next prev currX = alpha * currX + (1 - alpha) * prev

-- lowPass 0.5 [0..5]
-- lowPass 0.5 [0, 1, 2, 3, 4, 5]
-- scanl next 0 [1,2,3,4,5]
-- scanlGo next 0 [1,2,3,4,5]
-- 0 : scanlGo next (next 0 1) [2,3,4,5]
-- 0 : scanlGo next (0.5 * 1 + (1 - 0.5) * 0) [2,3,4,5]
-- 0 : scanlGo next (0.5) [2,3,4,5]
-- 0 : 0.5 : scanlGo next (next 0.5 2) [3,4,5]
-- 0 : 0.5 : scanlGo next (1.25) [3,4,5]
-- 0 : 0.5 : 1.25 : scanlGo next (2.125) [4,5]
-- 0 : 0.5 : 1.25 : 2.125 : scanlGo next (next (2.125) 4) [5]
-- 0 : 0.5 : 1.25 : 2.125 : scanlGo next (3.0625) [5]
-- 0 : 0.5 : 1.25 : 2.125 : 3.0625 : scanlGo next (next (3.0625) 5) []
-- 0 : 0.5 : 1.25 : 2.125 : 3.0625 : 4.03125 : []
-- [0, 0.5, 1.25, 2.125, 3.0625, 4.03125]

------------- Delay ------------------

-- Delay:
-- Delay, Feedback, Song
-- Take window of size ds 
-- Multiply it by f
-- Apply it every ds 

-- [1,2,3,4,5,6,7,8,9,10] `splitAt` 2
-- ([1,2],[3,4,5,6,7,8,9,10])
-- map (*f=0.5) [1,2]
-- win' = [0.5, 1.0]
-- [3,4,5,6,7,8,9,10] splitAt 2
-- ([3,4],[5,6,7,8,9,10])

-- (win,tal) splitAt 2 = ( [1,2],[3,4,5,6,7,8,9,10] )
-- length xs = 10
-- length tal = 8
-- 10 - 8 = 2
-- replicate (length xs - length tal) 0.0
-- [0.0, 0.0] ++ feedback win
-- feedback ls = let ls' = map (*0.5) ls in ls' ++ feedback ls'

-- replicate (length xs - length tal) 0.0 ++ feedback win

getLists _ _ [] = []
getLists d f xs = (padd ++ feedback win) : getLists d f tal
  where
    -- ds = floor $ d*beatDuration*sampleRate
    (win,tal) = splitAt d xs
    feedback ls = let ls' = map (*f) ls in ls' ++ feedback ls'
    padd = replicate (length xs - length tal) 0.0
    

-- delay :: Float -> Float -> Song -> Song
delay d f s = foldr (zipWith (+)) s $ getLists ds f s 
  where
    ds = floor $ d * beatDuration * sampleRate

-- BTW THIS DOENST WORK AT ALL

-- fuckk d f s = 




-- combine
-- [3.5,5,5,6,7,8,9,10] splitAt 3
-- ([3.5,5],[5,6,7,8,9,10])
-- map (*f=0.5) [3.5,5]
-- [1.75, 2.5]
-- combine
-- [1.75, 2.5, 5, 6, 7, 8, 9, 10]
-- splitAt 3 [1.75, 2.5, 5, 6, 7, 8, 9, 10]
-- ([1.75,2.5],[5,6,7,8,9,10])





    


--------------- Songs --------------------

-- Edit this so its a list of notes that u can map over and stuff

aMajorScale :: Song
aMajorScale = concat [note i 1.0 | i <- concat (replicate 3 [0, 2, 4, 5, 7, 9, 11, 12])]

(>*) :: Int -> [a] -> [a]
n >* xs = concat $ replicate n xs
infixr 5 >*
(|>) :: Monoid m => m -> m -> m
(|>) = mappend
infixr 4 |>
 
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

testSong :: Song
testSong = combine [aMajorScale, note 12 1.0 |> aMajorScale]

-- testSong' :: Int -> [Song]
testSong' :: Int -> [Song]
testSong' 0 = []
testSong' n = [rest (0.25 * fromIntegral i) |> aMajorScale | i <- [1..n]]

-- type FUCK = [Note]

-- mix :: [Song]

chordMajor :: [Float]
chordMajor = [0, 4, 7] -- 1, 3, 5
chordMajor7 :: [Float]
chordMajor7 = chordMajor ++ [7] -- 1, 3, 5, 7

chordProg1 :: Song
chordProg1 = 8 >* 
  (chord chordMajor7 4.0            |> chord (map (+7) chordMajor7) 4.0 |>
   chord (map (+3) chordMajor7) 4.0 |> chord (map (+5) chordMajor7) 4.0)

------------- Util ---------------- 

save :: FilePath -> Song -> IO ()
save filePath song = BSL.writeFile filePath $ B.toLazyByteString $ foldMap B.floatLE song

play :: Song -> IO ()
play song = do
    save outputFilePath song
    _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
    return ()