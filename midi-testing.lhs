> import qualified Data.ByteString.Lazy as B
> import Data.Binary.Get (runGet, getByteString, getWord32be, getWord16be, skip)
> import Data.Binary 
> import Data.Word
> import Control.Monad
> import Data.Bits

> filePath :: FilePath
> filePath = "dohotgirlslikechordsguitarsolo.mid"

> readBin :: FilePath -> IO B.ByteString
> readBin = B.readFile

> run = do
>     midi <- readBin filePath
>     print $ runGet parseMidi midi

> parseMidi = do
>     header <- headerChunk
>     track <- trackChunk
>     let Track trackData = track
>     events <- parseEvents $ fromIntegral (t_size trackData)
>     return [header, track, Events events]

> data IdkMidi = Header MidiHeader | Track TrackChunk | Events [Event] deriving Show
> type Midii = [IdkMidi]

> type DeltaTime = Word8
> type Event = (DeltaTime, Events)
> data Events = MidiEvent (Word8, Word8, Word8) | MetaEvent | SysExEvent deriving Show



% >     return Midi {
% >         header = header,
% >         track_chunk = track,
% >         events = [event]
% >     }

> data Midi = Midi {
>     header :: MidiHeader,
>     track_chunk :: TrackChunk
>     --events :: [Event]
> } deriving Show

------ MIDI File Structure ------ 

--- Header Chunk ---

TVRoZAAAAAYAAAABAGBNVHJrAAA=

Chunk ID
4d 54 68 64
M  T  h  d

Chunk Size
 0  0  0  6
00 00 00 06 = 6

Format Type
0 = single track file format
1 = multiple track file format
2 = multiple song file format
 0  0
00 00 = 0 Single Track

Number of Tracks
 0  1
00 01 = 1 Track

Time Division
 0 96
00 60 = 96


Total
ID          | Chunk Size  | Format| N trks| T-Div
M  T  h  d  |  0  0  0  6 |  0  0 |  0  1 |  0 96
4d 54 68 64 | 00 00 00 06 | 00 00 | 00 01 | 00 60

> data MidiHeader = MidiHeader {
>     h_id :: String, -- 4 Bytes
>     size :: Word32,
>     format :: Word16,
>     n_trks :: Word16,
>     t_div :: Word16
> } deriving Show

> headerChunk = do
>     h_id <- getByteString 4
>     size <- getWord32be
>     format <- getWord16be
>     n_trks <- getWord16be
>     t_div <- getWord16be
>     return $ Header MidiHeader {
>         h_id = show h_id,
>         size = size,
>         format = format,
>         n_trks = n_trks,
>         t_div = t_div}

% >     return (show h_id, size, format, n_trks, t_div)

--- Track Chunk ---

4d 54 72 6b 00 00 02 b9 00 ff 03 00 00 ff 58 04 04 02 24 08 00 ff 58 04 04 02 24 08

Chunk ID
M  T  r  k
4d 54 72 6b

Chunk Size
 0  0 512 185
00 00 02  b9  = 697

> data TrackChunk = TrackChunk {
>     t_id :: String, -- 4 Bytes
>     t_size :: Word32
> } deriving Show

> trackChunk = do
>     t_id <- getByteString 4
>     t_size <- getWord32be
>     return $ Track TrackChunk {
>         t_id = show t_id,
>         t_size = t_size
>     }

-- Event Array --

> parseVarLenThing :: Word8 -> Get Word8
> parseVarLenThing acc = do
>     byte <- getWord8
>     let acc' = (acc `shiftL` 7) .|. (byte .&. 0x7f)
>     if (byte .&. 0x80) == 1
>         then parseVarLenThing acc'
>         else return acc'
> parseVarLen :: Get Word8
> parseVarLen = parseVarLenThing 0

% > parseEvent = do
% >     deltaTime <- parseVarLen
% >     statusByte <- getWord8
% >     case statusByte of
% >         0xff -> do
% >             metaType <- getWord8
% >             len <- parseVarLen
% >             text <- getByteString (fromIntegral len)
% >             return ("Meta Event", deltaTime, metaType, text)
% >         0x90 -> do
% >             noteNumber <- getWord8
% >             velocity <- getWord8
% >             return ("Note On Event", deltaTime, ) -- Nah im going to bed but remeber to fix this pleasae like this is not a good method for parsing events
% >         _   -> do
% >             fail "Non-meta event parsing not implemented yet"

> parseEvents len = replicateM (len`div`4) parseEvent

> parseEvent = do
>     deltaTime <- parseVarLen
>     statusByte <- getWord8
>     case statusByte of
>         0x80 -> do -- Note Off Event
>             note <- getWord8
>             vel  <- getWord8
>             return (deltaTime, MidiEvent (statusByte, note, vel))
>         0x90 -> do -- Note On Event
>             note <- getWord8
>             vel  <- getWord8
>             return (deltaTime, MidiEvent (statusByte, note, vel))
>         _    -> do
>             skip 2
>             return (deltaTime, MetaEvent)

Delta Time 1
00 
 0 Meta Event

Status 
ff 03 = Track Name 
Len
00 = 0
Text = nul

Delta Time 2
00
Status
ff 58 = Time Signature

         |  4/ 2 |  
ff 58 04 | 04 02 | 24 08
2 = 2^-2 = 4
4/4

Delta Time 4
00 90 = Note On Event, NoteNumber, Velocity

00 90 | 4d | 64
 0 NO | F5 | 

C-1 = 0
4D = 77
77/12 = 6 <- 5
77%12 = 5 <- F

Mod = Note
Div = Octave+1

Delta Time 5
60 90 | 4a | 64
      | D5 | 


Delta Time 6
00 80 | 4d | 40
      | F5 | 


Delta Time 7
18 90 | 49  | 64
      | C#5 | 



00 80 4a 40 18 90 48 64 00 80 49 40

------ Pretty Printing ------

> printMidi :: Midii -> IO ()
> printMidi [] = return ()
> printMidi (Header h:xs) = do
>     putStrLn   "---- Header Chunk ---"
>     putStrLn $ "Midi Format: " ++ show (format h)
>     putStrLn $ "Number of Tracks: " ++ show (n_trks h)
>     putStrLn $ "Time Division: " ++ show (t_div h)
>     putStrLn "---------------------"
>     printMidi xs
> printMidi (Track t:xs) = do
>     putStrLn   "---- Track Chunk ----"
>     putStrLn $ "Track Size: " ++ show (t_size t)
>     putStrLn "---------------------"
>     printMidi xs
> printMidi (Events es:xs) = do
>     putStrLn "------ Events -------"
>     printEvents es
>     printMidi xs

> printEvents :: [Event] -> IO ()
> printEvents [] = return ()
> printEvents ((dt, MidiEvent (status, note, vel)):xs) = do
>     putStrLn "--- Midi Event ---"
>     putStrLn $ "Status: " ++ show status
>     putStrLn $ "Delta Time: " ++ show dt
>     putStrLn $ "Note: " ++ note2Name (fromIntegral note)
>     printEvents xs
> printEvents (_:xs) = printEvents xs

> showMidi :: IO ()
> showMidi = do
>    midi <- readBin filePath
>    let parsedMidi = runGet parseMidi midi
>    printMidi parsedMidi


> notes :: [String]
> notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

% > transNote :: Num a => a -> (String, Int)

> transNote :: Integral b => b -> (b, b)
> transNote n = (note, octave)
>   where
>     octave = n `div` 12 - 1
>     note   = n `mod` 12

> letterNote :: Int -> String
> letterNote n = notes !! n

> note2Name :: Int -> String
> note2Name n = letterNote note ++ show octave
>   where
>     (note, octave) = transNote n

> song :: IO Midii
> song = runGet parseMidi <$> readBin filePath