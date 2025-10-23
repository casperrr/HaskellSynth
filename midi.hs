import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Binary
import Data.Binary.Get
import Data.Word
import Control.Monad
import Data.Bits

filePath :: FilePath
filePath = "dohotgirlslikechordsguitarsolo.mid"

readBin :: FilePath -> IO BL.ByteString
readBin = BL.readFile

-- "MThd\NUL\NUL\NUL\ACK\NUL\NUL\NUL\SOH\NUL`MTrk\NUL\NUL\STX\185\NUL\255\ETX\NUL\NUL\255X\EOT\EOT\STX$\b\NUL\255X\EOT\EOT\STX$\b\NUL\144Md`\144Jd\NUL\128M@\CAN\144Id\NUL\128J@\CAN\144Hd\NUL\128I@\CAN\144Fd\NUL\128H@\CAN\144Ed\NUL\128F@\CAN\128E@\NUL\144Fd\CAN\128F@\NUL\144Hd\CAN\128H@\NUL\144Jd\CAN\144Hd\NUL\128J@\CAN\144Fd\NUL\128H@\CAN\144Ed\NUL\128F@\CAN\144Dd\NUL\128E@\CAN\144Cd\NUL\128D@\CAN\144Ad\NUL\128C@\CAN\144@d\NUL\128A@\CAN\128@@\NUL\144Cd\CAN\128C@\NUL\144Hd\CAN\128H@\NUL\144Md\CAN\144Ld\NUL\128M@\CAN\128L@\NUL\144Md\CAN\144Ld\NUL\128M@\CAN\144Hd\NUL\128L@\CAN\144Cd\NUL\128H@\CAN\144Ad\NUL\128C@\CAN\144@d\NUL\128A@\CAN\128@@\NUL\144Cd\CAN\128C@\NUL\144Hd\CAN\144Gd\NUL\128H@\CAN\128G@\NUL\144Qd0\144Jd\NUL\128Q@\CAN\128J@\NUL\144Nd`\144Jd\NUL\128N@\CAN\144Ed\NUL\128J@\CAN\128E@\NUL\144Jd\CAN\144Id\NUL\128J@\CAN\128I@\NUL\144Jd\CAN\144Id\NUL\128J@\CAN\144Gd\NUL\128I@\CAN\144Ed\NUL\128G@\CAN\144Bd\NUL\128E@\CAN\144@d\NUL\128B@\CAN\128@@\NUL\144Ed\CAN\128E@\NUL\144Ed\CAN\144@d\NUL\128E@\CAN\128@@\NUL\144Ed\CAN\128E@\NUL\144Gd\CAN\128G@\NUL\144Gd0\144Ed\NUL\128G@\CAN\144Bd\NUL\128E@\CAN\144@d\NUL\128B@\CAN\144=d\NUL\128@@\CAN\144;d\NUL\128=@\CAN\144\&8d\NUL\128;@\CAN\128\&8@\CAN\144\&4d\CAN\128\&4@\NUL\144\&5d\CAN\128\&5@\NUL\144<d0\128<@\NUL\144<d0\128<@\NUL\144Cd0\128C@\NUL\144Cd0\128C@\NUL\144Hd0\128H@\NUL\144Od0\144Md\NUL\128O@0\144Ld\NUL\128M@0\144Hd\NUL\128L@0\144Cd\NUL\128H@0\144Ad\NUL\128C@0\144@d\NUL\128A@0\128@@\NUL\144Ad0\128A@\NUL\144Hd0\128H@\NUL\144Jd0\144Id\NUL\128J@0\128I@\NUL\144Ld0\128L@\NUL\144Pd0\128P@\NUL\144Sd0\144Pd\NUL\128S@0\144Ld\NUL\128P@\CAN\144Gd\NUL\128L@\CAN\144Ed\NUL\128G@\CAN\128E@\NUL\144Gd\CAN\128G@\NUL\144Id\CAN\144Gd\NUL\128I@\CAN\144Ed\NUL\128G@\CAN\128E@\NUL\144Gd\129@\128G@\NUL\255/\NUL"
headerChunk :: Get (String, Word32, Word16, Word16, Word16)
headerChunk = do
    -- Get the Header Chunk 4 bytes
    h <- getByteString 4
    unless (h == BC.pack "MThd") $ fail "MIssing MThd marker"

    -- <header_length> 4 bytes
    -- Length of the header chunk (always 6 bytes long)
    headerLength <- getWord32be
    unless (headerLength == 6) $ fail "Invalid Header Length"

    -- <format> 2 bytes
    -- 0 = single track file format
    -- 1 = multiple track file format
    -- 2 = multiple song file format
    format <- getWord16be

    -- <n> 2 bytes
    -- Number of tracks in the file
    n <- getWord16be

    -- <division> 2 bytes
    -- Unit of time for delta timing. If the value is positive, then it represents the units per beat.
    division <- getWord16be

    return (show h, headerLength, format, n, division)
    
-- Result - ("\"MThd\"",6,0,1,96)

trackChunk = do
    t <- getByteString 4
    unless (t == BC.pack "MTrk") $ fail "Missing MTrk marker"

    -- <length> 4 bytes
    -- The number of bytes in the track chunk, following this number
    len <- getWord32be

    -- Parse Tracks
    trackData <- getByteString (fromIntegral len)
    
    return (show t, len, show trackData)

parseTrack = do
    t <- getByteString 4
    unless (t == BC.pack "MTrk") $ fail "Missing MTrk marker"

    len <- getWord32be
    trackStart <- bytesRead
    
    events <- parseEvents trackStart (fromIntegral len)

    return (show t, len, trackStart, events)

parseEvents start len = do
    current <- bytesRead
    if current - start >= fromIntegral len
        then return []
        else do
            _vTime <- parseVarLen
            -- Parse Event
            status <- getWord8
            event  <- parseEvent status
            -- Continue with next event
            rest <- parseEvents start len
            return (event:rest)


parseEvent status
    | status == 0xFF = do
        metaType  <- getWord8
        metaLen   <- parseVarLen
        eventData <- getByteString $ fromIntegral metaLen
        return (metaType, metaLen, show eventData)
    | status >= 0x80 && status <= 0xEF = do
        
    | otherwise = do
        return (00,00,"hi")
        




parseVarLenThing :: Word8 -> Get Word8
parseVarLenThing acc = do
    byte <- getWord8
    let acc' = (acc `shiftL` 7) .|. (byte .&. 0x7f)
    if (byte .&. 0x80) == 1
        then parseVarLenThing acc'
        else return acc'
parseVarLen :: Get Word8
parseVarLen = parseVarLenThing 0
    

bin :: (Num a1, Num a2, Eq a2) => [a2] -> a1
bin bs = sum $ map ((2^) . snd) $ filter (\x -> fst x == 1) $ zip (reverse bs) [0..]


midiParse = do
    header@(h, headerLength, format, n, division) <- headerChunk
    track <- parseTrack
    return (header, track)

run = do
    midi <- readBin filePath
    print $ runGet midiParse midi
