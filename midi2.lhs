Haskell Midi Parser

> import qualified Data.ByteString.Lazy as BL
> import qualified Data.ByteString.Char8 as BC
> import Data.Word
> import Data.Bits
> import Data.Binary.Get
> import Control.Monad
  
> filePath :: FilePath
> filePath = "dohotgirlslikechordsguitarsolo.mid"

> readBin :: FilePath -> IO BL.ByteString
> readBin = BL.readFile

Header Chunk is 14 Bytes long
- ChunkID,    4 Bytes, "MThd"
- ChunkSize,  4 Bytes, A Number
- FormatType, 2 Bytes, 0 - 2 
- nTracks,    2 Bytes, 1 - 65,535
- timeDiv,    2 Bytes, idk

> headerChunk = do
>     h <- getByteString 4
>     unless (h == BC.pack "MThd") $ fail "Missing MThd marker"
>
>     headerLength <- getWord32be
>     unless (headerLength == 6) $ fail "Invalaid Header Length"
>
>     format <- getWord16be
>     unless (format > 3 || format < 0) $ fail "Invalid Format"
> 
>     n <- getWord16be
>     division <- getWord16be
>      
>     return (show h, headerLength, format, n, division)


> parseMidi = do
>     header <- headerChunk
>     return header 

> run = do
>     midi <- readBin filePath
>     print $ runGet parseMidi midi