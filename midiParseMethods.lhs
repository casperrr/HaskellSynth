> import Control.Monad.State
> import qualified Data.ByteString as B
> import Data.Word
> import Data.Bits

1. Using the State Monad for Binary Parsing

> type Parser a = State B.ByteString a

> data MidiHeader = MidiHeader {
>     format :: Format,
>     nTracks :: Int,
>     division :: Division
> }
> data Format = SingleTrack | MultipleTracks | MultipleSongs deriving (Show, Eq)
> data Division = TicksPerQuarterNote Int | SMPTEFormat Int Int deriving (Show, Eq)

> getByte :: Parser Word8
> getByte = do
>     bs <- get
>     if B.null bs
>         then error "Unexpected end of input"
>         else do
>             put (B.tail bs)
>             return (B.head bs)