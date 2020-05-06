module Data.Redis.Command
  ( Command,
    StreamCommand,
    command,
    commandS,
    get,
    set,
    hget,
    hset,
    ping,
    hgetall,
  )
where

import Control.Monad.Catch (MonadCatch)
import Data.ByteString (ByteString)
import Data.Redis.Internal.Protocol (Parser, bulkString, int, string, writeArray)
import Data.Redis.Internal.Types (Command (..), StreamCommand (..))
import Data.Word (Word8)

command :: MonadCatch m => [ByteString] -> Parser m Word8 a -> Command m a
command parts = Command (writeArray parts)

commandS :: [ByteString] -> Parser m Word8 a -> StreamCommand m a
commandS parts = StreamCommand (writeArray parts)

get :: MonadCatch m => ByteString -> Command m (Maybe ByteString)
get k = command ["GET", k] bulkString

set :: MonadCatch m => ByteString -> ByteString -> Command m ByteString
set k v = command ["SET", k, v] string

hget :: MonadCatch m => ByteString -> ByteString -> Command m (Maybe ByteString)
hget h k = command ["HGET", h, k] bulkString

hset :: MonadCatch m => ByteString -> ByteString -> ByteString -> Command m Int
hset h k v = command ["HSET", h, k, v] int

ping :: MonadCatch m => Command m ByteString
ping = command ["PING"] string

hgetall :: MonadCatch m => ByteString -> StreamCommand m (Maybe ByteString)
hgetall h = commandS ["HGETALL", h] bulkString
