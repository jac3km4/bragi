module Data.Redis.Internal.Types
  ( Connection (..),
    Redis (..),
    Address (..),
    RedisException (..),
    Resp (..),
    Command (..),
    StreamCommand (..),
  )
where

import ByteString.StrictBuilder (Builder)
import Control.Concurrent.KazuraQueue (Queue)
import Control.Exception (Exception)
import Data.ByteString.Char8 (ByteString)
import Data.IORef (IORef)
import Data.Word (Word8)
import Network.Socket (PortNumber, Socket)
import Streamly.Internal.Data.Parser.ParserD (Parser)

data Command m a
  = Command !Builder !(Parser m Word8 a)
  deriving (Functor)

instance (Monad m) => Applicative (Command m) where
  pure a = Command mempty (pure a)
  Command bs fk <*> Command bs' fa = Command (bs <> bs') (fk <*> fa)

data StreamCommand m a
  = StreamCommand !Builder !(Parser m Word8 a)

data Redis
  = Redis
      { address :: Address,
        maxSize :: Int,
        connections :: Queue Connection,
        active :: IORef Int
      }

newtype Connection
  = Connection
      { socket :: Socket
      }

data Resp
  = RespString ByteString
  | RespError ByteString
  | RespInt Int
  | RespBulkString (Maybe ByteString)
  | RespArray Int
  deriving (Show)

data Address
  = Address
      { ip :: (Word8, Word8, Word8, Word8),
        port :: PortNumber
      }

newtype RedisException
  = RedisException String
  deriving (Show)

instance Exception RedisException
