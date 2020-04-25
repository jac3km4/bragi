module Data.Redis.Internal.Types
  ( Redis (..),
    RedisException (..),
    Resp (..),
    Command (..),
    StreamCommand (..),
  )
where

import ByteString.StrictBuilder (Builder)
import Control.Exception (Exception)
import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)
import Network.Socket (Socket)
import Streamly.Internal.Data.Parser (Parser)

data Command m a
  = Command !Builder !(Parser m Word8 a)
  deriving (Functor)

instance (Monad m) => Applicative (Command m) where
  pure a = Command mempty (pure a)
  Command bs fk <*> Command bs' fa = Command (bs <> bs') (fk <*> fa)

newtype StreamCommand m a = StreamCommand (Command m a)

newtype Redis
  = Redis
      { socket :: Socket
      }

data Resp
  = RespString ByteString
  | RespError ByteString
  | RespInt Int
  | RespBulkString (Maybe ByteString)
  | RespArray Int
  deriving (Show)

data RedisException
  = RedisException String
  deriving (Show)

instance Exception RedisException
