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
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Parser.ParserD as PRD

data Command m a
  = Command !Builder !(PR.Parser m Word8 a)
  deriving (Functor)

instance (Monad m) => Applicative (Command m) where
  pure a = Command mempty (pure a)
  Command bs fk <*> Command bs' fa = Command (bs <> bs') (fk <*> fa)

data StreamCommand m a
  = StreamCommand !Builder !(PRD.Parser m Word8 a)

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

newtype RedisException
  = RedisException String
  deriving (Show)

instance Exception RedisException
