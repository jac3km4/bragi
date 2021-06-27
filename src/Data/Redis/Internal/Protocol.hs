module Data.Redis.Internal.Protocol
  ( PR.Parser,
    IP.parse,
    resp,
    string,
    bulkString,
    int,
    array,
    respError,
    parseArray,
    writeArray,
  )
where

import ByteString.StrictBuilder (Builder)
import qualified ByteString.StrictBuilder as Builder
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadCatch, MonadThrow (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.Redis.Internal.Types (RedisException (..), Resp (..))
import Data.Word (Word8)
import qualified Streamly.Data.Fold as FL
import Streamly.Internal.Data.Parser (Parser)
import qualified Streamly.Internal.Data.Parser as PR
import qualified Streamly.Internal.Data.Stream.IsStream as IP
import qualified Streamly.Prelude as S

resp :: MonadCatch m => Parser m Word8 Resp
resp =
  RespString <$> string
    <|> RespBulkString <$> bulkString
    <|> RespInt <$> int
    <|> RespArray <$> array
    <|> RespError <$> respError

string :: MonadCatch m => Parser m Word8 ByteString
string = char '+' *> line

bulkString :: MonadCatch m => Parser m Word8 (Maybe ByteString)
bulkString = char '$' *> (nothing <|> body)
  where
    nothing = char '-' *> char '1' $> Nothing
    body = line *> fmap Just line

int :: MonadCatch m => Parser m Word8 Int
int = char ':' *> fmap (read . BS.unpack) line

array :: MonadCatch m => Parser m Word8 Int
array = char '*' *> fmap (read . BS.unpack) line

respError :: MonadCatch m => Parser m Word8 ByteString
respError = char '-' *> line

line :: MonadCatch m => Parser m Word8 ByteString
line =
  Builder.builderBytes
    <$> PR.takeWhile (/= toEnum (fromEnum '\r')) (FL.foldMap Builder.word8)
    <* char '\r'
    <* char '\n'

char :: MonadCatch m => Char -> Parser m Word8 Word8
char c = PR.satisfy (== toEnum (fromEnum c))

-- consumes an array header and returns the stream of underlying elements
parseArray :: MonadThrow m => Parser m Word8 a -> S.SerialT m Word8 -> m (S.SerialT m a)
parseArray fa s = do
  (len, rem') <- extractHeader id s
  pure . S.take len $ IP.parseMany fa rem'
  where
    extractHeader acc = S.uncons >=> extractHeader' acc
    extractHeader' acc (Just (c, rem')) =
      let c' = toEnum $ fromEnum c
       in case c' of
            '\n' -> pure (read $ acc [], rem')
            '\r' -> extractHeader acc rem'
            '*' -> extractHeader acc rem'
            _ -> extractHeader (acc . (c' :)) rem'
    extractHeader' _ Nothing =
      throwM . RedisException $ "unexpected EOF"

writeArray :: [ByteString] -> Builder
writeArray parts =
  Builder.asciiChar '*' <> writeInt (length parts) <> foldMap writeBulkString parts
  where
    writeBulkString bs =
      Builder.asciiChar '$' <> writeInt (BS.length bs) <> Builder.bytes bs <> writeLineFeed
    writeInt n =
      Builder.bytes (BS.pack (show n)) <> writeLineFeed
    writeLineFeed =
      Builder.bytes "\r\n"
