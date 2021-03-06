module Data.Redis
  ( run,
    runStream,
    runCmd,
    withConnection,
  )
where

import qualified ByteString.StrictBuilder as Builder
import Control.Exception (bracket)
import Data.Redis.Internal.Protocol (parse, parseArray)
import Data.Redis.Internal.Types (Command (..), Redis (..), StreamCommand (..))
import Data.Word (Word8)
import Network.Socket (PortNumber)
import qualified Network.Socket as Sock
import qualified Streamly.External.ByteString as Strict
import Streamly.Internal.Data.Stream.IsStream.Common (concatM)
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Prelude as S

run :: Redis -> Command IO a -> IO a
run (Redis sock) (Command bs parser) = do
  SK.writeChunk sock . Strict.toArray $ Builder.builderBytes bs
  parse parser (S.unfold SK.read sock)

runCmd :: (Word8, Word8, Word8, Word8) -> PortNumber -> Command IO a -> IO a
runCmd ip port cmd = withConnection ip port (`run` cmd)

runStream :: (Word8, Word8, Word8, Word8) -> PortNumber -> StreamCommand IO a -> S.SerialT IO a
runStream ip port (StreamCommand (Command bs parser)) =
  TCP.withConnection ip port $ \sock -> do
    S.fromEffect $ SK.writeChunk sock $ Strict.toArray $ Builder.builderBytes bs
    concatM $ parseArray parser $ S.unfold SK.read sock

withConnection :: (Word8, Word8, Word8, Word8) -> PortNumber -> (Redis -> IO a) -> IO a
withConnection ip port f = bracket (TCP.connect ip port) Sock.close (f . Redis)
