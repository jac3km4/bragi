module Data.Redis
  ( Redis,
    Connection,
    Address (..),
    newPool,
    runStream,
    runCmd,
  )
where

import qualified ByteString.StrictBuilder as Builder
import Control.Arrow ((&&&))
import qualified Control.Concurrent.KazuraQueue as Q
import Control.Exception (SomeException, bracket, catch, throw)
import qualified Data.IORef as IORef
import Data.Redis.Internal.Protocol (parseArray)
import Data.Redis.Internal.Types
import Streamly (SerialT)
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Prelude as S

newPool :: Address -> Int -> IO Redis
newPool addr size = do
  chan <- Q.newQueue
  ref <- IORef.newIORef 0
  pure $ Redis addr size chan ref

getConnection :: Redis -> IO Connection
getConnection Redis {address, maxSize, connections, active} = do
  res <- Q.tryReadQueue connections
  case res of
    Just conn -> pure conn
    Nothing -> do
      wait <- IORef.atomicModifyIORef' active claim
      if wait
        then Q.readQueue connections
        else catch connect $ \(ex :: SomeException) -> do
          IORef.atomicModifyIORef' active $ (-) 1 &&& const ()
          throw ex
  where
    connect = do
      conn <- Connection <$> TCP.connect (ip address) (port address)
      Q.writeQueue connections conn
      pure conn
    claim cnt
      | cnt < maxSize = (cnt + 1, False)
      | otherwise = (cnt, True)

releaseConnection :: Redis -> Connection -> IO ()
releaseConnection redis =
  Q.writeQueue (connections redis)

runCmd :: Redis -> Command IO a -> IO a
runCmd redis (Command bs parser) =
  bracket (getConnection redis) (releaseConnection redis) $ \(Connection sock) -> do
    SK.writeChunk sock . Strict.toArray $ Builder.builderBytes bs
    S.parseD parser (S.unfold SK.read sock)

runStream :: Redis -> StreamCommand IO a -> SerialT IO a
runStream redis (StreamCommand bs parser) =
  S.bracketIO (getConnection redis) (releaseConnection redis) $ \(Connection sock) ->
    S.concatM $ do
      SK.writeChunk sock $ Strict.toArray $ Builder.builderBytes bs
      parseArray parser $ S.unfold SK.read sock
