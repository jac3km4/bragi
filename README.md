# bragi
This is an experimental redis client built with [Streamly](https://hackage.haskell.org/package/streamly).
The distinct features of this client are:
- applicative command DSL which guarantees pipeliening at compile-time
- fully stream-based - it's built from bottom up with streams, all array-based commands can be consumed as a `SerialT IO a` stream from Redis
- high performance, it only uses abstractions that add minimal runtime overhead

# examples
### basic usage
```haskell
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Redis as Redis
import qualified Data.Redis.Command as Cmd
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  -- run a set of commands in a pipeline
  res <- Redis.runCmd ip port $ do
    _ <- Cmd.set "foo" "bar"
    r <- Cmd.get "foo"
    _ <- Cmd.hset "hah" "a" "b"
    _ <- Cmd.hset "hah" "b" "c"
    pure r
  print res
  
  -- consume all keys and values of the hash as a stream
  S.mapM_ print $
    Redis.runStream ip port (Cmd.hgetall "hah")
  where
    ip = (127, 0, 0, 1)
    port = 6379
```

### consume a hashmap of 10000 elements as a stream
```haskell
main :: IO ()
main = withFile "/dev/null" WriteMode $ \handle -> do
  -- Redis.runCmd ip port $ for_ [(0 :: Int) .. 10000] $ \i -> do
  --   Cmd.hset "perf" (BC.pack (show i)) "perf"
  _ <-
    S.fold (FL.drainBy (BS.hPut handle))
      $ S.mapMaybe id
      $ Redis.runStream ip port (Cmd.hgetall "perf")
```
```sh
/usr/bin/time ./bragi-perf
0.17user 0.00system 0:00.18elapsed 98%CPU (0avgtext+0avgdata 6448maxresident)k
0inputs+0outputs (0major+546minor)pagefaults 0swaps
```
