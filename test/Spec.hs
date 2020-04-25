{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Redis as Redis
import qualified Data.Redis.Command as Cmd
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  res <- Redis.runCmd ip port $ do
    _ <- Cmd.set "foo" "bar"
    r <- Cmd.get "foo"
    _ <- Cmd.hset "hah" "a" "b"
    _ <- Cmd.hset "hah" "b" "c"
    pure r
  print res
  S.mapM_ print $
    Redis.runStream ip port (Cmd.hgetall "hah")
  where
    ip = (127, 0, 0, 1)
    port = 6379
