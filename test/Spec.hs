{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Redis as Redis
import qualified Data.Redis.Command as Cmd
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  redis <- Redis.newPool address 1
  res <- Redis.runCmd redis $ do
    _ <- Cmd.set "foo" "bar"
    r <- Cmd.get "foo"
    _ <- Cmd.hset "hah" "a" "b"
    _ <- Cmd.hset "hah" "b" "c"
    pure r
  print res
  S.mapM_ print $
    Redis.runStream redis (Cmd.hgetall "hah")
  where
    address = Redis.Address (127, 0, 0, 1) 6379
