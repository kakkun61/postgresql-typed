{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import           Control.Concurrent  (forkIO, newEmptyMVar, putMVar, takeMVar)
import           Control.DeepSeq     (deepseq)
import           Control.Monad       (replicateM)
import           Data.ByteString     (ByteString)
import           Data.Foldable       (traverse_)
import           Data.Int            (Int32)
import           Data.Traversable    (traverse)
import           Database.PostgreSQL.Typed

main :: IO ()
main = do
  r <-
    concurrent $ do
      conn <- pgConnect defaultPGDatabase { pgDBPass = "password", pgDBAddr = Left ("localhost", "5432") }
      result <- pgQuery conn [pgSQL| SELECT * FROM person |] :: IO [(Int32, ByteString)]
      -- connection info on compile time is passed via environment variables.
      -- http://hackage.haskell.org/package/postgresql-typed-0.5.3.0/docs/Database-PostgreSQL-Typed.html#g:3
      pgDisconnect conn
      pure result
  r `deepseq` pure ()

concurrent :: IO a -> IO [a]
concurrent task = do
  vars <- replicateM 100 newEmptyMVar
  traverse_ (\v -> forkIO $ task >>= putMVar v) vars
  traverse takeMVar vars
