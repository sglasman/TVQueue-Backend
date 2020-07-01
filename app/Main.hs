module Main where

import           API                            ( startService
                                                , startServiceTls
                                                )
import           Control.Concurrent             ( forkFinally )
import           App                            ( evalApp )
import           Control.Monad                  ( void )
import           PollingService                 ( startPolling )

main :: IO ()
main = do
  forkFinally (void $ evalApp startPolling) (\_ -> void $ evalApp startPolling)
  startServiceTls
  return ()
