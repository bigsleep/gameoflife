module Worker
( startWorker
) where

import GameOfLife
import Control.Concurrent.STM (STM, TVar, modifyTVar', atomically)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import Control.Exception (catch, SomeException(..))
import Debug.Trace (trace)

updateField :: TVar Field -> TVar Int -> STM ()
updateField field counter =
    modifyTVar' field nextStep >> modifyTVar' counter increment
    where
    increment a = if a >= 1000000 then 0 else a + 1


worker :: TVar Field -> TVar Int -> Int -> IO ()
worker field counter interval = forever $ work `catch` onError
    where
    work = do
        threadDelay interval
        atomically $ updateField field counter

    onError (SomeException e) = trace (show e) (return ())

startWorker :: TVar Field -> TVar Int -> Int -> IO ()
startWorker f c i = void . forkIO $ worker f c i
