module Worker
( startWorker
) where

import GameOfLife
import Control.Concurrent.STM (STM, TVar, modifyTVar', atomically)
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import Control.Exception (catch, SomeException(..))

updateField :: TVar Field -> STM ()
updateField field =
    modifyTVar' field nextStep


worker :: TVar Field -> Int -> IO ()
worker field interval = forever $ work `catch` onError
    where
    work = do
        threadDelay interval
        atomically $ updateField field

    onError (SomeException e) = print e

startWorker :: TVar Field -> Int -> IO ()
startWorker f i = void . forkIO $ worker f i
