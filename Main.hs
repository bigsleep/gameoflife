module Main where

import qualified Data.HashMap.Strict as HM (insert, lookupDefault)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, newTVar, atomically)
import qualified Network.Wai.Handler.Warp as Warp (run)
import qualified Network.Wai.Handler.WebSockets as WaiWS (websocketsOr)
import qualified Network.WebSockets as WS (defaultConnectionOptions)

import Worker (startWorker)
import WebSocketsApp (wsApp)
import Root (rootApp)
import GameOfLife (initializeField)


main :: IO ()
main = do
    let port = 3000
        width = 10
        height = 10
        field = initializeField width height
    fieldTVar <- atomically $ newTVar field
    counterTVar <- atomically $ newTVar 0
    startWorker fieldTVar counterTVar 1000000
    Warp.run port $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp width height fieldTVar counterTVar) rootApp
