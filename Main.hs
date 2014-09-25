module Main where

import Control.Concurrent.STM (newTVar, atomically)
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
        width = 20
        height = 20
        field = initializeField width height
    fieldTVar <- atomically $ newTVar field
    startWorker fieldTVar 2000000
    Warp.run port $ WaiWS.websocketsOr WS.defaultConnectionOptions (wsApp width height fieldTVar) rootApp
