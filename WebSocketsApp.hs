{-# LANGUAGE OverloadedStrings #-}
module WebSocketsApp
( wsApp
) where

import GameOfLife
import Control.Monad (forever)
import Control.Concurrent.STM (STM, TVar, readTVar, modifyTVar', atomically)
import Control.Exception (catch, SomeException(..))
import qualified Network.WebSockets as WS (WebSocketsData(..), ConnectionException(..), ServerApp, acceptRequest, receiveData, sendTextData, sendBinaryData)
import qualified Data.ByteString as B (ByteString)
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..), encode, decode)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict as HM (HashMap, fromList, lookupDefault, adjust)

data Request = Request [(Int, Int)] deriving (Show, Eq)

data Response = Response [[LifeStatus]] deriving (Show, Eq)

instance DA.FromJSON LifeStatus where
    parseJSON = fmap toLifeStatus . DA.parseJSON
        where
        toLifeStatus True = Life
        toLifeStatus False = Death

instance DA.ToJSON LifeStatus where
    toJSON Life = DA.toJSON True
    toJSON Death = DA.toJSON False

instance DA.FromJSON Request where
    parseJSON = fmap Request . DA.parseJSON

instance DA.ToJSON Request where
    toJSON (Request x) = DA.toJSON x

instance DA.FromJSON Response where
    parseJSON =  fmap Response . DA.parseJSON

instance DA.ToJSON Response where
    toJSON (Response x) = DA.toJSON x

instance WS.WebSocketsData Request where
    fromLazyByteString = fromMaybe (Request []) . DA.decode
    toLazyByteString = DA.encode

instance WS.WebSocketsData Response where
    fromLazyByteString = fromMaybe (Response []) . DA.decode
    toLazyByteString = DA.encode



wsApp :: Int -> Int -> TVar Field -> TVar Int -> WS.ServerApp
wsApp w h field counter pdc = do
    c <- WS.acceptRequest pdc
    forever $ (routine c)

    where
    routine c = do
        Request ps <- WS.receiveData c
        doFlip ps
        f <- atomically . readTVar $ field
        WS.sendTextData c (toResponse f)

    toResponse (Field _ _ m) = Response $ map getStatusColumn [0..(w-1)]
        where
        getStatusColumn x = map (getStatus x) [0..(h-1)]
        getStatus x y = HM.lookupDefault Death (x, y) m

    doFlip ps = atomically $ do
        _ <- readTVar counter
        modifyTVar' field (flipField ps)
        modifyTVar' counter id

    flipField ps (Field w' h' m) = Field w' h' (flipActions ps m)

    flipActions ps = foldr (.) id $ (map (HM.adjust flipLifeStatus) . filter innerPosition $ ps)

    innerPosition (x, y) = 0 <= x && x < w && 0 <= y && y < h
