{-# LANGUAGE DeriveDataTypeable #-}
module GameOfLife
( LifeStatus(..)
, Field(..)
, initializeField
, nextStep
) where

import Data.Typeable (Typeable)
import qualified Data.HashMap.Strict as HM (HashMap, fromList, lookupDefault)

data LifeStatus = Life | Death deriving (Show, Eq, Typeable)

data Field = Field
    { fieldWidth :: Int
    , fieldHeight :: Int
    , fieldMap :: HM.HashMap (Int, Int) LifeStatus
    } deriving (Show, Eq, Typeable)


initializeField :: Int -> Int -> Field
initializeField width height = Field width height m
    where
    m = HM.fromList [((x, y), Death) | x <- [0..(width - 1)], y <- [0..(height - 1)]]

nextStep :: Field -> Field
nextStep (Field w h m) = Field w h $ HM.fromList [((x, y), nextLife (x, y)) | x <- [0..(w - 1)], y <- [0..(h - 1)]]

    where
    living = (Life ==) . flip (HM.lookupDefault Death) m

    move (x, y) (a, b) = (x + a, y + b)

    nextLife (x, y)
        | state == Death && count == 3 = Life
        | state == Life && (count == 2 || count == 3) = Life
        | otherwise = Death
        where
        state = HM.lookupDefault Death (x, y) m
        count = countLivingNeighbors (x, y)

    neighbors = [(x, y) | x <- [-1..1], y <- [-1..1], (x, y) /= (0, 0)]

    correctCordinate (x, y) = (x + w `mod` w, y + h `mod` h)

    countLivingNeighbors p = length . filter living . map (correctCordinate . move p) $ neighbors
