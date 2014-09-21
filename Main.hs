module Main where

import GameOfLife
import qualified Data.HashMap.Strict as HM (insert, lookupDefault)
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
    let (Field w h m) = initializeField 6 6
        lifes =
            [ (2, 1)
            , (2, 2)
            , (4, 2)
            , (1, 3)
            , (3, 3)
            , (3, 4)
            ]
        m' = foldr (.) id (map (flip HM.insert Life) lifes) $ m
        f = Field w h m'
    step 1 f

    where
    step c f
        | c <= 100 = do
            displayField f
            threadDelay 1000000
            step (c + 1) . nextStep $ f
        | otherwise = return ()


displayField :: Field -> IO ()
displayField (Field w h m) = mapM_ displayRow [0..(h - 1)] >> putStrLn ""
    where
    displayRow y = putStrLn . map (`showState` y) $ [0..(w - 1)]
    showState x y | state x y == Life = '#'
                  | otherwise = '%'
    state x y = HM.lookupDefault Death (x, y) m
