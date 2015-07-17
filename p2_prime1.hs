{-# LANGUAGE TupleSections #-}

import Data.Int (Int32)
import Data.Array.IO
import Control.Monad
import Control.Applicative

type N = Int32
type Sieve = IOUArray N Bool

limit :: N
limit = 31622

adjust :: N -> N -> N
adjust x y
    | x <= y         = y * 2
    | x `mod` y == 0 = x
    | otherwise      = ((x `div` y) + 1) * y

mkS :: (N, N) -> Bool -> IO Sieve
mkS = newArray

sieve :: [(N, N)] -> IO ()
sieve ranges = do
    sieve0 <- mkS (2, limit) True
    sieves <- mapM (\b -> (b,) <$> mkS b True) ranges
    forM_ [2 .. limit] $ \i -> do
        p <- readArray sieve0 i
        when p $ do
            forM_ [2*i, 3*i .. limit] $ \i -> do
                writeArray sieve0 i False
            forM_ sieves $ \((start, end), sieve) -> do
                let offset = adjust start i
                forM_ [offset, offset + i .. end] $ \i -> do
                    writeArray sieve i False
    forM_ sieves $ \((start, end), sieve) -> do
        when (start < 2) $
            writeArray sieve 1 False
        forM_ [start .. end] $ \i -> do
            p <- readArray sieve i
            when p $ print i
        putStrLn ""

lpair :: [N] -> (N, N)
lpair [x, y] = (x, y)

main :: IO ()
main = do
    t <- readLn
    ranges <- replicateM t $
        lpair . map read . words <$> getLine
    sieve ranges