import Control.Applicative
import Control.Monad
import Data.Char

pri :: Char -> Int
pri '^' =  4
pri '/' =  3
pri '*' =  2
pri '-' =  1
pri '+' =  0
pri '(' = -1

rpn :: String -> String
rpn expr = go "" "" expr
  where
    go res ops "" = reverse res ++ ops
    go res ops (c : cs)
        | isLetter c = go (c : res) ops cs
    go res (op : ops) (')' : cs)
        | op == '(' = go res ops cs
        | otherwise = go (op : res) ops (')' : cs)
    go res ops (c : cs)
        | c == '(' || null ops || pri c > pri (head ops) = go res (c : ops) cs
        | otherwise = go (head ops : res) (tail ops) (c : cs)

main :: IO ()
main = do
    n <- readLn
    replicateM_ n $
        putStrLn . rpn =<< getLine