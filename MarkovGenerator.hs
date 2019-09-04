module MarkovGenerator 
    (
        generateMarkovChain
    ,   generateMarkovText
    ) where

import Control.Monad.Identity
import Control.Monad.Writer.Lazy
import qualified Data.Map.Strict as M
import System.Random

type MarkovChain = M.Map (String, String) [String]

generateMarkovChain :: [String]-> MarkovChain
generateMap txt
    | length txt < 2 = M.fromList []
    | otherwise = M.fromListWith (++) (zipWith3 f txt (tail txt) (tail (tail txt))) where
        f a b c = ((a, b), [c])

generateMarkovText :: MarkovChain -> IO String
generateMarkovText m = do
    g <- getStdGen
    let t = selectRandomBigram g m
    return $ finishTextGen g t m

selectRandomBigram :: StdGen -> MarkovChain -> Writer String (String, String)
selectRandomBigram g m = do
    tell (a ++ " " ++ b ++ " " ++ c)
    return (b, c) where
        (r, g1) = randomR (0, M.size m - 1) g
        ((a, b), l) = M.elemAt r m
        (r2, _) = randomR (0, length l - 1) g1
        c = l !! r2
    
finishTextGen :: StdGen -> Writer String (String, String) -> MarkovChain -> String
finishTextGen g (WriterT (Identity ((a, b), acc))) m
    | not (M.member (a, b) m) = acc
    | any ( == '.') c = acc ++ " " ++ c
    | otherwise = finishTextGen g1 (WriterT (Identity ((b, c), acc ++ " " ++ c))) m where
        l = m M.! (a, b)
        (r, g1) = randomR (0, length l - 1) g
        c = l !! r
