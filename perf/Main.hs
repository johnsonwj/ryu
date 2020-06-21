module Main where

import System.Random
import Criterion.Main
import Data.Text (Text)

import qualified Text.Format.Floating.Ryu as Ryu
import qualified Text.Format.Floating.Simple as Simple
import Text.Printf

main :: IO ()
main = do
    floatGen <- newStdGen
    doubleGen <- newStdGen

    let
        floats = take 1024 (randoms floatGen :: [Float])
        doubles = take 1024 (randoms doubleGen :: [Double])
    
    defaultMain
        [ bgroup "float" (benchAll floats)
        , bgroup "double" (benchAll doubles)
        ]

benchAll :: (Ryu.RyuFloat f, Show f, PrintfArg f) => [f] -> [Benchmark]
benchAll fs =
    [ bench "ryu" $ nf formatFloatsRyu fs
    , bench "simple" $ nf formatFloatsSimple fs
    , bench "show" $ nf formatFloatsShow fs
    , bench "printf" $ nf formatFloatsPrintf fs
    ]

formatFloatsRyu :: Ryu.RyuFloat f => [f] -> [Text]
formatFloatsRyu = map Ryu.formatDec

formatFloatsSimple :: RealFloat f => [f] -> [Text]
formatFloatsSimple = map Simple.formatDec

formatFloatsShow :: Show f => [f] -> [String]
formatFloatsShow = map show

formatFloatsPrintf :: PrintfArg f => [f] -> [String]
formatFloatsPrintf = map (printf "%f")
