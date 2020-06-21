module Main where

import System.Random
import Criterion.Main
import Data.Text (Text)

import qualified Text.Format.Floating.Ryu as Ryu
import qualified Text.Format.Floating.Simple as Simple

main :: IO ()
main = do
    floatGen <- newStdGen
    doubleGen <- newStdGen

    let
        floats = take 1024 (randoms floatGen :: [Float])
        doubles = take 1024 (randoms doubleGen :: [Double])
    
    defaultMain
        [ bgroup "ryu"
            [ bench "float" $ nf formatFloatsRyu floats
            , bench "double" $ nf formatFloatsRyu doubles
            ]
        , bgroup "simple"
            [ bench "float" $ nf formatFloatsSimple floats
            , bench "double" $ nf formatFloatsSimple doubles
            ]
        ]

formatFloatsRyu :: Ryu.RyuFloat f => [f] -> [Text]
formatFloatsRyu = map Ryu.formatDec

formatFloatsSimple :: RealFloat f => [f] -> [Text]
formatFloatsSimple = map Simple.formatDec
