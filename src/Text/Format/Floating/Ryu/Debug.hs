module Text.Format.Floating.Ryu.Debug where

import System.IO
import Text.Printf
import Text.Format.Floating.Ryu hiding (ryuQ)
import Text.Format.Floating.Ryu.Tables.Formulas

printTable :: String -> [(Integer, Integer)] -> IO ()
printTable label table = do
    let fileName = "ryu_table_" ++ label ++ ".txt"
        tableLines = (\(e2, m) -> printf "%8d  %8d  %8d  %32x" e2 (ryuQ e2) (-e2 - (ryuQ e2)) m) <$> table
    h <- openFile fileName WriteMode
    hPutStrLn h (printf "%8s  %8s  %8s  %s" "e2" "q" "-e2 - q" "mult")
    sequence_ (hPutStrLn h <$> tableLines)
    hClose h

-- does not evaluate its argument
printableTable :: RyuFloat f => f -> [(Integer, Integer)]
printableTable x = multiplierTable x (ryuB0 x) (ryuB1 x)

printFloatTable :: IO ()
printFloatTable = printTable "float" $ printableTable (undefined :: Float)

printDoubleTable :: IO ()
printDoubleTable = printTable "double" $ printableTable (undefined :: Double)