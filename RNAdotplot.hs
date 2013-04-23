{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import qualified Data.Text.IO as T
import Data.List (nubBy,sort)
import Data.Function
import Data.List.Split (splitOn)

import BioInf.ViennaRNA.DotPlot
import BioInf.ViennaRNA.DotPlot.Export
import BioInf.ViennaRNA.DotPlot.Import



data Options = Options
  { infile       :: String
  , outfile      :: String
  , structures   :: [String]
  , colorRGB     :: [(String,(Double,Double,Double))]
  , colorize     :: [(String,String)]
  , defaultColor :: String
  } deriving (Show,Data,Typeable)

options = Options
  { infile       = ""      &= help "file to process, stdin if empty"
  , outfile      = ""      &= help "output file, stdout if empty"
  , structures   = []      &= help "structures to put into dotplot. Both, upper and lower part for mfe plots, only lower part for partition function plots"
  , colorRGB     = cs      &= help "defines an rgb color"
  , colorize     = []      &= help "color a set of structures. define all subsets. Write the structures as 1-2-4,red to color 1,2, and 4 red. (deficiency of the optparser)"
  , defaultColor = "black" &= help "color to choose if no matching set for coloring was found"
  }

main = do
  o@Options{..} <- cmdArgs $ options
  i <- case infile of
         "" -> T.getContents
         fi -> T.readFile fi
  let cSs = map (\xs -> (sort . map read . splitOn "-" $ fst xs, snd xs)) colorize
  let dpOld = textToDotPlot i
  let dpNew = if null structures
                then dpOld
                else addStructure (nubBy ((==) `on` fst) $ reverse colorRGB) defaultColor cSs (clearMFE dpOld) structures
  case outfile of
    "" -> T.putStrLn          . dotPlotToText $ dpNew
    fo -> T.writeFile outfile . dotPlotToText $ dpNew

cs =
  [ ("black"   , (0,0,0))
  , ("red"     , (1,0,0))
  , ("green"   , (0,1,0))
  , ("blue"    , (0,0,1))
  , ("cyan"    , (0,1,1))
  , ("magenta" , (1,0,1))
  , ("yellow"  , (1,1,0))
  ]

