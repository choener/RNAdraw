{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import qualified Data.Text.IO as T

import BioInf.ViennaRNA.DotPlot
import BioInf.ViennaRNA.DotPlot.Export
import BioInf.ViennaRNA.DotPlot.Import



data Options = Options
  { infile          :: String
  , outfile         :: String
  , structures      :: [String]
  , colorRGB        :: [(String,Double,Double,Double)]
  , colorStructures :: [[String]]
  , defaultColor    :: String
  } deriving (Show,Data,Typeable)

options = Options
  { infile          = ""      &= help "file to process, stdin if empty"
  , outfile         = ""      &= help "output file, stdout if empty"
  , structures      = []      &= help "structures to put into dotplot. Both, upper and lower part for mfe plots, only lower part for partition function plots"
  , colorRGB        = cs      &= help "defines an rgb color"
  , colorStructures = []      &= help "color a set of structures. define all subsets"
  , defaultColor    = "black" &= help "color to choose if no matching set for coloring was found"
  }

main = do
  o@Options{..} <- cmdArgs $ options
  i <- case infile of
         "" -> T.getContents
         fi -> T.readFile fi
  let dp = undefined :: DotPlot
  case outfile of
    "" -> T.putStrLn          . dotPlotToText $ dp
    fo -> T.writeFile outfile . dotPlotToText $ dp

cs =
  [ ("black"   , 0,0,0)
  , ("red"     , 1,0,0)
  , ("green"   , 0,1,0)
  , ("blue"    , 0,0,1)
  , ("cyan"    , 0,1,1)
  , ("magenta" , 1,0,1)
  , ("yellow"  , 1,1,0)
  ]

