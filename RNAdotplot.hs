{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Function
import           Data.List (nubBy,sort)
import           Data.List.Split (splitOn)
import qualified Data.Text.IO as T
import           System.Console.CmdArgs

import           BioInf.ViennaRNA.DotPlot
import           BioInf.ViennaRNA.DotPlot.Export
import           BioInf.ViennaRNA.DotPlot.Import



-- | The RNAdotplot tool has three modes:
--
-- - 
--
-- - 'Overlay' plot: sort each cell so that the highest probability is drawn
-- first, overlaid by lower probabilities.
--
-- - 'Difference' plot: indicate with the respective color, where each of two
-- probabilities is higher. Areas with the same probability lead to an empty
-- cell.

data Options
  = Structures
    { infile       :: String
    , outfile      :: String
    , structures   :: [String]
    , colorRGB     :: [(String,(Double,Double,Double))]
    , colorize     :: [(String,String)]
    , defaultColor :: String
    }
  | Overlay
    { infiles      :: [String]
    , outfile      :: String
    , colorRGB     :: [(String,(Double,Double,Double))]
    , colorchoice  :: [String]
    , defaultColor :: String
    }
  deriving (Show,Data,Typeable)

optStructures = Structures
  { infile       = ""      &= help "file to process, stdin if empty"
  , outfile      = ""      &= help "output file, stdout if empty"
  , structures   = []      &= help "structures to put into dotplot. Both, upper and lower part for mfe plots, only lower part for partition function plots"
  , colorRGB     = cs      &= help "defines an rgb color"
  , colorize     = []      &= help "color a set of structures. define all subsets. Write the structures as 1-2-4,red to color 1,2, and 4 red. (deficiency of the optparser)"
  , defaultColor = "black" &= help "color to choose if no matching set for coloring was found"
  }

optOverlay = Overlay
  { infiles      = [] &= args
  , outfile      = ""
  , colorRGB     = cs
  , colorchoice  = []
  , defaultColor = "black"
  }

main = do
  o <- cmdArgs $ modes [optStructures,optOverlay]
  case o of
    Structures{..} -> do
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
    Overlay{..} -> do
      is <- sequence $ map (fmap textToDotPlot . T.readFile) $ infiles
      let mdp = foldl1 mergeDotPlots $ zipWith (colorizeProbDots colorRGB) colorchoice is
      case outfile of
        "" -> T.putStrLn          . dotPlotToText $ mdp
        fo -> T.writeFile outfile . dotPlotToText $ mdp

cs =
  [ ("black"   , (0,0,0))
  , ("red"     , (1,0,0))
  , ("green"   , (0,1,0))
  , ("blue"    , (0,0,1))
  , ("cyan"    , (0,1,1))
  , ("magenta" , (1,0,1))
  , ("yellow"  , (1,1,0))
  ]

