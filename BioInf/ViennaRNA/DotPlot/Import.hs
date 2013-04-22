{-# LANGUAGE OverloadedStrings #-}

module BioInf.ViennaRNA.DotPlot.Import where

import qualified Data.Text as T
import Data.Text (Text(..))

import BioInf.ViennaRNA.DotPlot



textToDotPlot :: Text -> DotPlot
textToDotPlot t = error "write me" where
  ls = T.lines t
  sq = case (drop 1 . dropWhile (not . T.isPrefixOf "/sequence") $ ls) of
         (x:_)     -> x
         otherwise -> error $ "lines do not contain sequence part: " ++ show ls
  ps = filter (T.isSuffixOf "box")
     . takeWhile (not . T.isPrefixOf "showpage")
     . drop 1
     . dropWhile (not . T.isPrefixOf "%start of base pair probability data")
     $ ls

