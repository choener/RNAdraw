{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module BioInf.ViennaRNA.DotPlot.Export where

import Data.Text (Text)
import qualified Data.Array.IArray as A
import qualified Data.Text as T
import Text.Printf
import Text.QuasiText
import Data.List (sortBy)
import Data.Ord
import Data.Function

import BioInf.ViennaRNA.DotPlot



dotPlotToText :: DotPlot -> Text
dotPlotToText DotPlot{..} = qqDotPlot rnaSequence cs where
  cs = [ T.pack (conv i j p mc) | ((i,j),Just (p,mc)) <- (sortBy (comparing snd) $ A.assocs dotplot) ]
  conv i j p mc
    | i<j                     = printf "%5d %5d %10.8f ubox" i j p
    | i>j, Nothing      <- mc = printf "%5d %5d %10.8f lbox" j i z
    | i>j, Just (r,g,b) <- mc = printf "%5d %5d %10.8f %f %f %f cbox" j i z r g b
    where z = 0.95 :: Double



-- * QQ stuff

qqDotPlot :: Text -> [Text] -> Text
qqDotPlot s xs' = let xs = T.unlines xs' in [embed|
%!PS-Adobe-3.0 EPSF-3.0
%%Title: RNA Dot Plot
%%Creator: BioInf.ViennaRNA.DotPlot.Export
%%CreationDate:
%%BoundingBox: 66 211 518 662
%%DocumentFonts: Helvetica
%%Pages: 1
%%EndComments

%Options: -d2 
% 
%This file contains the square roots of the base pair probabilities in the form
% i  j  sqrt(p(i,j)) ubox

%%BeginProlog
/DPdict 100 dict def
DPdict begin
/logscale false def
/lpmin 1e-05 log def

/box { %size x y box - draws box centered on x,y
   2 index 0.5 mul sub            % x -= 0.5
   exch 2 index 0.5 mul sub exch  % y -= 0.5
   3 -1 roll dup rectfill
} bind def

/ubox {
   logscale {
      log dup add lpmin div 1 exch sub dup 0 lt { pop 0 } if
   } if
   3 1 roll
   exch len exch sub 1 add box
} bind def

/lbox {
   3 1 roll
   len exch sub 1 add box
} bind def

/cbox {
   setrgbcolor
   3 1 roll
   len exch sub 1 add box
   0 0 0 setrgbcolor
} bind def

/drawseq {
% print sequence along all 4 sides
[ [0.7 -0.3 0 ]
  [0.7 0.7 len add 0]
  [-0.3 len sub -0.4 -90]
  [-0.3 len sub 0.7 len add -90]
] {
   gsave
    aload pop rotate translate
    0 1 len 1 sub {
     dup 0 moveto
     sequence exch 1 getinterval
     show
    } for
   grestore
  } forall
} bind def

/drawgrid{
  0.01 setlinewidth
  len log 0.9 sub cvi 10 exch exp  % grid spacing
  dup 1 gt {
     dup dup 20 div dup 2 array astore exch 40 div setdash
  } { [0.3 0.7] 0.1 setdash } ifelse
  0 exch len {
     dup dup
     0 moveto
     len lineto 
     dup
     len exch sub 0 exch moveto
     len exch len exch sub lineto
     stroke
  } for
  [] 0 setdash
  0.04 setlinewidth 
  currentdict /cutpoint known {
    cutpoint 1 sub
    dup dup -1 moveto len 1 add lineto
    len exch sub dup
    -1 exch moveto len 1 add exch lineto
    stroke
  } if
  0.5 neg dup translate
} bind def

end
%%EndProlog
DPdict begin
%delete next line to get rid of title
270 665 moveto /Helvetica findfont 14 scalefont setfont (dot.ps) show

/sequence { (\
$s\
) } def
/len { sequence length } bind def

72 216 translate
72 6 mul len 1 add div dup scale
/Helvetica findfont 0.95 scalefont setfont

drawseq
0.5 dup translate
% draw diagonal
0.04 setlinewidth
0 len moveto len 0 lineto stroke 

/min { 2 copy gt { exch } if pop } bind def

/utri{ % i j prob utri
  gsave
  1 min 2 div
  0.85 mul 0.15 add 0.95  0.33
  3 1 roll % prepare hsb color
  sethsbcolor
  % now produce the coordinates for lines
  exch 1 sub dup len exch sub dup 4 -1 roll dup 3 1 roll dup len exch sub
  moveto lineto lineto closepath fill
  grestore
} bind def

%data starts here

%start of quadruplex data

%draw the grid
drawgrid

%start of base pair probability data
$xs
showpage
end
%%EOF
|]
