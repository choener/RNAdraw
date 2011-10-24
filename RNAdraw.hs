{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
--
-- local TODO list
--
-- - build sstree
-- - have diagrams running
-- - be able to draw overlapped secondary structures
-- - implement algorithm for being free of overlaps

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Tree

import Biobase.Primary
import Biobase.Secondary
import qualified Biobase.Secondary.Diagrams as D



-- draw test, these functions should produce all possible drawings, one after
-- another. a filter can then be used to keep the first viable one.
--
-- in a twist, we can "take 1000" or so, and if we get no result, we reduce the
-- allowed angles where there are overlaps.

drawTest (D.SSTree ij@(i,j) a []) = regPoly (j-i+1) 1 # withBounds (square 1 :: Diagram Cairo R2) -- showOrigin

drawTest (D.SSTree ij@(i,j) a [x@(D.SSTree (k,l) _ _)])
  -- missing: IL
  -- bulge right
  | j-l > 1 = append (-1,0) (regPoly 4 1 # lc blue # showOrigin) (drawTest x # rotateBy ( 1/4))
  -- bulge left
  | k-i > 1 = append (1 ,0) (regPoly 4 1 # lc blue # showOrigin) (drawTest x # rotateBy (-1/4))
  -- normal stem
  | k-i==1 && j-l==1 = append (0,1) (regPoly 4 1 # showOrigin) (drawTest x)
  | otherwise = error $ show (i,j,k,l)

drawTest (D.SSTree ij@(i,j) a xs)
  -- a simple Y-shaped ML
  | length xs == 2 = append (1,0) (append (-1,0) (regPoly 4 1 # lc red # showOrigin) (drawTest (xs!!0) # rotateBy (1/4))) (drawTest (xs!!1) # rotateBy (-1/4))
  -- 4-way junction
  -- anything bigger leads to regular n-things (should work for 4-way junction, too)

drawTest (D.SSExt  l   a [x]) = drawTest x

pair (i,j) = (circle 1 ||| hrule 1 ||| circle 1) # showOrigin


-- ** testing area

main = defaultMain $ xyz

xyz = drawTest big

bulges = D.d1sTree $ D.mkD1S "(((((...(((...)))))).))"

multil = D.d1sTree $ D.mkD1S "((.((...))..((....)).))"

big = D.d1sTree $ D.mkD1S "((((..((....)))).(((((...))..))).))"

verybig = D.d1sTree $ D.mkD1S
  "(.....(((.(((((((((((.....(((.....)))......)))))..((((((((.((((((....))).)))..(((((((((.((((((((((((......(((((..(((((((....))))))).)))))))).)).)))))))))))))))))).))))))................))))))))).......)"
