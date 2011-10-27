{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
--
-- local TODO list
--
-- - build sstree
-- - have diagrams running
-- - be able to draw overlapped secondary structures
-- - implement algorithm for being free of overlaps
--
-- TODO we are repeating a lot of existing code for the sake of easier
-- transition to C. fix this!

module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Data.Tree
import Data.List
import Data.Ord

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

-- | Hard bounding boxes for individual objects, soft bounding boxes for
-- collections of objects.

data BBox
  = Hard Points4
  | Soft Points4 [BBox]

-- | Determine if there is any intersection between two bounding boxes.

intsect :: BBox -> BBox -> Bool
intsect (Hard x)    (Hard y)    = isect x y
intsect (Hard x)    (Soft y ys) = isect x y && any (intsect (Hard x)) ys
intsect (Soft x xs) (Hard y)    = isect x y && any (intsect (Hard y)) xs
intsect (Soft x xs) (Soft y ys) = isect x y && or [intsect x' y' | x'<-xs, y'<-ys]

-- | Intersection between bounding boxes.

isect xs ys = undefined where
  -- the two points minX,minY with minimal distance from each other
  (minX,minY) = minimumBy (comparing (uncurry dist)) [ (x,y) | x<-xs, y<-ys ]
  -- center point between x and y
  c = minX + (minY-minX)/2
  -- normal vector at "c"
  nC = let (cx,cy) = c; l = vlen c in (cy, negate cx) `scale2d` (1/l)

-- | scale vector by scalar

scale2d (x,y) s = (x*s,y*s)

-- | distance between points

dist :: P2D -> P2D -> Double
dist (x1,x2) (y1,y2) = sqrt $ (x1-y1)^2 + (x2-y2)^2

-- | length of a vector

vlen (x,y) = sqrt $ x^2+y^2

type Points4 = [P2D]

type P2D = (Double,Double)


-- ** testing area

main = defaultMain $ xyz

xyz = drawTest big

bulges = D.d1sTree $ D.mkD1S "(((((...(((...)))))).))"

multil = D.d1sTree $ D.mkD1S "((.((...))..((....)).))"

big = D.d1sTree $ D.mkD1S "((((..((....)))).(((((...))..))).))"

verybig = D.d1sTree $ D.mkD1S
  "(.....(((.(((((((((((.....(((.....)))......)))))..((((((((.((((((....))).)))..(((((((((.((((((((((((......(((((..(((((((....))))))).)))))))).)).)))))))))))))))))).))))))................))))))))).......)"
