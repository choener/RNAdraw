{-# LANGUAGE PatternGuards #-}
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

import qualified Diagrams.Prelude as Dia
import qualified Diagrams.Backend.Cairo.CmdLine as Dia
import Data.Tree
import Data.List
import Data.Ord

import Biobase.Primary
import Biobase.Secondary
import qualified Biobase.Secondary.Diagrams as D



-- ** Bounding boxes.

-- | We have two kinds of bounding boxes. Hard bounding boxes are concrete
-- objects that may not intersect. Soft bounding boxes are a collection of soft
-- and hard bounding boxes. On intersection with a soft bounding box, we have
-- to recursively test until we have decided that no hard bounding box
-- intersects or that there is indeed intersection. It is hoped, that most
-- potential intersections are solved by the soft bounding boxes not
-- intersecting, in which case the calculations are simpler.

data BBox
  = Hard {bbox :: [R2]}
  | Soft {bbox :: [R2], subs :: [BBox]}
  deriving (Show)

-- | Creation of a soft bounding box, given other bounding boxes. The only
-- strict requirement is that the soft bounding box "bbox" is a convex
-- polytope.
--
-- All boxes that are to be enclosed have to live in the same local vector
-- space!
--
-- Currently, the enclosing box is a simple local-axis aligned bounding box.
--
-- TODO let us check out, if calculating the smallest convex polytope reduces
-- the runtime.

softBox :: [BBox] -> BBox
softBox bs = Soft xs bs where
  minX = minimum . concatMap (map fst . bbox) $ bs
  maxX = maximum . concatMap (map fst . bbox) $ bs
  minY = minimum . concatMap (map snd . bbox) $ bs
  maxY = maximum . concatMap (map snd . bbox) $ bs
  xs = [ (x,y) | x<-[minX,maxX], y<-[minY,maxY] ]

-- | This produces a hard bounding box, which again is just a local-axis
-- aligned bounding box.

hardBox :: [R2] -> BBox
hardBox bs = Hard xs where
  minX = minimum . map fst $ bs
  maxX = maximum . map fst $ bs
  minY = minimum . map snd $ bs
  maxY = maximum . map snd $ bs
  xs = [ (x,y) | x<-[minX,maxX], y<-[minY,maxY] ]

-- | This function performs a recursive test to check if there are
-- intersections between two 'BBox'es. As below, we either produce a seperating
-- hyperplane or return 'Nothing' on intersection. If a soft bounding box is
-- involved, the seperation is calculated based on the soft box, not the
-- individual hard boxes. If the soft boxes intersect, but not the hard boxes,
-- the answer is Just ((0,0),(0,0))
--
-- NOTE just to make sure: each individual box is NOT checked for consistency.

boxIsect :: BBox -> BBox -> Maybe (R2,R2)
boxIsect (Hard xs) (Hard ys) = seperatingHyperPlane xs ys
boxIsect (Hard xs) (Soft ys sys)
  | Just shp <- seperatingHyperPlane xs ys = Just shp
  | any (==Nothing) is = Nothing
  | otherwise = Just zero
  where
    is = map (boxIsect (Hard xs)) sys
    zero = ((0,0),(0,0))
boxIsect x@(Soft _ _) y@(Hard _) = boxIsect y x
boxIsect (Soft xs sxs) (Soft ys sys)
  | Just shp <- seperatingHyperPlane xs ys = Just shp
  | any (==Nothing) is = Nothing
  | otherwise = Just zero
  where
    is = [ boxIsect sx sy | sx<-sxs, sy<-sys ]
    zero = ((0,0),(0,0))





-- ** Calculations in 2-dimensional euclidean space. We use "diagrams" /
-- "vector-space" functions but give the required calculations for C code, too.

-- | Distance between two points in 2-d.
--
-- |x,y| = sqrt ( (x_1-y_1)^2 + (x_2-y_2)^2 )

distance :: R2 -> R2 -> Double
distance = Dia.distance

-- | Given two sets of points, "xs" and "ys", find the two points with minimal
-- distance.

minimalDistance :: [R2] -> [R2] -> (R2,R2)
minimalDistance xs ys = minimumBy (comparing $ uncurry distance) [ (x,y) | x<-xs, y<-ys ]

-- | Given two sets of points, "xs" and "ys", determine if there exists a
-- seperating hyperplane (how fancy to say in 2D). If yes, return "Just
-- (originvector, unit normal)", otherwise return "Nothing".

seperatingHyperPlane :: [R2] -> [R2] -> Maybe (R2,R2)
seperatingHyperPlane xs ys
  | null xs || null ys
  = Nothing -- need points...
  | all (head sxs ==) sxs && all (head sys ==) sys && head sxs /= head sys
  = Just (ov,n) -- all points are on their respective sides of the hyperplane and the two sets are on different sides
  | otherwise
  = Nothing
  where
    (sX,sY) = minimalDistance xs ys
    ov = (sX+sY) / 2
    n = Dia.normalized ov -- seperating hyperplane: ov + n
    txs = map (subtract ov) xs
    tys = map (subtract ov) ys
    dxs = map (n Dia.<.>) txs -- <.> is the inner/dot product
    dys = map (n Dia.<.>) tys
    sxs = map signum dxs
    sys = map signum dys



-- ** Types and other stuff

-- | The type of 2-d vectors: (Double,Double)

type R2 = Dia.R2



-- ** test data

testX = [ (0,0), (2,0), (0,2), (2,2) ]
testY = map (+(1,1)) testX
testZ = map (+(10,10)) testX






{-

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

-}
