{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad
import Data.Maybe

import Biobase.Primary
import Biobase.Secondary
import qualified Biobase.Secondary.Diagrams as D



-- ** Construction of individual RNA secondary structure features and bounding
-- boxes. Each individual element lives in its own local vector space.
-- Combination of elements requires translating and rotating positional
-- coordinates and bounding box coordinates.
--
-- Again, we use what "diagrams" gives us but are verbose (and hopefully clear)
-- on how to write this in C.
--
-- The individual nucleotide position are given in 5' -> 3' order. The first 5'
-- nucleotide forms the local origin.
--
-- TODO For now, all construction functions produce "anonymous" structures that
-- do not contain any nucleotide annotations. In principle, it should be "easy"
-- later on, to annotate accordingly -- considering that we return only "R2"
-- points for the nucleotides and the bounding boxes.
--
-- TODO this could (and should) later on be extended to allow for different
-- kinds of construction algorithms.
--
-- TODO weird, if this works out, we will have a 5'->3' chain of nucleotide
-- positions corresponding /exactly/ to the chain of nucleotides of the input.
-- Hmm...
--
-- TODO whenever we call one of stem, unpairedLoop, etc we need to test the
-- resulting new thing for intersections...

-- | Create a hairpin.
--
-- TODO Should we use polytopes or circles?
--
-- TODO right now, we always produce the same thing, for testing only.

hairpin :: Int -> Candidates
hairpin k = [(xs, Hard xs)] where
  xs = [ (0,0), (0,1), (1,1), (1,0) ]

-- | A stem extends an existing structure by two nucleotides and produces a new
-- bounding box.
--
-- TODO switch from lists to a data structure that allows prepend/append in the
-- same amount of time?

stem :: Candidates -> Candidates
stem cs = map f cs where
  f (xs,box) = (h : nxs ++ [l], nbox) where
    nxs = map (+(0,1)) xs
    nbox = softBox $ [translateBox (0,1) box, hardBox [(1,1),h,l]]
  h = (0,0)
  l = (1,0)

-- | Bulges and interior loops. can be in stretched "stem-form" or with one of
-- several possible angles. The possibilities are sorted by how nice they are,
-- or whatever... . "i" and "j" are the number of unpaired nucleotides on
-- either side. And again, we have to discuss if the should be a smooth form or
-- an angled form on how to draw the unpaired stuff.

unpairedLoop :: Int -> Int -> Candidates -> Candidates
unpairedLoop i j cs
  | i==0 && j==0 = stem cs
  | otherwise = as ++ map stretched cs
  where
    -- Before trying the stretched version, we try to draw bulges and interior
    -- loops at an angle. If the number of relative unpairedness between the
    -- two strands is two small, we don't allow angles.
    as = if False then map angled cs else []
    -- Ok, this smells of unification. Dependent on relative difference, we
    -- allow for certain kinds of angles...
    angled (xs,box) = undefined where
      angledness = fromIntegral (i+1) / fromIntegral (j+1)
    -- An interior loop that has been "stretched" to look like a normal stem.
    -- Nucleotides are supposed to be equidistant from each other for each
    -- individual strand.
    stretched (xs,box) = ([h] ++ unpI ++ nxs ++ unpJ ++ [l], nbox) where
      nxs = map (+(0, mij+1)) xs
      mij = fromIntegral $ max i j
      iScale = mij / fromIntegral i
      jScale = mij / fromIntegral j
      unpI = [ (0, fromIntegral k * iScale) | k<-[1..i] ]
      unpJ = [ (1, fromIntegral k * jScale) | k<-[1..j] ]
      nbox = softBox $ [translateBox (0,mij+1) box, hardBox [(0,0),(1,mij+1)]]
    h = (0,0)
    l = (1,0)

-- | And finally, multibranched loops. Dependent on the number of branches
-- (remember the additional one going "out"), we select possible angles.
--
-- TODO For now, we just subdivide equally and see what happens, later on we
-- probably have to take some state into account which restricts the total
-- available angular freedom.
--
-- TODO Simple hairpins may branch off at any point, complex stem structures
-- may only branch off at certain angles. So basically, in "n" we count the
-- number of complex things ... ?

multibranchedLoop :: [Region] -> Candidates
multibranchedLoop rs = undefined where
  n = 1 + (length . filter isPaired $ rs)
  l = 2 * n + sum (map len . filter isUnpaired $ rs) -- total number of nucleotides needed on the polytope / circle

-- | Multibranched loops are combined of an ordered list of unpaired regions
-- and regions closed by a pair. In addition, paired regions are actually each
-- a list of one or more candidates.

data Region
  = Unpaired {len :: Int}
  | Paired {cands :: Candidates}
  deriving (Show)

isUnpaired :: Region -> Bool
isUnpaired (Unpaired _) = True
isUnpaired _ = False

isPaired :: Region -> Bool
isPaired (Paired _) = True
isPaired _ = False





mkTree :: String -> Candidates
mkTree = goExt . D.d1sTree . D.mkD1S where
  goExt (D.SSExt k _ []) = error "draw unpaired region"
  -- single stem in external region
  goExt (D.SSExt k _ [x]) = do
    s <- goStem x
    return s

goStem (D.SSTree (i,j) _ []) = [(x, hardBox x)] where
  x = [(0,0), (0,1.1)]

goStem (D.SSTree (i,j) _ [x@(D.SSTree (k,l) _ _)])
  -- left bulge
  | i+1<k && j-1==l = do
    let h = [(0,0)]
    let hbox = hardBox h
    (t',tbox') <- goStem x
    theta <- [-90, -45, 0 :: Deg]
    let t = map (+(0,1)) . map (Dia.rotate theta) $ t'
    let tbox = translateBox (0,1) . rotateBox theta $ tbox'
    guard . isJust $ boxIsect hbox tbox
    return (h++t, softBox [hbox, tbox])
  -- right bulge
  -- TODO UNIFY!!!
  | i+1==k && j-1>l = do
    let h = [(0,0)]
    let hbox = hardBox h
    (t',tbox') <- goStem x
    theta <- [90, 45, 0 :: Deg]
    let t = map (+(0,1)) . map (Dia.rotate theta) $ t'
    let tbox = translateBox (0,1) . rotateBox theta $ tbox'
    guard . isJust $ boxIsect hbox tbox
    return (h++t, softBox [hbox, tbox])
  -- simple stem or interior loop
  -- | i+1==k && j-1==l = do
  | otherwise = do
    let h = [(0,0)]
    let hbox = hardBox h
    (t',tbox') <- goStem x
    let t = map (+(0,1)) t'
    let tbox = translateBox (0,1) tbox'
    guard . isJust $ boxIsect hbox tbox
    return (h++t, softBox [hbox, tbox])

goStem (D.SSTree (i,j) _ xs) = do
  let h = [(0,0)]
  let hbox = hardBox h
  let split = 360 / (fromIntegral $ length xs + 1)
  let thetas = map Dia.Deg . take (length xs) $ iterate (+split) split
  ys' <- allWays $ map goStem xs
  let ys = zipWith (\theta y -> translateBoth (0,1)
                              . rotateBoth theta
                              . translateBoth (0,1)
                              $ y
                ) thetas ys'
  return ( h ++ concatMap fst ys
         , softBox $ hbox : map snd ys
         )

translateBoth k (xs, box) = ( map (+k) xs
                            , translateBox k box
                            )

rotateBoth theta (xs, box) = ( map (Dia.rotate theta) xs
                             , rotateBox theta box
                             )

allWays [] = [[]]
allWays (x:xs) = do
  y <- x
  ys <- allWays xs
  return $ y:ys

t1 =  "((.((.))).)"
t2 = "(()())"

t = mapM_ (\t -> print t >> print "") $ mkTree t2

main = do
  xs <- (map mkTree . lines) `fmap` getContents
  mapM_ (\x -> (print $ take 1 x) >> putStrLn "") xs
  print $ map length xs


{-
test = unpairedLoop 2 4 . stem . hairpin $ 4
t = mapM_ print test
-}

-- | A candidate structure is a list of coordinates in 5'->3' form and a
-- bounding box around that all.

type Candidate = ([R2], BBox)

-- | We typically deal with many candidates that then have to be evaluated.

type Candidates = [Candidate]


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

-- | Translate all coordinates of a bounding box.

translateBox :: R2 -> BBox -> BBox
translateBox p (Hard xs) = Hard $ map (+p) xs
translateBox p (Soft xs sxs) = Soft (map (+p) xs) (map (translateBox p) sxs)

-- | Rotate all coordinates of a bounding box.

rotateBox :: Deg -> BBox -> BBox
rotateBox theta (Hard xs) = Hard $ map (Dia.rotate theta) xs
rotateBox theta (Soft xs sxs) = Soft (map (Dia.rotate theta) xs) (map (rotateBox theta) sxs)



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

-- | What kind of rotation are we doing, 0..360 degree kinds.

type Deg = Dia.Deg



-- ** test data

testX = [ (0,0), (2,0), (0,2), (2,2) ]
testY = map (+(1,1)) testX
testZ = map (+(10,10)) testX

-- main = return ()





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
