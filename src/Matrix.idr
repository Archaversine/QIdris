module Matrix 

import Control.Monad.State

import Data.Fin.Extra
import public Data.Vect

import Util

public export
record Matrix (rows : Nat) (cols : Nat) (a : Type) where 
    constructor MkMat
    contents : Vect rows (Vect cols a)

export
Functor (Matrix r c) where 
    map f (MkMat m) = MkMat (map (map f) m)

export 
fromVect : {n : Nat} -> Vect n a -> Matrix n 1 a
fromVect v = MkMat (map (\x => [x]) v)

export
Show a => Show (Matrix r c a) where 
    show (MkMat m) = unlines (map show m)

export 
{r, c : Nat} -> Num a => Num (Matrix r c a) where 
    (+) (MkMat m1) (MkMat m2) = MkMat (zipWith (zipWith (+)) m1 m2)
    (*) (MkMat m1) (MkMat m2) = MkMat (zipWith (zipWith (*)) m1 m2)

    fromInteger k = MkMat (replicate r (replicate c (fromInteger k)))

export
zeros : Num a => {r, c : Nat} -> Matrix r c a
zeros = MkMat (replicate r (replicate c 0))

export 
ones : Num a => {r, c : Nat} -> Matrix r c a
ones = MkMat (replicate r (replicate c 1))

export
index : Fin r -> Fin c -> Matrix r c a -> a
index r c (MkMat m) = index c (index r m)

updateRow : a -> Fin c -> Vect c a -> Vect c a
updateRow value c row = updateAt c (const value) row

export 
set : Fin r -> Fin c -> a -> Matrix r c a -> Matrix r c a
set r c value (MkMat m) = MkMat (updateAt r (updateRow value c) m)

setM : Fin r -> Fin c -> a -> State (Matrix r c a) ()
setM r c value = modify (set r c value)

export 
identity : Num a => {size : Nat} -> Matrix size size a
identity = foldl (.) id [set i i 1 | i <- range {len=size}] zeros

export 
transpose : {r, c : Nat} -> Matrix r c a -> Matrix c r a
transpose (MkMat m) = MkMat (transpose m)

forM_ : (Applicative f, Foldable t) => t a -> (a -> f b) -> f ()
forM_ = flip traverse_

vectToList : Vect n a -> List a 
vectToList [] = []
vectToList (x :: xs) = x :: vectToList xs

shiftFin : (m : Nat) -> NonZero m => (Fin n) -> Fin m
shiftFin m f = modFin (finToNat f) m

export infixl 9 ><

export
(><) : Num a 
    => {r1, r2, c1, c2 : Nat} 
    -> Matrix r1 c1 a 
    -> Matrix r2 c2 a 
    -> Matrix (r1 * r2) (c1 * c2) a
(><) m1 m2 = execState (zeros {r=r1*r2,c=c1*c2}) $ do 
    -- TODO: May need to test with zero dimensions eventually
    let nonzeroR1 = the (NonZero r1) (believe_me 0)
        nonzeroC1 = the (NonZero c1) (believe_me 0)
        nonzeroR2 = the (NonZero r2) (believe_me 0)
        nonzeroC2 = the (NonZero c2) (believe_me 0)

    forM_ (vectToList $ Data.Vect.Fin.range {len = r1 * r2}) $ \row => do 
        forM_ (vectToList $ Data.Vect.Fin.range {len = c1 * c2}) $ \col => do
            let rowA = modFin (finToNat row `div` r2) r1
                colA = modFin (finToNat col `div` c2) c1
                rowB = modFin (finToNat row `mod` r2) r2
                colB = modFin (finToNat col `mod` c2) c2

            setM row col (index rowA colA m1 * index rowB colB m2)

export
(.scale) : Num a => Matrix r c a -> a -> Matrix r c a
(.scale) (MkMat m) s = MkMat (map (map (*s)) m)

export infixl 8 -

export 
(-) : Neg a => Matrix r c a -> Matrix r c a -> Matrix r c a 
(-) (MkMat m1) (MkMat m2) = MkMat (zipWith (zipWith (-)) m1 m2)

export infixl 9 * 

export 
dotProduct : Num a => Vect n a -> Vect n a -> a
dotProduct v1 v2 = sum (zipWith (*) v1 v2)

export
matmul : {c1 : Nat} -> {n : Nat} -> Num a => Matrix r1 c1 a -> Matrix c1 n a -> Matrix r1 n a
matmul (MkMat mat1) mat2 = let (MkMat mat2') = transpose mat2 in MkMat $ mat1 <&> \r1 => 
    mat2' <&> \c1 => 
        dotProduct r1 c1