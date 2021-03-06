{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Matrix
    ( Matrix(MkMatrix, unMatrix)
    , fromVectors, liftMatrix
    , appendX, appendY

    , Fin(unFin)
    , next, prev

    , xMax, xMin, yMin, yMax
    , xIndices, yIndices
    , index, update
    ) where

import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(Proxy))
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeNats (Nat, type (+))
import Instances.TH.Lift ()  -- provides @instance Lift Vector@
import Language.Haskell.TH.Syntax (Lift, Q, TExp, liftTyped)
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics (Lens', A_Lens, lens)
import Optics.At (Index, IxValue, IxKind, Ixed, ix)


type Matrix :: Nat -> Nat -> Type -> Type
newtype Matrix x y a = MkMatrix
    { unMatrix :: V y (V x a)
    } deriving (Eq, Foldable, Functor, Lift, Ord, Show)

deriving instance Lift a => Lift (V n a)  -- orphan instance

instance (Dim x, Dim y) => Applicative (Matrix x y) where
    pure = Vec.replicate (V.reflectDim $ Proxy @x)
        .> Vec.replicate (V.reflectDim $ Proxy @y)
        .> fmap V.V
        .> V.V
        .> MkMatrix

    funs <*> vals = MkMatrix $ do
        yFin <- V.V $ Vec.fromList $ yIndices vals
        pure $ do
            xFin <- V.V $ Vec.fromList $ xIndices vals
            pure $ index xFin yFin funs $ index xFin yFin vals

type instance Index (Matrix x y a) = (Fin x, Fin y)
type instance IxValue (Matrix x y a) = a

instance Ixed (Matrix x y a) where
    type IxKind (Matrix x y a) = A_Lens
    ix (xFin, yFin) = indexed xFin yFin

type Fin :: Nat -> Type
newtype Fin n = UnsafeMkFin
    { unFin :: Int
    } deriving stock (Eq, Ord, Show)
      deriving newtype Hashable

fromVectors :: (Dim x, Dim y) => Vector (Vector a) -> Maybe (Matrix x y a)
fromVectors outer = do
    inner <- V.fromVector outer
    MkMatrix <$> traverse V.fromVector inner

liftMatrix :: (Dim x, Dim y, Lift a) => [[a]] -> Q (TExp (Matrix x y a))
liftMatrix =
    fmap Vec.fromList
        .> Vec.fromList
        .> fromVectors
        .> fromJust
        .> liftTyped

{-# ANN appendX "HLINT: ignore" #-}
appendX :: Matrix l y a -> Matrix r y a -> Matrix (l + r) y a
appendX (MkMatrix left) (MkMatrix right) =
    MkMatrix $ V.V $ fmap V.V $
        Vec.zipWith (<>) (toVec left) (toVec right)
  where
    toVec = V.toVector .> fmap V.toVector

appendY :: Matrix x l a -> Matrix x r a -> Matrix x (l + r) a
appendY (MkMatrix left) (MkMatrix right) =
    MkMatrix $ V.V $
        V.toVector left <> V.toVector right

next :: Dim n => Fin n -> Maybe (Fin n)
next fin@(UnsafeMkFin num)
    | num < top = Just $ UnsafeMkFin $ num + 1
    | otherwise = Nothing
  where
    top = V.reflectDim fin - 1

prev :: Fin n -> Maybe (Fin n)
prev (UnsafeMkFin num)
    | num > 0   = Just $ UnsafeMkFin $ num - 1
    | otherwise = Nothing

index :: Fin x -> Fin y -> Matrix x y a -> a
index (unFin -> xFin) (unFin -> yFin) (MkMatrix matrix) =
    matrix & get yFin & get xFin
  where
    get n vec = V.toVector vec Vec.! n

set :: Int -> Vector a -> a -> Vector a
set n vec x = vec Vec.// [(n, x)]

update :: Fin x -> Fin y -> a -> Matrix x y a -> Matrix x y a
update (unFin -> xFin) (unFin -> yFin) val (MkMatrix matrix) =
    MkMatrix
        $ V.V
        $ fmap V.V
        $ set yFin asVecs
        $ set xFin (asVecs Vec.! yFin) val
  where
    asVecs = matrix & V.toVector & fmap V.toVector

indexed :: Fin x -> Fin y -> Lens' (Matrix x y a) a
indexed xFin yFin = lens (index xFin yFin) (flip $ update xFin yFin)

xMin :: Matrix x y a -> Fin x
xMin _ = UnsafeMkFin 0

yMin :: Matrix x y a -> Fin y
yMin _ = UnsafeMkFin 0

xMax :: Dim x => Matrix x y a -> Fin x
xMax (MkMatrix matrix) = UnsafeMkFin $ V.dim (Vec.head $ V.toVector matrix) - 1

yMax :: Dim y => Matrix x y a -> Fin y
yMax (MkMatrix matrix) = UnsafeMkFin $ V.dim matrix - 1

xIndices :: Dim x => Matrix x y a -> [Fin x]
xIndices matrix = UnsafeMkFin <$> [0 .. unFin (xMax matrix)]

yIndices :: Dim y => Matrix x y a -> [Fin y]
yIndices matrix = UnsafeMkFin <$> [0 .. unFin (yMax matrix)]
