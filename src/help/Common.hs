{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map (Map)
import Data.Map　qualified as Map
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Foldable qualified as Fold1
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Linear.V2 (V2(V2))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par

import Paths_adventofcode2021 (getDataFileName)


type Parser a = Parsec Void Text a

runParser :: String -> Parser a -> Text -> Either String a
runParser desc parser = Par.parse parser desc .> first Par.errorBundlePretty

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8

textShow :: Show a => a -> Text
textShow = show .> Text.pack

prettyGrid
    :: (Enum a, Ord a)
    => V2 a -> V2 a -> (Maybe b -> Text) -> Map (V2 a) b -> Text
prettyGrid (V2 xMin yMin) (V2 xMax yMax) pretty grid
    | null grid  = ""
    | otherwise = Text.intercalate "\n" $ do
        y <- [yMin .. yMax]
        pure $ join $ do
            x <- [xMin .. xMax]
            pure $ Text.justifyLeft width ' ' $ pretty $
                Map.lookup (V2 x y) grid
  where
    join = if width > 1 then Text.unwords else mconcat
    width = maximum $
        Text.length (pretty Nothing)
            : fmap (Just .> pretty .> Text.length) (Map.elems grid)

cons :: a -> [a] -> [a]
cons x xs = x : xs

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = \case
    Nothing -> Left l
    Just r  -> Right r

ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap .> fmap

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = ffmap

compose :: Int -> (a -> a) -> a -> a
compose n f
    | n <= 0    = id
    | otherwise = f .> compose (n - 1) f

supremum1 :: Foldable1 f => Ord a => f a -> a
supremum1 = Fold1.toNonEmpty .> \(x :| xs) -> supremum x xs

supremum :: (Foldable f, Ord a) => a -> f a -> a
supremum = supremumBy compare

supremumBy1 :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
supremumBy1 cmp = Fold1.toNonEmpty .> \(x :| xs) -> supremumBy cmp x xs

supremumBy :: Foldable f => (a -> a -> Ordering) -> a -> f a -> a
supremumBy cmp = foldl $
    \x y -> case x `cmp` y of
        GT -> x
        _  -> y

infimum1 :: (Foldable1 f, Ord a) => f a -> a
infimum1 = Fold1.toNonEmpty .> \(x :| xs) -> infimum x xs

infimum :: (Foldable f, Ord a) => a -> f a -> a
infimum = infimumBy compare

infimumBy1 :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
infimumBy1 cmp = Fold1.toNonEmpty .> \(x :| xs) -> infimumBy cmp x xs

infimumBy :: Foldable f => (a -> a -> Ordering) -> a -> f a -> a
infimumBy cmp = foldl $
    \x y -> case x `cmp` y of
        LT -> x
        _  -> y
