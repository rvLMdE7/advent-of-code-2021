{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day04 where

import Control.Applicative (some)
import Control.Monad (replicateM, guard)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.List qualified as List
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Vector.Algorithms.Merge qualified as Vec.Merge
import Flow ((.>))
import Linear (V2(V2))
import Optics ((^.), (.~), (%~), (&), _2, view, zoom, use, over)
import Optics.State.Operators ((.=))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common
import Common.Optics
import Common.State.Trans


-- data types

type Matrix a = Vector (Vector a)

data Status = Drawn | NotDrawn
    deriving (Eq, Ord, Read, Show)

data Number a = MkNumber
    { number :: a
    , status :: Status
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Number

data Bingo a = MkBingo
    { toCall :: [a]
    , players :: [Matrix (Number a)]
    , winners :: [Matrix (Number a)]
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Bingo

-- parsing

parseNumbers :: Num a => Parser [a]
parseNumbers =
    Par.Ch.Lex.decimal `Par.sepEndBy` (Par.Ch.char ',' <* Par.Ch.hspace)

parseRow :: Num a => Int -> Parser [a]
parseRow n = replicateM n (Par.Ch.Lex.decimal <* Par.Ch.hspace)

lineEnd :: Parser ()
lineEnd = Par.Ch.hspace *> Par.Ch.newline *> Par.Ch.hspace

parseBoard :: Num a => V2 Int -> Parser (Matrix a)
parseBoard size = do
    rows <- replicateM (size ^. _y) $ parseRow (size ^. _x) <* lineEnd
    pure $ Vec.fromList $ fmap Vec.fromList rows

parseBoards :: Num a => V2 Int -> Parser [Matrix a]
parseBoards size = parseBoard size `Par.sepEndBy` some lineEnd

parseBingo :: Num a => V2 Int -> Parser ([a], [Matrix a])
parseBingo size = do
    nums <- parseNumbers <* some lineEnd
    board <- parseBoards size <* Par.Ch.space
    pure (nums, board)

getBingo :: Num a => Text -> Either String ([a], [Matrix a])
getBingo = runParser "day-04" $ parseBingo (pure 5) <* Par.eof

-- functions for working with Matrix

index :: V2 Int -> Matrix a -> a
index (V2 x y) matrix = matrix Vec.! y Vec.! x

findIndices :: (a -> Bool) -> Matrix a -> Vector (V2 Int)
findIndices predic matrix = do
    x <- Vec.enumFromN 0 $ Vec.length matrix
    y <- Vec.enumFromN 0 $ Vec.length (matrix Vec.! x)
    let vec = V2 x y
    guard $ predic $ index vec matrix
    pure vec

transpose :: Matrix a -> Matrix a
transpose = to .> List.transpose .> from
  where
    to = Vec.toList .> fmap Vec.toList
    from = fmap Vec.fromList .> Vec.fromList

vecSort :: Ord a => Vector a -> Vector a
vecSort = Vec.modify Vec.Merge.sort

unwrap :: Vector (V2 Int, a) -> Vector (Int, Vector (Int, a))
unwrap vec = do
    y <- Vec.uniq $ vecSort $ fmap (fst .> view _y) vec
    pure $ (y, ) $ do
        (V2 x _, val) <- Vec.filter (fst .> view _y .> (==) y) vec
        pure (x, val)

accumulate :: (a -> b -> a) -> Matrix a -> Vector (V2 Int, b) -> Matrix a
accumulate f matrix vec = Vec.update matrix $ do
    (y, xs) <- unwrap vec
    pure (y, Vec.accumulate f (matrix Vec.! y) xs)

-- main logic

isWinner :: Matrix (Number a) -> Bool
isWinner matrix = rowAllDrawn matrix || rowAllDrawn (transpose matrix)
  where
    rowAllDrawn = any $ all $ status .> (==) Drawn

score :: Num a => Matrix (Number a) -> a
score = fmap (Vec.filter notDrawn .> fmap number .> sum) .> sum
  where
    notDrawn = status .> (==) NotDrawn

drawNumber :: Eq a => a -> Matrix (Number a) -> Matrix (Number a)
drawNumber x matrix = accumulate (&) matrix points
  where
    points = (, draw) <$> findIndices (number .> (==) x) matrix
    draw = #status .~ Drawn

drawUntil :: Eq a => (Bingo a -> Bool) -> State (Bingo a) (Maybe a)
drawUntil test = zoom #toCall pop >>= \case
    Nothing -> pure Nothing
    Just x  -> do
        let splitWinners = fmap (drawNumber x) .> List.partition isWinner
        (won, rest) <- splitWinners <$> use #players
        #players .= rest
        #winners <>= won
        State.gets test >>= \case
            True  -> pure $ Just x
            False -> drawUntil test

drawUntilAnyWinner :: Eq a => State (Bingo a) (Maybe a)
drawUntilAnyWinner = drawUntil $ winners .> null .> not

drawUntilAllWinners :: Eq a => State (Bingo a) (Maybe a)
drawUntilAllWinners = drawUntil $ players .> null

play :: State (Bingo a) b -> [a] -> [Matrix a] -> (b, Bingo a)
play state calls boards = State.runState state $ MkBingo
    { toCall = calls
    , players = ffmap (`MkNumber` NotDrawn) <$> boards
    , winners = []
    }

part1 :: (Eq a, Num a) => [a] -> [Matrix a] -> Maybe a
part1 calls boards =
    case play drawUntilAnyWinner calls boards & _2 %~ winners of
        (Just num, board : _) -> Just $ score board * num
        _                     -> Nothing

part2 :: (Eq a, Num a) => [a] -> [Matrix a] -> Maybe a
part2 calls boards =
    case play drawUntilAllWinners calls boards & lastWinner of
        (Just num, board : _) -> Just $ score board * num
        _                     -> Nothing
  where
    lastWinner = over _2 $ winners .> reverse

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-04.txt"
    case getBingo @Int text of
        Left err -> die err
        Right (numbers, boards) -> do
            part1 numbers boards & maybe "???" show & putStrLn
            part2 numbers boards & maybe "???" show & putStrLn
