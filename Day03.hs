module Day03 (day03) where

import qualified Data.Either as E
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Text.ParserCombinators.Parsec

data Expr = Mul (Int, Int) | Enable Bool
  deriving (Show)

mulExpr :: GenParser Char st Expr
mulExpr = do
  string "mul("
  a <- read <$> many1 digit
  string ","
  b <- read <$> many1 digit
  string ")"
  return $ Mul (a, b)

enableExpr :: GenParser Char st Expr
enableExpr = Enable True <$ string "do()"

disableExpr :: GenParser Char st Expr
disableExpr = Enable False <$ string "don't()"

expr :: GenParser Char st Expr
expr = choice [try mulExpr, try enableExpr, try disableExpr]

maybeExpr :: GenParser Char st (Maybe Expr)
maybeExpr = choice [Just <$> expr, Nothing <$ anyChar]

exprs :: GenParser Char st [Expr]
exprs = catMaybes <$> many maybeExpr

type Input = [Expr]

parseInput :: T.Text -> Input
parseInput = E.either (error . show) id . parse exprs "(parse error)" . T.unpack

evaluateExprs1 :: [Expr] -> Int
evaluateExprs1 ((Mul (a, b)) : xs) = a * b + evaluateExprs1 xs
evaluateExprs1 (_ : xs) = evaluateExprs1 xs
evaluateExprs1 [] = 0

part1 :: Input -> Int
part1 = evaluateExprs1

evaluateExprs2 :: Bool -> [Expr] -> Int
evaluateExprs2 True ((Mul (a, b)) : xs) = a * b + evaluateExprs2 True xs
evaluateExprs2 False ((Mul (a, b)) : xs) = evaluateExprs2 False xs
evaluateExprs2 _ (Enable enable : xs) = evaluateExprs2 enable xs
evaluateExprs2 _ [] = 0

part2 :: Input -> Int
part2 = evaluateExprs2 True

day03 = (parseInput, part1, part2)
