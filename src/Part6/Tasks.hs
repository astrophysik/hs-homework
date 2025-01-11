{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map (Map, empty, insert, fromList, findWithDefault)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
       fromElements :: Int -> Int -> [((Int, Int), Int)] -> mx
       getElement :: mx -> (Int, Int) -> Int
       getDim :: mx -> (Int, Int)

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       fromElements _ _ [((0, 0), e)] = e
       fromElements _ _ _ = 0
       getDim _ = (1, 1)
       getElement n _ = n

instance Matrix [[Int]] where
       fromElements w h elems = [[findElem i j | i <- [0..w - 1]] | j <- [0..h - 1]]
                     where findElem i j = case lookup (i, j) elems of
                            Just e -> e
                            Nothing -> 0
       getDim [] = (0, 0)
       getDim m@(h : t) = (length h, length m)
       getElement m (x, y) = (m !! y) !! x



instance Matrix (SparseMatrix Int) where
       fromElements w h elems = SparseMatrix {
              sparseMatrixWidth = w,
              sparseMatrixHeight = h,
              sparseMatrixElements = fromList elems
       }
       getDim m = (sparseMatrixWidth m, sparseMatrixHeight m)
       getElement m pos = findWithDefault 0 pos (sparseMatrixElements m) 

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = fromElements w w [((i, i), 1) | i <- [0..w - 1]]

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = fromElements w h []

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix lhs rhs | w1 == h2 = fromElements w2 h1
                             [((j, i), sum [getElement lhs (k, i) * getElement rhs (j, k) | k <- [0..w1 - 1]])
                                   | i <- [0..h1 - 1], j <- [0..w2 - 1]]
              where (w1, h1) = getDim lhs
                    (w2, h2) = getDim rhs  

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant m
  | w == h = determinantRecursive (toLists m)
  where
    (w, h) = getDim m
    toLists mx = [[getElement mx (i, j) | j <- [0..w - 1]] | i <- [0..h - 1]]

    determinantRecursive [[x]] = x
    determinantRecursive mx@(row0 : _) = sum [(-1) ^ j * (row0 !! j) * determinantRecursive (minor mx j)
                                                 | j <- [0..length row0 - 1]]

    minor mx column = [[getElement (mx :: [[Int]]) (j, i) | j <- [0..length (head mx) - 1], j /= column]
      | i <- [1..length mx - 1]]
