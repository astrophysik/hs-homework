module Part1.Tasks where

import Util(notImplementedYet)
import Data.Fixed (mod')

factorial :: (Eq a, Num a) => a -> a
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = sum $ takeWhile (\a -> abs a > 1e-10) [((-1) ^ n * (mod' x  (2 * pi)) ^ (2 * n + 1)) / fromIntegral (factorial (2 * n + 1)) | n <- [0..]]

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
myCos x = sum $ takeWhile (\a -> abs a > 1e-10) [((-1) ^ n * (mod' x  (2 * pi)) ^ (2 * n)) / fromIntegral (factorial (2 * n)) | n <- [0..]]

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b | b == 0 = abs a
          | otherwise = myGCD b (a `mod` b)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year  | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 = day >= 0 && day <= 31
                              | month == 4 || month == 6 || month == 9 || month == 11 = day >= 0 && day <= 30
                              | month == 2 && (year `mod` 4 == 0 && ((year `mod` 400 == 0) || (year `mod` 100 /= 0))) = day >= 0 && day <= 29
                              | otherwise = day >= 0 && day <= 28

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow a 0 = 1
myPow a 1 = a
myPow a b = a * (myPow a (b - 1))  

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper p 1 = True
isPrimeHelper p q | p `mod` q == 0 = False
                  | otherwise = isPrimeHelper p (q - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime p = isPrimeHelper p (p - 1)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea points = abs $ sum deltas / 2
     where
        coords = zip points (tail points ++ [head points])
        deltas = [x1 * y2 - x2 * y1| ((x1, y1), (x2, y2)) <- coords]

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c | a + b <= c || a + c <= b || b + c <= a = -1
                   | a ^ 2 + b ^ 2 == c ^ 2 || a ^ 2 + c ^ 2 == b ^ 2 || b ^ 2 + c ^ 2 == a ^ 2 = 2
                   | a ^ 2 + b ^ 2 < c ^ 2 || a ^ 2 + c ^ 2 < b ^ 2 || b ^ 2 + c ^ 2 < a ^ 2 = 0
                   | a ^ 2 + b ^ 2 > c ^ 2 && a ^ 2 + c ^ 2 > b ^ 2 && b ^ 2 + c ^ 2 > a ^ 2 = 1
