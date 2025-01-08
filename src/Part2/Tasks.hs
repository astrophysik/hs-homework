module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

infixl 7 |+|
infixl 7 |-|
infixl 8 |*|

(|+|) :: Term -> Term -> Term
(|+|) (IntConstant lhs) (IntConstant rhs) = IntConstant (rhs + lhs)
(|+|) lhs rhs = BinaryTerm Plus lhs rhs 
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant lhs) (IntConstant rhs) = IntConstant (rhs - lhs)
(|-|) lhs rhs = BinaryTerm Minus lhs rhs 
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant lhs) (IntConstant rhs) = IntConstant (rhs * lhs)
(|*|) lhs rhs = BinaryTerm Times lhs rhs 

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement expression = case expression of (IntConstant v) -> expression
                                                               (Variable v) -> if v == varName then replacement else expression
                                                               (BinaryTerm op lhs rhs) -> BinaryTerm op (replaceVar varName replacement lhs) (replaceVar varName replacement rhs)  

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expression = case expression of (IntConstant v) -> expression
                                         (BinaryTerm Plus lhs rhs) -> lhs |+| rhs
                                         (BinaryTerm Minus lhs rhs) -> lhs |-| rhs
                                         (BinaryTerm Times lhs rhs) -> lhs |*| rhs



