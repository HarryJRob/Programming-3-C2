{- Notes from Lec -}

-- Evaluator not the same as parser. Only takes complete expressions
-- Parser takes a string finds the symbols and then applies the evaluator

-- See the text book chapter for more info  

import Parsing

data BExp = Var String | Tru | Fls | And BExp BExp | Or BExp BExp deriving (Show,Eq)


varExp :: Parser BExp
varExp = do s <- ident
            return (Var s)

truExp :: Parser BExp
truExp = do symbol("T")
            return (Tru)

flsExp :: Parser BExp
flsExp = do symbol("F")
            return (Fls)

andExp :: Parser BExp
andExp = do e1 <- lowerExpr
            symbol("&")
            e2 <- expr
            return (And e1 e2)

orExp :: Parser BExp
orExp = do  e1 <- lowerLowerExpr
            symbol("|")
            e2 <- lowerExpr
            return (Or e1 e2)

-- Use <|> to write alternatives in the grammar
-- Order is very very very important
-- Allow parenthethes to jump to the highest level

-- How to bind things tighter
-- The entry point is the tightest binding
expr :: Parser BExp
expr = andExp <|> lowerExpr
lowerExpr = orExp <|> lowerLowerExpr
lowerLowerExpr = varExp <|> truExp <|> flsExp


parseBExp :: String -> BExp
parseBExp = fst . head . (parse expr)
