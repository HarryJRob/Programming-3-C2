-- COMP2209 Coursework 2, University of Southampton 2018
-- DUMMY FILE FOR YOU TO EDIT AND ADD YOUR OWN IMPLEMENTATIONS
-- NOTE THAT NO THIRD PARTY MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION TYPE SIGNATURES NOR TYPE DEFINITIONS 
-- This module statement makes public only the specified functions and types
-- DO NOT CHANGE THIS LIST OF EXPORTED FUNCTIONS AND TYPES
module Challenges (convertLet, prettyPrint, parseLet, countReds, compileArith,
    Expr(App, Let, Var), LamExpr(LamApp, LamAbs, LamVar)) where

import Data.Char
import Parsing

-- Challenge 1
data Expr = App Expr Expr | Let [Int] Expr Expr | Var Int deriving (Show,Eq)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq)

-- Var is the base case for this recursive function.
-- It pattern matches down the expression converting as it goes.
-- A helper function is needed for the Let as the function definition in let is not of the type Expr

-- convert a let expression to lambda expression
convertLet :: Expr -> LamExpr
-- replace the definition below with your solution
convertLet (Var a)              = LamVar a
convertLet (App a b)            = LamApp (convertLet a) (convertLet b)
convertLet (Let (a:as) b c)     = LamApp (LamAbs a (convertLet c)) (createFunction as b)
    where 
        createFunction :: [Int] -> Expr -> LamExpr
        createFunction [] expr      = convertLet expr
        createFunction (a:as) expr  = (LamAbs a (createFunction as expr))


-- Challenge 2
-- Var is once again the base case for this recursive function
-- Since brackets are needed to calrify applications I pattern matched the cases where brackets were needed and added them.

-- pretty print a let expression by converting it to a string
prettyPrint :: Expr -> String
-- replace the definition below with your solution
prettyPrint (Var a)                                 = "x" ++ show a  
prettyPrint (App aex@(Var a) bex@(Var b))           = prettyPrint aex ++ " " ++ prettyPrint bex
prettyPrint (App aex@(Var a) b)                     = prettyPrint aex ++ " (" ++ prettyPrint b  ++ ")"
prettyPrint (App aex@(Let as b c) bex@(App d e))    = "(" ++ prettyPrint aex ++ ") (" ++ prettyPrint bex ++ ")"
prettyPrint (App aex@(Let as b c) d)                = "(" ++ prettyPrint aex ++ ") " ++ prettyPrint d
prettyPrint (App a b)                               = (prettyPrint a) ++ " " ++ (prettyPrint b)
prettyPrint (Let as b c)                            = "let " ++ showList as ++ " = " ++ (prettyPrint b) ++ " in " ++ (prettyPrint c)
    where
        showList :: [Int] -> String
        showList []     = ""
        showList [x]    = "x" ++ show x
        showList (x:xs) = "x" ++ show x ++ " " ++ (showList xs)

-- General Parsing Functions
-- One or more occurences of an input symbol
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

-- Left Associative parsing function adapated from the Parsec library
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

-- Challenge 3
-- I defined a monadic recursive descent parser using parsing.hs and then applied that to the string

-- parse a let expression
parseLet :: String -> Maybe Expr
-- replace the definition below with your solution
parseLet s 
    | result == [] || (snd $ head result) /= ""     = Nothing
    | otherwise                                     = Just (fst $ head result)  
        where
            result = parse expr s
            {- Language Definition

            E -> Vars {Vars} 
            Vars -> V | let V {V} = E in E | ( E ) 

            where {V} denotes 0 or many Vs
            -}

            -- Parser rules to go from a string to a let expression
            expr :: Parser Expr
            expr = appExp <|> vars 
            vars = varExp <|> letExp <|> braExp

            varExp :: Parser Expr
            varExp = do symbol "x"
                        s <- nat
                        return (Var s)

            appExp :: Parser Expr
            appExp = do chainl1 vars appParseOperation

            appParseOperation :: Parser (Expr -> Expr -> Expr)
            appParseOperation = do  e1 <- char ' '
                                    space
                                    case e1 of
                                        ' ' -> return App 

            letExp :: Parser Expr
            letExp = do symbol "let"
                        xs <- many1 varExp
                        symbol "="
                        e1 <- expr
                        symbol "in"
                        e2 <- expr
                        return (Let [ a | (Var a) <- xs ] e1 e2)

            braExp :: Parser Expr
            braExp = do symbol "("
                        e1 <- expr
                        symbol ")"
                        return (e1)

-- Challenge 4
-- count reductions using two different strategies 
countReds :: LamExpr -> Int -> (Maybe Int, Maybe Int)
-- replace the definition below with your solution
countReds expr limit 
    | leftReds <= limit && rightReds <= limit   = (Just leftReds, Just rightReds)
    | leftReds <= limit                         = (Just leftReds, Nothing)
    | rightReds <= limit                        = (Nothing, Just rightReds)
    | otherwise                                 = (Nothing, Nothing)
    where
        leftReds = length $ eval betaReduceByOneL expr
        rightReds = length $ eval betaReduceByOneR expr
 
        --Checks if a variable is free in a expression
        isFreeVar :: Int -> LamExpr -> Bool
        isFreeVar x (LamVar a) = x == a
        isFreeVar x (LamApp e1 e2) = (isFreeVar x e1) || (isFreeVar x e2)
        isFreeVar x (LamAbs a e1) 
            | x == a    = False
            | otherwise = isFreeVar x e1

        --Alpha-Conversion
        alphaConv :: Int -> LamExpr -> LamExpr -> LamExpr
        alphaConv x (LamVar a) e1
            | x == a    = e1
            | otherwise = LamVar x
        alphaConv x (LamAbs a e1) e2
            | x /= a && not (isFreeVar x e2)    = LamAbs x (alphaConv x e1 e2)
            | x /= a && isFreeVar x e2          = alphaConv x (LamAbs x' (alphaConv x e1 (LamVar x'))) e2
            | otherwise                         = LamAbs x e1
                where
                    x' = x+1    
        alphaConv x (LamApp e1 e2) e3 = LamApp (alphaConv x e1 e3) (alphaConv x e2 e3) 

        -- Leftmost Innermost beta-reduction
        betaReduceByOneL :: LamExpr -> LamExpr
        betaReduceByOneL (LamVar a)                     = LamVar a
        betaReduceByOneL (LamAbs a e1)                  = LamAbs a e1
        betaReduceByOneL (LamApp (LamAbs a e1) e2)      = alphaConv a e1 e2
        betaReduceByOneL (LamApp (LamVar a) e2)         = LamApp (LamVar a) (betaReduceByOneL e2)
        betaReduceByOneL (LamApp e1 e2)                 = LamApp (betaReduceByOneL e1) e2

        -- Rightmost Innermost beta-reduction
        betaReduceByOneR :: LamExpr -> LamExpr
        betaReduceByOneR (LamVar a)                 = LamVar a
        betaReduceByOneR (LamAbs a e1)              = LamAbs a e1
        betaReduceByOneR (LamApp (LamAbs a e1) e2)  = alphaConv a e1 e2
        betaReduceByOneR (LamApp e1 (LamAbs a e2))  = LamApp (betaReduceByOneR e1) (LamAbs a e2)
        betaReduceByOneR (LamApp e1 (LamVar a))     = LamApp (betaReduceByOneR e1) (LamVar a)
        betaReduceByOneR (LamApp e1 e2)             = LamApp e1 (betaReduceByOneR e2)

        -- Creates a list if input expressions paired with the output expression for a given evaluation stategy
        reductions :: (LamExpr -> LamExpr) -> LamExpr -> [ (LamExpr, LamExpr) ]
        reductions evalStrat e = [ p | p <- zip evals (tail evals) ]
            where evals = iterate evalStrat e

        -- Simplifies the output list of repeated beta reductions in a expression to a finite list of steps when the input changed
        eval :: (LamExpr -> LamExpr) -> LamExpr -> [LamExpr]
        eval evalStrat = (map fst) . takeWhile (uncurry (/=)) . reductions evalStrat

-- Challenge 5
-- Custom data type to act as a intermediate between LamExpr and string
data NotQuiteArithmeticExpr = ArithVar Int | Plus NotQuiteArithmeticExpr NotQuiteArithmeticExpr | Section NotQuiteArithmeticExpr | SectionVar NotQuiteArithmeticExpr NotQuiteArithmeticExpr deriving (Show,Eq)

-- compile an arithmetic expression into a lambda calculus equivalent
compileArith :: String -> Maybe LamExpr
-- replace the definition below with your solution
compileArith s 
    | parsedResult == [] || (snd $ head $ parsedResult) /= ""       = Nothing
    | otherwise                                                     = Just (convertArithToLambda (fst $ head $ parsedResult))
    where
        parsedResult = parse arithExpr s

        -- Converts a arithmetic expression to a lambda expression
        convertArithToLambda :: NotQuiteArithmeticExpr -> LamExpr
        convertArithToLambda (ArithVar n) = (LamAbs 1 (LamAbs 2 (numToApps n)))
            where
                numToApps :: Int -> LamExpr
                numToApps 0 = (LamVar 2)
                numToApps n = LamApp (LamVar 1) (numToApps (n-1))

        convertArithToLambda (Plus e1 e2) = (LamApp (LamApp (convertArithToLambda e1) lamSucc) (convertArithToLambda e2))
            where
                lamSucc = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
                
        convertArithToLambda (Section e1) = LamApp (convertArithToLambda e1) lamSucc
            where
                lamSucc = (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))

        convertArithToLambda (SectionVar (Section e1) e2) = LamApp (convertArithToLambda (Section e1)) (convertArithToLambda e2)  

        {- Arithmetic (Not quite)
        arithmeticExpression ::= Value | Section
        Section ::= ( +	Value )		
        Value	::=	 Var Int | Value + Value | Section Value | ( Value )			
        -}

        -- Parser rules to go from a string to a arithmetic expression
        arithExpr :: Parser NotQuiteArithmeticExpr
        arithExpr = valueExp <|> sectionExp  
        valueExp = varExp <|> chainl1 varExp addop <|> sectionVarExp <|> braExp

        varExp :: Parser NotQuiteArithmeticExpr
        varExp = do e1 <- nat
                    return (ArithVar e1)

        addop :: Parser (NotQuiteArithmeticExpr -> NotQuiteArithmeticExpr -> NotQuiteArithmeticExpr)
        addop = do symbol "+"
                   return (Plus)

        braExp :: Parser NotQuiteArithmeticExpr
        braExp = do symbol "("
                    e1 <- valueExp
                    symbol ")"
                    return (e1)

        sectionExp :: Parser NotQuiteArithmeticExpr
        sectionExp = do symbol "("
                        symbol "+"
                        e1 <- valueExp
                        symbol ")"
                        return (Section e1)

        sectionVarExp :: Parser NotQuiteArithmeticExpr
        sectionVarExp = do e1 <- sectionExp
                           space
                           e2 <- valueExp
                           return (SectionVar e1 e2)  



{- Additional Tests:

These were entered manually

Challenge 1:

Challenge 2:
1.      (App (Let [1,2] (Var 2) (Var 1)) (App (Var 1) (Var 2)))      = "(Let x1 x2 = x2 in x1) (x1 x2)" 
Challenge 3:
1.      x1 x2 (let x1 x2 = x2 in x1 x1)     = Just (App (App (Var 1) (Var 2)) (Let [1,2] (Var 2) (App (Var 1) (Var 1))))

Challenge 4:
1.      ((\x.x) (\y.y)) z   = z
                This requires an additional base case in rightmost reduction
2.      z ((\x.x) (\y.y))    = z (\y.y)
                This requires an additional base case in leftmost reduction

Challenge 5:
1.      (+1) 1     reduces to 2     
                output = Just (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))) (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamVar 2) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))) (LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))))
                pretty output = ((\x1.\x2. x1 x2) (\x1.\x2.\x3. x2 (x1 x2 x3))) (\x1.\x2. x1 x2)
                human readable = (1 successor) 1
                        i.e. apply the successor to 1 one times which is two
-}