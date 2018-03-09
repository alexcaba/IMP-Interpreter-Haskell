module Interpreter
  (
    -- * Types
    Prog,
    Asgn,

    -- * Functions
    evalRaw,
    evalAdt,
  ) where

data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Equal Expr Expr
          | Smaller Expr Expr
          | Symbol String
          | Value Int deriving (Show, Read)

data Asgn = Asgn String Expr deriving (Show, Read)

data Prog = Eq Asgn
          | Seq Prog Prog
          | If Expr Prog Prog
          | For Asgn Expr Asgn Prog
          | Assert Expr
          | Return Expr deriving (Show, Read)

evalExpr :: Expr -> [(String, Int)] -> Either String Int
evalExpr (Value value) l = Right(value)

evalExpr (Symbol name) l = case lookup name l of
                        Nothing -> Left("Uninitialized variable")
                        Just value -> Right(value)

evalExpr (Add expr1 expr2) l = case evalExpr expr1 l of
                            Left(err) -> Left(err)
                            Right(value1) ->
                                case evalExpr expr2 l of
                                Left(err) -> Left(err)
                                Right(value2) -> Right(value1 + value2)

evalExpr (Sub expr1 expr2) l = case evalExpr expr1 l of
                            Left(err) -> Left(err)
                            Right(value1) ->
                                case evalExpr expr2 l of
                                Left(err) -> Left(err)
                                Right(value2) -> Right(value1 - value2)

evalExpr (Mult expr1 expr2) l = case evalExpr expr1 l of
                              Left(err) -> Left(err)
                              Right(value1) ->
                                  case evalExpr expr2 l of
                                  Left(err) -> Left(err)
                                  Right(value2) -> Right(value1 * value2)

evalExpr (Equal expr1 expr2) l = case evalExpr expr1 l of
                              Left(err) -> Left(err)
                              Right(value1) ->
                                  case evalExpr expr2 l of
                                  Left(err) -> Left(err)
                                  Right(value2) -> 
                                      if(value1 == value2) 
                                      then Right(1) 
                                      else Right(0)

evalExpr (Smaller expr1 expr2) l = case evalExpr expr1 l of
                                Left(err) -> Left(err)
                                Right(value1) ->
                                    case evalExpr expr2 l of
                                    Left(err) -> Left(err)
                                    Right(value2) -> 
                                        if(value1 < value2) 
                                        then Right(1) 
                                        else Right(0)

insertVar :: String -> Int -> [(String, Int)] -> [(String,Int)]
insertVar name value [] = [(name, value)]
insertVar name value (h:t) = if fst(h) == name
                            then if fst(h) == "return"
                                  then insertVar name (snd(h)) t
                                  else insertVar name value t
                            else h : (insertVar name value t)

evalAsgn :: Asgn -> [(String, Int)] -> Either String [(String, Int)]
evalAsgn (Asgn name expr) l = case evalExpr expr l of
                              Left(err) -> Left(err)
                              Right(value) -> Right(insertVar name value l)

evalProg :: Prog -> [(String, Int)] -> Either String [(String, Int)]
evalProg (Seq prog1 prog2 ) l = case evalProg prog1 l of
                              Left(err) -> Left(err)
                              Right(l2) ->
                                  case evalProg prog2 l2 of
                                  Left(err) -> Left(err)
                                  Right(l3) -> Right(l3)

evalProg (Eq assign) l = case evalAsgn assign l of
                        Left(err) -> Left(err)
                        Right(value) -> Right(value)

evalProg (For assign1 cond assign2 prog) l = case evalAsgn assign1 l of
                                            Left(err) -> Left(err)
                                            Right(l2) ->
                                                case evalExpr cond l2 of
                                                Left(err) -> Left(err)
                                                Right(bool) ->
                                                    if(bool == 1)
                                                    then case evalProg prog l2 of
                                                        Left(err) -> Left(err)
                                                        Right(l3) -> evalProg (For assign2 cond assign2 prog) l3
                                                    else
                                                        Right(l2)

evalProg (If cond prog1 prog2) l = case evalExpr cond l of
                                  Left(err) -> Left(err)
                                  Right(bool) ->
                                      if(bool == 1)
                                      then evalProg prog1 l
                                      else evalProg prog2 l

evalProg (Assert cond) l = case evalExpr cond l of
                          Left(err) -> Left(err)
                          Right(bool) ->
                              if(bool == 1)
                              then Right(l)
                              else Left("Assert failed")

evalProg (Return expr) l = case evalExpr expr l of
                          Left(err) -> Left(err)
                          Right(value) -> Right(insertVar "return" value l)

parse :: String -> Maybe Prog
parse = undefined

evalAdt :: Prog -> Either String Int
evalAdt prog = case evalProg prog [] of
              Left(err) -> Left(err)
              Right(l) -> 
                  case lookup "return" l of
                          Nothing -> Left("Missing return")
                          Just value -> Right(value)

evalRaw :: String -> Either String Int
evalRaw rawProg =
    case parse rawProg of
        Just prog -> evalAdt prog
        Nothing   -> Left "Syntax error"