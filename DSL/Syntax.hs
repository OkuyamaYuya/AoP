module Syntax where

data TY  = INT
         | BOOL
         | LISTty TY
         | PAIRty TY TY
         | FUN TY TY 
         | BOTTOM String 
           deriving (Show,Read,Eq)

class ShowType t where
  showType :: t -> String

instance ShowType TY where
  showType (BOTTOM s) = "BOTTOM "++ s
  showType INT = " Int "
  showType BOOL = " Bool "
  showType (LISTty t) = "(List "++showType t++")"
  showType (PAIRty t1 t2) = "(Pair "++showType t1++" "++showType t2++")"
  showType (FUN t1 t2) = "(" ++ (toTypeStyle.args $ (FUN t1 t2))
    where
      -- can't use higher order function
      -- a in a -> b is always int,bool,list,pair.
      args (FUN a b) = a : args b
      args others    = [others]
      toTypeStyle (a:[]) = ")" ++ showType a
      toTypeStyle (a:as) = showType a ++ toTypeStyle as

class ShowTypeHs t where
  showTypeHs :: t -> String

instance ShowTypeHs TY where
  showTypeHs (BOTTOM s) = " BOTTOM "++s
  showTypeHs INT = "Int"
  showTypeHs BOOL = "Bool"
  showTypeHs (LISTty t) = "List "++showTypeHs t
  showTypeHs (PAIRty t1 t2) = "("++showTypeHs t1++","++showTypeHs t2++")"
  showTypeHs (FUN t1 t2) = "(" ++ showTypeHs t1 ++ ") -> " ++ showTypeHs t2


data Program = Program [Sentence] deriving (Show,Read)

data Sentence = BIND {name::String, args::[String] , ty2::TY, e2::Expr} 
              | ITYPE TY
              | OTYPE TY
              | RIGHT [Expr]
              | LEFT  [Expr]
              | INSTANCE [Expr]
              | COMMENTOUT
                deriving (Show,Read)

data Expr = NAT Int 
          | B Bool
          | VAR String 
          | PLUS Expr Expr
          | MINUS Expr Expr | TIMES Expr Expr
          | EQU  Expr Expr
          | LEQ  Expr Expr
          | AND  Expr Expr
          | OR Expr Expr
          | APP Expr Expr
          | LIST [Expr]
          | PAIR Expr Expr
          | GET Expr Expr
          | IF    {cond::Expr,  tru::Expr, fal::Expr} 
          | FOLDR {f::Expr, e::Expr}
          | EOF
            deriving (Show,Read)

class ShowExpr f where
  showExpr :: f -> String

instance ShowExpr Expr where
  showExpr (NAT a) = show a
  showExpr (B True) = "true"
  showExpr (B False) = "false"
  showExpr (VAR a) = a
  showExpr (PAIR a b) = "(mk-pair " ++ showExpr a ++ " " ++ showExpr b ++ ")"
  showExpr (PLUS a b)  = "(+ " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (MINUS a b) = "(- " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (TIMES a b) = "(* " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (AND a b) = "(and " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (OR a b)  = "(or " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (EQU a b) = "(= " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (LEQ a b) = "(<= " ++ showExpr a ++ " "  ++ showExpr b ++ ")"
  showExpr (APP (APP a b) c) = "(" ++ showExpr a ++ " " ++ showExpr b ++ " " ++ showExpr c ++ ")"
  showExpr (APP a b) = "(" ++ showExpr a ++ " " ++ showExpr b ++ ")"
  showExpr (IF c t f) = "(ite " ++ showExpr c ++ showExpr t ++ showExpr f ++ ")"

class ShowExprHs f where
  showExprHs :: f -> String

instance ShowExprHs Expr where
  showExprHs (NAT a) = show a
  showExprHs (B a) = show a
  showExprHs (VAR a) = a
  showExprHs (PAIR a b) = "(" ++ showExprHs a ++ " , " ++ showExprHs b ++ ")"
  showExprHs (PLUS a b)  = "(" ++ showExprHs a ++ " + "  ++ showExprHs b ++ ")"
  showExprHs (MINUS a b) = "(" ++ showExprHs a ++ " - "  ++ showExprHs b ++ ")"
  showExprHs (TIMES a b) = "(" ++ showExprHs a ++ " * "  ++ showExprHs b ++ ")"
  showExprHs (AND a b) = "(" ++ showExprHs a ++ " && "  ++ showExprHs b ++ ")"
  showExprHs (OR a b)  = "(" ++ showExprHs a ++ " || "  ++ showExprHs b ++ ")"
  showExprHs (EQU a b) = "(" ++ showExprHs a ++ " == "  ++ showExprHs b ++ ")"
  showExprHs (LEQ a b) = "(" ++ showExprHs a ++ " <= "  ++ showExprHs b ++ ")"
  showExprHs (APP a b) = "(" ++ showExprHs a ++ " " ++ showExprHs b ++ ")"
  showExprHs (IF c t f) = "if " ++ showExprHs c ++ " then " ++ showExprHs t ++ " else " ++ showExprHs f
  showExprHs (LIST as) = "[" ++ mapLike showExprHs as ++ "]"
    where mapLike f [x] = f x
          mapLike f (x:xs) = f x ++ "," ++ mapLike f xs

-- main = do
--   putStrLn $ showExprHs $ LIST [NAT 1,NAT 2,NAT 40]
--   putStrLn $ showExprHs $ VAR "cons"
--   putStrLn $ showTypeHs $ LISTty INT
--   putStrLn $ showExprHs $ AND (APP (APP (VAR "leq") (APP (VAR "sumVal") (VAR "a"))) (APP (VAR "sumVal") (VAR "b"))) (EQU (APP (VAR "sumWt") (VAR "a")) (APP (VAR "sumWt") (VAR "b")))
