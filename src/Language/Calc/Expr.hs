module Language.Calc.Expr where

data Expr = EInt Int
          | ERead
          | EStore Expr
          | EAdd Expr Expr
          | ESub Expr Expr
          | EMul Expr Expr
          | EDiv Expr Expr
          deriving Show

