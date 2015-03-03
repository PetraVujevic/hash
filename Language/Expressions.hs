module Language.Expressions (
    Cmd(..)
  , Expr(..)
) where

data Cmd =  Cmd { name :: Expr -- The command name 
               , args :: [Expr] -- The command arguments
               }
          | Assign { var :: Expr -- Assignment target
                   , val :: Expr -- A value to assign to a variable
				   } deriving Show
				   
data Expr = Var String -- A named variable
          | Str String -- A mere string, the peasant of expressions
            deriving (Eq, Show)

