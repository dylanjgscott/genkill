module Token where

data Token = TokenNum Token.Num
           | TokenId Token.Id
           | TokenParenOpen
           | TokenParenClose
           | TokenLc
           | TokenLd
           | TokenSt
           | TokenAdd
           | TokenSub
           | TokenMul
           | TokenDiv
           | TokenLt
           | TokenGt
           | TokenEq
           | TokenBr
           | TokenRet
           | TokenCall
           deriving (Show, Eq)
          
type Num = Integer

type Id = String

type Reg = Integer
