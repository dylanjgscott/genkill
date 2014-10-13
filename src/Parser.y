{
module Parser where
import Assembly
import Token
}

-- parse :: [Token] -> Assembly
%name           parse

-- The type of the input tokens.
%tokentype      { Token }

-- Function to call when there is a syntax error.
%error          { syntaxError }

-- Specify how we will refer to the data contructors that represent
-- each token when defining the grammar.
%token
NUM     { TokenNum   $$      }
ID      { TokenId    $$      }
'('     { TokenParenOpen     }
')'     { TokenParenClose    }
LC      { TokenLc            }
LD      { TokenLd            }
ST      { TokenSt            }
ADD     { TokenAdd           }
SUB     { TokenSub           }
MUL     { TokenMul           }
DIV     { TokenDiv           }
LT      { TokenLt            }
GT      { TokenGt            }
EQ      { TokenEq            }
BR      { TokenBr            }
RET     { TokenRet           }
CALL    { TokenCall          }

-- Grammar for Language below
%%
Program         : '(' Functions ')'                     { $2                    }

Functions       : {-empty-}                             { []                    }
                | Function Functions                    { $1 : $2               }

Function        : '(' Id Args Blocks ')'                { Function $2 $3 $4 	}

Args            : '(' IdList ')'                        { $2                    }

IdList          : {-empty-}                             { []                    }
                | Id IdList                             { $1 : $2               }

Blocks          : '(' Num Instructions ')'              { Block $2 $3 : []   }
                | '(' Num Instructions ')' Blocks       { Block $2 $3 : $5   }

Instructions    : Instruction                           { $1 : []               }
Instructions    : Instruction Instructions              { $1 : $2               }

Instruction     : '(' LC Reg Num ')'                    { Lc $3 $4           }
Instruction     : '(' LD Reg Id ')'                     { Ld $3 $4           }
Instruction     : '(' ST Id Reg ')'                     { St $3 $4           }
Instruction     : '(' ADD Reg Reg Reg ')'               { Add $3 $4 $5       }
Instruction     : '(' SUB Reg Reg Reg ')'               { Sub $3 $4 $5       }
Instruction     : '(' MUL Reg Reg Reg ')'               { Mul $3 $4 $5       }
Instruction     : '(' DIV Reg Reg Reg ')'               { Div $3 $4 $5       }
Instruction     : '(' LT Reg Reg Reg ')'                { Lt $3 $4 $5        }
Instruction     : '(' GT Reg Reg Reg ')'                { Gt $3 $4 $5        }
Instruction     : '(' EQ Reg Reg Reg ')'                { Eq $3 $4 $5        }
Instruction     : '(' BR Reg Num Num ')'                { Br $3 $4 $5        }
Instruction     : '(' RET Reg ')'                       { Ret $3             }
Instruction     : '(' CALL Reg Id RegList ')'           { Call $3 $4 $5      }

RegList         : {-empty-}                             { []                    }
RegList         : Reg RegList                           { $1 : $2               }

Num             : NUM                                   { $1                    }
Reg             : ID                                    { if (head $1) == 'r'
                                                            then  read (tail $1)
                                                            else syntaxError []
                                                        }
Id              : ID                                    { $1 	                }

{
-- Call this function when we get a parse error.
syntaxError :: [Token] -> a
syntaxError _ = error "Syntax Error."
}
