{
module Lexer where
import Token
}

%wrapper "basic"

$digit =      [0-9]
$alpha =      [a-zA-Z]

tokens :-

    $white+                 ;
    lc                      { \s -> TokenLc }
    ld                      { \s -> TokenLd }
    st                      { \s -> TokenSt }
    add                     { \s -> TokenAdd }
    sub                     { \s -> TokenSub }
    mul                     { \s -> TokenMul }
    div                     { \s -> TokenDiv }
    lt                      { \s -> TokenLt }
    gt                      { \s -> TokenGt }
    eq                      { \s -> TokenEq }
    cmp                     { \s -> TokenEq }
    br                      { \s -> TokenBr }
    ret                     { \s -> TokenRet }
    call                    { \s -> TokenCall }
    \-?$digit+              { \s -> TokenNum (read s) }
    $alpha[$alpha$digit]*   { \s -> TokenId s }
    \(                      { \s -> TokenParenOpen }
    \)                      { \s -> TokenParenClose }
