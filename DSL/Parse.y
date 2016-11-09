{
module Parse (parse) where

import Base
import qualified Syntax as S
import qualified Token as T
}

%name parse
%tokentype { T.Token }
%error  { parseError }
%monad { Result } { thenR } { returnR }

%token
  bool { T.Bool $$ }
  int { T.Int $$ }
  var { T.Var $$ }
  '(' { T.Lparen }
  ')' { T.Rparen }
  '[' { T.Lparen2 }
  ']' { T.Rparen2 }
  '-' { T.Minus }
  '+' { T.Plus }
  '*' { T.Times }
  '=='  { T.Eq }
  '&&'  { T.And }
  '||'  { T.Or }
  if  { T.If }
  then { T.Then }
  else { T.Else }
  foldr { T.Foldr }
  '.' { T.Dot }
  ',' { T.Comma }
  ':' { T.Colon }
  '=' { T.Assign }
  '->' { T.Arrow }
  tyInt  { T.TyInt }
  tyBool { T.TyBool }
  tyList { T.TyList }
  tyPair { T.TyPair }
  '--' { T.CommentOut }
  BASETYPE { T.Basetype }
  INPUT    { T.Input }
  LEFT     { T.LLabel }
  RIGHT    { T.RLabel }
  eol { T.Eol  }

%right '->'
%left '+' '-'
%left '*' '/'

%%

Main :
    Program { $1 }

Program :
    Sentences { S.Program $1 }

Sentences :
    eol Sentences { $2 }
  | Sentence { [$1] }
  | Sentence eol { [$1] }
  | Sentence eol Sentences { $1 : $3 }

Sentence :
    '--' All                    { S.CommentOut }
  | var ':' Type '=' Expr       { S.BIND $1 [] $3 $5 }
  | var Args ':' Type '=' Expr  { S.BIND $1 $2 $4 $6 }
  | BASETYPE ':' Type           { S.BASETYPE $3 }
  | LEFT  ':' '[' Sequence ']'  { S.LEFT $4 }
  | RIGHT ':' '[' Sequence ']'  { S.RIGHT $4 }
  | INPUT ':' '[' Sequence ']'  { S.INPUT $4 }

Args :
    var { [$1] }
  | var Args { $1 : $2 }
Expr : 
    foldr Expr_ Expr_  { S.FOLDR $2 $3 }
  | Expr_ { $1 }
  | Expr Expr_ { S.APP $1 $2 }

Expr_ : 
    Expr '+' Expr           { S.PLUS  $1  $3 }
  | Expr '-' Expr           { S.MINUS $1  $3 }
  | Expr '*' Expr           { S.TIMES $1  $3 }
  | Expr '&&' Expr          { S.AND   $1  $3 }
  | Expr '||' Expr          { S.OR    $1  $3 }
  | Expr '==' Expr          { S.EQU   $1  $3 }
  | '(' Expr ')'            { $2 }
  | int                     { S.NAT $1 }
  | var                     { S.VAR $1 }
  | bool                    { S.B $1 }
  | if Expr then Expr else Expr        { S.IF $2 $4 $6 }
  | '[' Sequence  ']'     { S.LIST $2 }
  | '(' Expr ',' Expr ')' { S.PAIR $2 $4 }
  | Expr '[' Expr ']'     { S.GET $1 $3 }

Sequence : 
    Expr  { [$1] }
  | Expr ',' Sequence { $1 : $3 }

Type : 
    tyInt  { S.INT }
  | tyBool { S.BOOL }
  | tyList Type { S.LISTty $2 }
  | tyPair Type Type { S.PAIRty $2 $3 }
  | Type '->' Type { S.FUN $1 $3 }
  | '(' Type ')'   { $2 }

All :
    All_ {}
  | All_ All {}

All_ :
    bool { }
  | int  { }
  | var  { }
  | '('  { }
  | ')'  { }
  | '['  { }
  | ']'  { }
  | '-'  { }
  | '+'  { }
  | '*'  { }
  | '==' { }
  | '&&' { }
  | '||' { }
  | if   { }
  | then { }
  | else { }
  | foldr { }
  | '.' { }
  | ',' { }
  | ':' { }
  | '=' { }
  | '->' { }
  | '--' { }
  | tyInt  { }
  | tyBool { }
  | tyList { }
  | tyPair { }

{

thenR = (>>=)
returnR = return

parseError tokens = Reject "parse error"

}
