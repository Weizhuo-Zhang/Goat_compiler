{
module Main where
}

%wrapper "basic"

$digit        = [0-9]
$alpha        = [a-zA-Z]
@ident        = $alpha ($alpha | $digit | \_ | \')*
@newline      = \"\\n\"
@digits       = $digit+
@floats       = @digits.@digits
@stringconst  = \" @newline*[^\"]*@newline*[^\"]*@newline* \"
@comment      = \# [^\n]* \n


rules :-
  $white+      ;
  @comment     ;
  begin        { \s -> BEGIN }
  bool         { \s -> BOOL }
  do           { \s -> DO }
  else         { \s -> ELSE }
  end          { \s -> END }
  false        { \s -> BOOL_CONST False }
  fi           { \s -> FI }
  float        { \s -> FLOAT }
  if           { \s -> IF }
  int          { \s -> INT }
  od           { \s -> OD }
  proc         { \s -> PROC }
  read         { \s -> READ }
  ref          { \s -> REF }
  then         { \s -> THEN }
  true         { \s -> BOOL_CONST True }
  val          { \s -> VAL }
  while        { \s -> WHILE }
  write        { \s -> WRITE }
  @ident       { \s -> IDENT s }
  @stringconst { \s -> STRING_CONST s }
  @digits      { \s -> INT_CONST (read s :: Int) }
  @floats      { \s -> FLOAT_CONST (read s :: Float)}
  :=           { \s -> ASSIGN}
  \(           { \s -> LPAREN }
  \)           { \s -> RPAREN }
  \+           { \s -> PLUS }
  \-           { \s -> MINUS }
  \*           { \s -> MUL }
  \;           { \s -> SEMI }

{
data Token
  = BEGIN | BOOL | DO | ELSE | END | FALSE | FI | FLOAT
  | IF | INT | OD | PROC | READ | REF | THEN | TRUE | VAL
  | WHILE | IDENT String | STRING_CONST String | ASSIGN | LPAREN
  | RPAREN | PLUS | MINUS | MUL | SEMI | INT_CONST Int
  | BOOL_CONST Bool | WRITE | FLOAT_CONST Float
    deriving (Eq, Show)

main
 = do
    s <- getContents
    let tokens = alexScanTokens s
    print (tokens)
}
