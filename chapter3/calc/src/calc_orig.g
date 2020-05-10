%token number, ident
%start calc
%%
calc
  : ("with" ident ("," ident)* ":") expr
  ;

expr
  : term ( ( "+" | "-" ) term )*
  ;

term
  : factor ( ( "*" | "/" ) factor )*
  ;

factor
  : number
  | "(" expr ")"
  ;