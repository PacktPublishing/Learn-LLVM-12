%token number, ident
%start calc
%%
calc
  :                                     {. std::vector<llvm::StringRef> vars; .}
    ("with" ident                       {. vars.push_back(llvm::StringRef(Tok.getLoc(), Tok.getLength()); .}
      ("," ident                        {. vars.push_back(llvm::StringRef(Tok.getLoc(), Tok.getLength()); .}
      )* ":")?
      expr {. /* CODE*/ .}
  ;

expr
  : term {. /* CODE*/ .} ( ( "+" | "-" ){. /* CODE*/ .} term {. /* CODE*/ .})*
  ;

term
  : factor {. /* CODE*/ .} ( ( "*" | "/" ){. /* CODE*/ .} factor {. /* CODE*/ .})*
  ;

factor
  : number {. /* CODE*/ .}
  | ident {. /* CODE*/ .}
  | "(" expr {. /* CODE*/ .} ")"
  ;