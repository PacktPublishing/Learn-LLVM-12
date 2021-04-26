%{
#include "Parser.h"
%}
%option noyywrap nounput noinput batch
id       [a-zA-Z_0-9]*
digit    [0-9]
hexdigit [0-9A-F]
space    [ \t\r]
%%
{space}+
{digit}+      return tinylang::Parser::token::T_integer_literal;
{hexdigit}+H  return tinylang::Parser::token::T_integer_literal;
"+"             return tinylang::Parser::token::T_PLUS;
"-"             return tinylang::Parser::token::T_MINUS;
"*"             return tinylang::Parser::token::T_STAR;
"/"             return tinylang::Parser::token::T_SLASH;
":="            return tinylang::Parser::token::T_COLONEQUAL;
"."             return tinylang::Parser::token::T_PERIOD;
","             return tinylang::Parser::token::T_COMMA;
";"             return tinylang::Parser::token::T_SEMI;
":"             return tinylang::Parser::token::T_COLON;
"="             return tinylang::Parser::token::T_EQUAL;
"#"             return tinylang::Parser::token::T_HASH;
"<"             return tinylang::Parser::token::T_LESS;
">"             return tinylang::Parser::token::T_GREATER;
"<="            return tinylang::Parser::token::T_LESSEQUAL;
">="            return tinylang::Parser::token::T_GREATEREQUAL;
"("             return tinylang::Parser::token::T_L_PAREN;
")"             return tinylang::Parser::token::T_R_PAREN;
"AND"           return tinylang::Parser::token::T_AND;
"BEGIN"         return tinylang::Parser::token::T_BEGIN;
"CONST"         return tinylang::Parser::token::T_CONST;
"DIV"           return tinylang::Parser::token::T_DIV;
"DO"            return tinylang::Parser::token::T_DO;
"END"           return tinylang::Parser::token::T_END;
"ELSE"          return tinylang::Parser::token::T_ELSE;
"FROM"          return tinylang::Parser::token::T_FROM;
"IF"            return tinylang::Parser::token::T_IF;
"IMPORT"        return tinylang::Parser::token::T_IMPORT;
"MOD"           return tinylang::Parser::token::T_MOD;
"MODULE"        return tinylang::Parser::token::T_MODULE;
"NOT"           return tinylang::Parser::token::T_NOT;
"OR"            return tinylang::Parser::token::T_OR;
"PROCEDURE"     return tinylang::Parser::token::T_PROCEDURE;
"RETURN"        return tinylang::Parser::token::T_RETURN;
"THEN"          return tinylang::Parser::token::T_THEN;
"VAR"           return tinylang::Parser::token::T_VAR;
"WHILE"         return tinylang::Parser::token::T_WHILE;
{id}            return tinylang::Parser::token::T_identifier;
