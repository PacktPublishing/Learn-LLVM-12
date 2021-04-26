%require "3.2"
%language "c++"
%defines "Parser.h"
%define api.namespace {tinylang}
%define api.parser.class {Parser}
%define api.token.raw
%define api.token.prefix {T_}
%define api.value.type variant
%token
  identifier integer_literal string_literal
  PLUS MINUS STAR SLASH COLONEQUAL PERIOD COMMA SEMI COLON
  EQUAL HASH LESS GREATER LESSEQUAL GREATEREQUAL L_PAREN
  R_PAREN AND BEGIN CONST DIV DO END ELSE FROM IF IMPORT
  MOD MODULE NOT OR PROCEDURE RETURN THEN VAR WHILE
%code requires {
#define YY_DECL tinylang::Parser::token yylex()
}
%%
compilationUnit
  : MODULE identifier SEMI imports block identifier PERIOD ;
imports : %empty | import imports ;
import
  : FROM identifier IMPORT identList SEMI
  | IMPORT identList SEMI ;
block : declarations blockBody END ;
declarations : %empty | declaration declarations ;
declaration
  : CONST constantDeclaration
  | VAR variableDeclaration
  | procedureDeclaration SEMI ;
constantDeclaration
  : %empty
  | identifier EQUAL expression SEMI constantDeclaration ;
variableDeclaration
  : %empty
  | identList COLON qualident SEMI variableDeclaration ;
procedureDeclaration
  : PROCEDURE identifier formalParameters SEMI
    block identifier ;
formalParameters
  : %empty | L_PAREN formalParameterList R_PAREN resultType ;
formalParameterList
  : %empty | formalParameter SEMI formalParameterList ;
formalParameter
  : varParam identList COLON qualident ;
varParam : %empty | VAR ;
resultType : %empty | COLON qualident ;
blockBody : %empty | BEGIN statementSequence ;
statementSequence
  : statement SEMI statementSequence | statement ;
statement
  : qualident COLONEQUAL expression
  | qualident actualParams
  | ifStatement | whileStatement | RETURN returnExpr ;
actualParams  : %empty | L_PAREN actualParamList R_PAREN ;
actualParamList : %empty | expList ;
returnExpr : %empty | expression ;
ifStatement
  : IF expression THEN statementSequence
    elseStatement END ;
elseStatement
  : %empty | ELSE statementSequence ;
whileStatement
  : WHILE expression DO statementSequence END ;
expList
  : expression COMMA expList | expression ;
expression
  : prefixedExpression relation prefixedExpression
  | prefixedExpression ;
relation
  : EQUAL | HASH | LESS | LESSEQUAL | GREATER | GREATEREQUAL ;
prefixedExpression: prefixOperator simpleExpression ;
simpleExpression
  : term addOperator simpleExpression | term ;
prefixOperator : %empty | PLUS | MINUS;
addOperator : PLUS | MINUS | OR ;
term : factor mulOperator term | factor ;
mulOperator : STAR | SLASH | DIV | MOD | AND ;
factor
  : integer_literal | L_PAREN expression R_PAREN | NOT factor
  | qualident actualParams ;
qualident : identifier PERIOD qualident | identifier ;
identList : identifier COMMA identList | identifier ;
