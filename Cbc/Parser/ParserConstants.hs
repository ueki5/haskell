module Cbc.Parser.ParserConstants where
data ParserConstants = 
    EOF
  | SPACES
  | BLOCK_COMMENT
  | LINE_COMMENT
  | VOID
  | CHAR
  | SHORT
  | INT
  | LONG
  | STRUCT
  | UNION
  | ENUM
  | STATIC
  | EXTERN
  | CONST
  | SIGNED
  | UNSIGNED
  | IF
  | ELSE
  | SWITCH
  | CASE
  | DEFAULT_
  | WHILE
  | DO
  | FOR
  | RETURN
  | BREAK
  | CONTINUE
  | GOTO
  | TYPEDEF
  | IMPORT
  | SIZEOF
  | IDENTIFIER
  | INTEGER
  | CHARACTER
  | STRING

data LexicalState = 
    DEFAULT
  | IN_BLOCK_COMMENT
  | IN_CHARACTER
  | CHARACTER_TERM
  | IN_STRING

tokenImage = [
    "<EOF>",
    "<SPACES>",
    "\"/*\"",
    "<token of kind 3>",
    "\"*/\"",
    "<LINE_COMMENT>",
    "\"void\"",
    "\"char\"",
    "\"short\"",
    "\"int\"",
    "\"long\"",
    "\"struct\"",
    "\"union\"",
    "\"enum\"",
    "\"static\"",
    "\"extern\"",
    "\"const\"",
    "\"signed\"",
    "\"unsigned\"",
    "\"if\"",
    "\"else\"",
    "\"switch\"",
    "\"case\"",
    "\"default\"",
    "\"while\"",
    "\"do\"",
    "\"for\"",
    "\"return\"",
    "\"break\"",
    "\"continue\"",
    "\"goto\"",
    "\"typedef\"",
    "\"import\"",
    "\"sizeof\"",
    "<IDENTIFIER>",
    "<INTEGER>",
    "\"\\\'\"",
    "<token of kind 37>",
    "<token of kind 38>",
    "<token of kind 39>",
    "\"\\\'\"",
    "\"\\\"\"",
    "<token of kind 42>",
    "<token of kind 43>",
    "<token of kind 44>",
    "\"\\\"\"",
    "\"(\"",
    "\".\"",
    "\";\"",
    "\"=\"",
    "\",\"",
    "\")\"",
    "\"...\"",
    "\"{\"",
    "\"}\"",
    "\"[\"",
    "\"]\"",
    "\"*\"",
    "\":\"",
    "\"+=\"",
    "\"-=\"",
    "\"*=\"",
    "\"/=\"",
    "\"%=\"",
    "\"&=\"",
    "\"|=\"",
    "\"^=\"",
    "\"<<=\"",
    "\">>=\"",
    "\"?\"",
    "\"||\"",
    "\"&&\"",
    "\">\"",
    "\"<\"",
    "\">=\"",
    "\"<=\"",
    "\"==\"",
    "\"!=\"",
    "\"|\"",
    "\"^\"",
    "\"&\"",
    "\">>\"",
    "\"<<\"",
    "\"+\"",
    "\"-\"",
    "\"/\"",
    "\"%\"",
    "\"++\"",
    "\"--\"",
    "\"!\"",
    "\"~\"",
    "\"->\""]
