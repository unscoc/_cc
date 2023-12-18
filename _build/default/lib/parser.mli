
(* The type of tokens. *)

type token = 
  | XOR_EQ
  | WHILE
  | VOLATILE
  | UNION
  | TYPE_ID of (string)
  | TYPEDEF
  | TVOID
  | TUNSIGNED
  | TSIGNED
  | TSHORT
  | TLONG
  | TINT
  | TFLOAT
  | TDOUBLE
  | TCHAR
  | SWITCH
  | SUB_EQ
  | STRUCT
  | STR of (string)
  | STATIC
  | STAR
  | SIZEOF
  | SEMI
  | RSHIFT_EQ
  | RSHIFT
  | RPAREN
  | RETURN
  | REGISTER
  | RBRACKET
  | RBRACE
  | QUESTION
  | PLUS
  | OR_EQ
  | OROR
  | OR
  | NOT
  | NORETURN
  | NE
  | MUL_EQ
  | MOD_EQ
  | MOD
  | MINUS
  | LT
  | LSHIFT_EQ
  | LSHIFT
  | LPAREN
  | LE
  | LBRACKET
  | LBRACE
  | INT of (string)
  | INLINE
  | INC
  | IF
  | ID of (string)
  | HAT
  | GT
  | GOTO
  | GE
  | FOR
  | FLOAT of (string)
  | EXTERN
  | EQEQ
  | EQ
  | EOF
  | ENUM
  | ELSE
  | ELLIPSIS
  | DOT
  | DO
  | DIV_EQ
  | DIV
  | DEFAULT
  | DEC
  | CONTINUE
  | CONST
  | COMMA
  | COLON
  | CHAR of (string)
  | CASE
  | BREAK
  | BANG
  | AUTO
  | ARROW
  | AND_EQ
  | ANDAND
  | AND
  | ADD_EQ

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val translation_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (int list)
