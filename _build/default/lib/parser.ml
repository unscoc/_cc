
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | XOR_EQ
    | WHILE
    | VOLATILE
    | UNION
    | TYPE_ID of (
# 54 "lib/parser.mly"
      (string)
# 19 "lib/parser.ml"
  )
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
    | STR of (
# 53 "lib/parser.mly"
      (string)
# 37 "lib/parser.ml"
  )
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
    | INT of (
# 51 "lib/parser.mly"
       (string)
# 72 "lib/parser.ml"
  )
    | INLINE
    | INC
    | IF
    | ID of (
# 54 "lib/parser.mly"
      (string)
# 80 "lib/parser.ml"
  )
    | HAT
    | GT
    | GOTO
    | GE
    | FOR
    | FLOAT of (
# 52 "lib/parser.mly"
       (string)
# 90 "lib/parser.ml"
  )
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
    | CHAR of (
# 51 "lib/parser.mly"
       (string)
# 112 "lib/parser.ml"
  )
    | CASE
    | BREAK
    | BANG
    | AUTO
    | ARROW
    | AND_EQ
    | ANDAND
    | AND
    | ADD_EQ
  
end

include MenhirBasics

# 1 "lib/parser.mly"
  
    open Ast 
    open Env 

    type declarator =
    | DeclPtr of declarator
    | DeclIdent of string
    | DeclArr of declarator * expr
    | DeclFun of declarator * decl list

    let make_decl ty d = 
      let name = ref "" in
      let rec aux ty = function
      | DeclPtr d -> aux (TPtr ty) d 
      | DeclIdent n -> name := n; ty 
      | DeclArr(d,sz) -> aux (TArr(ty,sz)) d 
      | DeclFun(d,dl) -> aux (TFun(ty,dl)) d
      in (!name, aux ty d)

    let make_decls ty dl =
      List.map (fun d -> make_decl ty d) dl

    let make_decls_with_init ty init_decl_list =
      List.map 
      (
        function 
        | (d,Some init) -> push_def (VarDef(make_decl ty d,init))
        | (d,None) -> push_def (Decl (make_decl ty d))
      ) init_decl_list

    let conv_ident = function
    | Some s -> s 
    | None -> ""



# 165 "lib/parser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_translation_unit) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: translation_unit. *)

  | MenhirState002 : (('s, _menhir_box_translation_unit) _menhir_cell1_UNION, _menhir_box_translation_unit) _menhir_state
    (** State 002.
        Stack shape : UNION.
        Start symbol: translation_unit. *)

  | MenhirState006 : ((('s, _menhir_box_translation_unit) _menhir_cell1_UNION, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_state
    (** State 006.
        Stack shape : UNION option(ident).
        Start symbol: translation_unit. *)

  | MenhirState017 : (('s, _menhir_box_translation_unit) _menhir_cell1_STRUCT, _menhir_box_translation_unit) _menhir_state
    (** State 017.
        Stack shape : STRUCT.
        Start symbol: translation_unit. *)

  | MenhirState019 : ((('s, _menhir_box_translation_unit) _menhir_cell1_STRUCT, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_state
    (** State 019.
        Stack shape : STRUCT option(ident).
        Start symbol: translation_unit. *)

  | MenhirState020 : (('s, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_state
    (** State 020.
        Stack shape : ENUM.
        Start symbol: translation_unit. *)

  | MenhirState022 : ((('s, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_state
    (** State 022.
        Stack shape : ENUM option(ident).
        Start symbol: translation_unit. *)

  | MenhirState024 : (((('s, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_cell1_enum_list, _menhir_box_translation_unit) _menhir_state
    (** State 024.
        Stack shape : ENUM option(ident) enum_list.
        Start symbol: translation_unit. *)

  | MenhirState025 : ((((('s, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_cell1_enum_list, _menhir_box_translation_unit) _menhir_cell1_COMMA, _menhir_box_translation_unit) _menhir_state
    (** State 025.
        Stack shape : ENUM option(ident) enum_list COMMA.
        Start symbol: translation_unit. *)

  | MenhirState027 : (('s, _menhir_box_translation_unit) _menhir_cell1_enum_const, _menhir_box_translation_unit) _menhir_state
    (** State 027.
        Stack shape : enum_const.
        Start symbol: translation_unit. *)

  | MenhirState029 : (('s, _menhir_box_translation_unit) _menhir_cell1_STAR, _menhir_box_translation_unit) _menhir_state
    (** State 029.
        Stack shape : STAR.
        Start symbol: translation_unit. *)

  | MenhirState030 : (('s, _menhir_box_translation_unit) _menhir_cell1_SIZEOF, _menhir_box_translation_unit) _menhir_state
    (** State 030.
        Stack shape : SIZEOF.
        Start symbol: translation_unit. *)

  | MenhirState031 : (('s, _menhir_box_translation_unit) _menhir_cell1_PLUS, _menhir_box_translation_unit) _menhir_state
    (** State 031.
        Stack shape : PLUS.
        Start symbol: translation_unit. *)

  | MenhirState032 : (('s, _menhir_box_translation_unit) _menhir_cell1_NOT, _menhir_box_translation_unit) _menhir_state
    (** State 032.
        Stack shape : NOT.
        Start symbol: translation_unit. *)

  | MenhirState033 : (('s, _menhir_box_translation_unit) _menhir_cell1_MINUS, _menhir_box_translation_unit) _menhir_state
    (** State 033.
        Stack shape : MINUS.
        Start symbol: translation_unit. *)

  | MenhirState034 : (('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 034.
        Stack shape : LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState036 : (('s, _menhir_box_translation_unit) _menhir_cell1_INC, _menhir_box_translation_unit) _menhir_state
    (** State 036.
        Stack shape : INC.
        Start symbol: translation_unit. *)

  | MenhirState037 : (('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 037.
        Stack shape : LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState040 : (('s, _menhir_box_translation_unit) _menhir_cell1_DEC, _menhir_box_translation_unit) _menhir_state
    (** State 040.
        Stack shape : DEC.
        Start symbol: translation_unit. *)

  | MenhirState042 : (('s, _menhir_box_translation_unit) _menhir_cell1_BANG, _menhir_box_translation_unit) _menhir_state
    (** State 042.
        Stack shape : BANG.
        Start symbol: translation_unit. *)

  | MenhirState043 : (('s, _menhir_box_translation_unit) _menhir_cell1_AND, _menhir_box_translation_unit) _menhir_state
    (** State 043.
        Stack shape : AND.
        Start symbol: translation_unit. *)

  | MenhirState047 : (('s, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_state
    (** State 047.
        Stack shape : postfix_expr.
        Start symbol: translation_unit. *)

  | MenhirState049 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 049.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState051 : (('s, _menhir_box_translation_unit) _menhir_cell1_shift_expr, _menhir_box_translation_unit) _menhir_state
    (** State 051.
        Stack shape : shift_expr.
        Start symbol: translation_unit. *)

  | MenhirState053 : (('s, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr, _menhir_box_translation_unit) _menhir_state
    (** State 053.
        Stack shape : multiplicative_expr.
        Start symbol: translation_unit. *)

  | MenhirState055 : (('s, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr, _menhir_box_translation_unit) _menhir_state
    (** State 055.
        Stack shape : multiplicative_expr.
        Start symbol: translation_unit. *)

  | MenhirState057 : (('s, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr, _menhir_box_translation_unit) _menhir_state
    (** State 057.
        Stack shape : multiplicative_expr.
        Start symbol: translation_unit. *)

  | MenhirState061 : (('s, _menhir_box_translation_unit) _menhir_cell1_additive_expr, _menhir_box_translation_unit) _menhir_state
    (** State 061.
        Stack shape : additive_expr.
        Start symbol: translation_unit. *)

  | MenhirState063 : (('s, _menhir_box_translation_unit) _menhir_cell1_additive_expr, _menhir_box_translation_unit) _menhir_state
    (** State 063.
        Stack shape : additive_expr.
        Start symbol: translation_unit. *)

  | MenhirState065 : (('s, _menhir_box_translation_unit) _menhir_cell1_shift_expr, _menhir_box_translation_unit) _menhir_state
    (** State 065.
        Stack shape : shift_expr.
        Start symbol: translation_unit. *)

  | MenhirState068 : (('s, _menhir_box_translation_unit) _menhir_cell1_relational_expr, _menhir_box_translation_unit) _menhir_state
    (** State 068.
        Stack shape : relational_expr.
        Start symbol: translation_unit. *)

  | MenhirState071 : (('s, _menhir_box_translation_unit) _menhir_cell1_relational_expr, _menhir_box_translation_unit) _menhir_state
    (** State 071.
        Stack shape : relational_expr.
        Start symbol: translation_unit. *)

  | MenhirState073 : (('s, _menhir_box_translation_unit) _menhir_cell1_relational_expr, _menhir_box_translation_unit) _menhir_state
    (** State 073.
        Stack shape : relational_expr.
        Start symbol: translation_unit. *)

  | MenhirState075 : (('s, _menhir_box_translation_unit) _menhir_cell1_relational_expr, _menhir_box_translation_unit) _menhir_state
    (** State 075.
        Stack shape : relational_expr.
        Start symbol: translation_unit. *)

  | MenhirState078 : (('s, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr, _menhir_box_translation_unit) _menhir_state
    (** State 078.
        Stack shape : logical_or_expr.
        Start symbol: translation_unit. *)

  | MenhirState080 : (('s, _menhir_box_translation_unit) _menhir_cell1_logical_and_expr, _menhir_box_translation_unit) _menhir_state
    (** State 080.
        Stack shape : logical_and_expr.
        Start symbol: translation_unit. *)

  | MenhirState082 : (('s, _menhir_box_translation_unit) _menhir_cell1_inclusive_or_expr, _menhir_box_translation_unit) _menhir_state
    (** State 082.
        Stack shape : inclusive_or_expr.
        Start symbol: translation_unit. *)

  | MenhirState084 : (('s, _menhir_box_translation_unit) _menhir_cell1_exclusive_or_expr, _menhir_box_translation_unit) _menhir_state
    (** State 084.
        Stack shape : exclusive_or_expr.
        Start symbol: translation_unit. *)

  | MenhirState086 : (('s, _menhir_box_translation_unit) _menhir_cell1_equality_expr, _menhir_box_translation_unit) _menhir_state
    (** State 086.
        Stack shape : equality_expr.
        Start symbol: translation_unit. *)

  | MenhirState088 : (('s, _menhir_box_translation_unit) _menhir_cell1_equality_expr, _menhir_box_translation_unit) _menhir_state
    (** State 088.
        Stack shape : equality_expr.
        Start symbol: translation_unit. *)

  | MenhirState091 : (('s, _menhir_box_translation_unit) _menhir_cell1_and_expr, _menhir_box_translation_unit) _menhir_state
    (** State 091.
        Stack shape : and_expr.
        Start symbol: translation_unit. *)

  | MenhirState097 : (('s, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_state
    (** State 097.
        Stack shape : expr.
        Start symbol: translation_unit. *)

  | MenhirState100 : ((('s, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_state
    (** State 100.
        Stack shape : logical_or_expr expr.
        Start symbol: translation_unit. *)

  | MenhirState103 : (('s, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr, _menhir_box_translation_unit) _menhir_state
    (** State 103.
        Stack shape : logical_or_expr.
        Start symbol: translation_unit. *)

  | MenhirState106 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 106.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState108 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 108.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState110 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 110.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState112 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 112.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState114 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 114.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState116 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 116.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState118 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 118.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState120 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 120.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState122 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 122.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState124 : (('s, _menhir_box_translation_unit) _menhir_cell1_unary_expr, _menhir_box_translation_unit) _menhir_state
    (** State 124.
        Stack shape : unary_expr.
        Start symbol: translation_unit. *)

  | MenhirState130 : ((('s, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_cell1_argument_expr_list, _menhir_box_translation_unit) _menhir_state
    (** State 130.
        Stack shape : postfix_expr argument_expr_list.
        Start symbol: translation_unit. *)

  | MenhirState132 : (('s, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_state
    (** State 132.
        Stack shape : postfix_expr.
        Start symbol: translation_unit. *)

  | MenhirState136 : (('s, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_state
    (** State 136.
        Stack shape : postfix_expr.
        Start symbol: translation_unit. *)

  | MenhirState139 : (('s, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_state
    (** State 139.
        Stack shape : postfix_expr.
        Start symbol: translation_unit. *)

  | MenhirState145 : (('s, _menhir_box_translation_unit) _menhir_cell1_type_spec, _menhir_box_translation_unit) _menhir_state
    (** State 145.
        Stack shape : type_spec.
        Start symbol: translation_unit. *)

  | MenhirState146 : (('s, _menhir_box_translation_unit) _menhir_cell1_type_qual, _menhir_box_translation_unit) _menhir_state
    (** State 146.
        Stack shape : type_qual.
        Start symbol: translation_unit. *)

  | MenhirState152 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_state
    (** State 152.
        Stack shape : LPAREN type_name.
        Start symbol: translation_unit. *)

  | MenhirState153 : (((('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_state
    (** State 153.
        Stack shape : LPAREN type_name LBRACE.
        Start symbol: translation_unit. *)

  | MenhirState154 : (('s, _menhir_box_translation_unit) _menhir_cell1_LBRACKET, _menhir_box_translation_unit) _menhir_state
    (** State 154.
        Stack shape : LBRACKET.
        Start symbol: translation_unit. *)

  | MenhirState156 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LBRACKET, _menhir_box_translation_unit) _menhir_cell1_constant_expr, _menhir_box_translation_unit) _menhir_state
    (** State 156.
        Stack shape : LBRACKET constant_expr.
        Start symbol: translation_unit. *)

  | MenhirState157 : (('s, _menhir_box_translation_unit) _menhir_cell1_DOT, _menhir_box_translation_unit) _menhir_state
    (** State 157.
        Stack shape : DOT.
        Start symbol: translation_unit. *)

  | MenhirState158 : ((('s, _menhir_box_translation_unit) _menhir_cell1_DOT, _menhir_box_translation_unit) _menhir_cell1_ident, _menhir_box_translation_unit) _menhir_state
    (** State 158.
        Stack shape : DOT ident.
        Start symbol: translation_unit. *)

  | MenhirState162 : (('s, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_state
    (** State 162.
        Stack shape : LBRACE.
        Start symbol: translation_unit. *)

  | MenhirState163 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_state
    (** State 163.
        Stack shape : LBRACE init_list.
        Start symbol: translation_unit. *)

  | MenhirState164 : (((('s, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_cell1_COMMA, _menhir_box_translation_unit) _menhir_state
    (** State 164.
        Stack shape : LBRACE init_list COMMA.
        Start symbol: translation_unit. *)

  | MenhirState168 : ((((('s, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_cell1_COMMA, _menhir_box_translation_unit) _menhir_cell1_desig, _menhir_box_translation_unit) _menhir_state
    (** State 168.
        Stack shape : LBRACE init_list COMMA desig.
        Start symbol: translation_unit. *)

  | MenhirState174 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_desig, _menhir_box_translation_unit) _menhir_state
    (** State 174.
        Stack shape : LBRACE desig.
        Start symbol: translation_unit. *)

  | MenhirState176 : ((((('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_state
    (** State 176.
        Stack shape : LPAREN type_name LBRACE init_list.
        Start symbol: translation_unit. *)

  | MenhirState180 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list, _menhir_box_translation_unit) _menhir_state
    (** State 180.
        Stack shape : LPAREN spec_qual_list.
        Start symbol: translation_unit. *)

  | MenhirState181 : (('s, _menhir_box_translation_unit) _menhir_cell1_STAR, _menhir_box_translation_unit) _menhir_state
    (** State 181.
        Stack shape : STAR.
        Start symbol: translation_unit. *)

  | MenhirState182 : (('s, _menhir_box_translation_unit) _menhir_cell1_type_qual, _menhir_box_translation_unit) _menhir_state
    (** State 182.
        Stack shape : type_qual.
        Start symbol: translation_unit. *)

  | MenhirState185 : (('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 185.
        Stack shape : LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState190 : (('s, _menhir_box_translation_unit) _menhir_cell1_LBRACKET, _menhir_box_translation_unit) _menhir_state
    (** State 190.
        Stack shape : LBRACKET.
        Start symbol: translation_unit. *)

  | MenhirState199 : (('s, _menhir_box_translation_unit) _menhir_cell1_pointer, _menhir_box_translation_unit) _menhir_state
    (** State 199.
        Stack shape : pointer.
        Start symbol: translation_unit. *)

  | MenhirState201 : (('s, _menhir_box_translation_unit) _menhir_cell1_direct_abstract_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 201.
        Stack shape : direct_abstract_declarator.
        Start symbol: translation_unit. *)

  | MenhirState205 : (('s, _menhir_box_translation_unit) _menhir_cell1_parameter_list, _menhir_box_translation_unit) _menhir_state
    (** State 205.
        Stack shape : parameter_list.
        Start symbol: translation_unit. *)

  | MenhirState209 : (('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs_sub, _menhir_box_translation_unit) _menhir_state
    (** State 209.
        Stack shape : decl_specs_sub.
        Start symbol: translation_unit. *)

  | MenhirState211 : (('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_state
    (** State 211.
        Stack shape : decl_specs.
        Start symbol: translation_unit. *)

  | MenhirState212 : (('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 212.
        Stack shape : LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState214 : (('s, _menhir_box_translation_unit) _menhir_cell1_pointer, _menhir_box_translation_unit) _menhir_state
    (** State 214.
        Stack shape : pointer.
        Start symbol: translation_unit. *)

  | MenhirState216 : (('s, _menhir_box_translation_unit) _menhir_cell1_direct_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 216.
        Stack shape : direct_declarator.
        Start symbol: translation_unit. *)

  | MenhirState221 : (('s, _menhir_box_translation_unit) _menhir_cell1_direct_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 221.
        Stack shape : direct_declarator.
        Start symbol: translation_unit. *)

  | MenhirState236 : (('s, _menhir_box_translation_unit) _menhir_cell1_direct_abstract_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 236.
        Stack shape : direct_abstract_declarator.
        Start symbol: translation_unit. *)

  | MenhirState244 : ((('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_state
    (** State 244.
        Stack shape : LPAREN type_name.
        Start symbol: translation_unit. *)

  | MenhirState249 : ((('s, _menhir_box_translation_unit) _menhir_cell1_SIZEOF, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 249.
        Stack shape : SIZEOF LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState251 : (((('s, _menhir_box_translation_unit) _menhir_cell1_SIZEOF, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_state
    (** State 251.
        Stack shape : SIZEOF LPAREN type_name.
        Start symbol: translation_unit. *)

  | MenhirState260 : (('s, _menhir_box_translation_unit) _menhir_cell1_struct_decl, _menhir_box_translation_unit) _menhir_state
    (** State 260.
        Stack shape : struct_decl.
        Start symbol: translation_unit. *)

  | MenhirState261 : (('s, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list, _menhir_box_translation_unit) _menhir_state
    (** State 261.
        Stack shape : spec_qual_list.
        Start symbol: translation_unit. *)

  | MenhirState262 : (('s, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_state
    (** State 262.
        Stack shape : LPAREN.
        Start symbol: translation_unit. *)

  | MenhirState263 : (('s, _menhir_box_translation_unit) _menhir_cell1_pointer, _menhir_box_translation_unit) _menhir_state
    (** State 263.
        Stack shape : pointer.
        Start symbol: translation_unit. *)

  | MenhirState265 : ((('s, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list, _menhir_box_translation_unit) _menhir_cell1_struct_declarator_list, _menhir_box_translation_unit) _menhir_state
    (** State 265.
        Stack shape : spec_qual_list struct_declarator_list.
        Start symbol: translation_unit. *)

  | MenhirState282 : (('s, _menhir_box_translation_unit) _menhir_cell1_external_decl, _menhir_box_translation_unit) _menhir_state
    (** State 282.
        Stack shape : external_decl.
        Start symbol: translation_unit. *)

  | MenhirState284 : (('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_state
    (** State 284.
        Stack shape : decl_specs.
        Start symbol: translation_unit. *)

  | MenhirState288 : ((('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_cell1_init_declarator_list, _menhir_box_translation_unit) _menhir_state
    (** State 288.
        Stack shape : decl_specs init_declarator_list.
        Start symbol: translation_unit. *)

  | MenhirState290 : (('s, _menhir_box_translation_unit) _menhir_cell1_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 290.
        Stack shape : declarator.
        Start symbol: translation_unit. *)

  | MenhirState291 : ((('s, _menhir_box_translation_unit) _menhir_cell1_declarator, _menhir_box_translation_unit) _menhir_cell1_EQ, _menhir_box_translation_unit) _menhir_state
    (** State 291.
        Stack shape : declarator EQ.
        Start symbol: translation_unit. *)

  | MenhirState294 : ((('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_cell1_declarator, _menhir_box_translation_unit) _menhir_state
    (** State 294.
        Stack shape : decl_specs declarator.
        Start symbol: translation_unit. *)

  | MenhirState296 : (('s, _menhir_box_translation_unit) _menhir_cell1_enter_scope, _menhir_box_translation_unit) _menhir_state
    (** State 296.
        Stack shape : enter_scope.
        Start symbol: translation_unit. *)

  | MenhirState298 : (('s, _menhir_box_translation_unit) _menhir_cell1_WHILE, _menhir_box_translation_unit) _menhir_state
    (** State 298.
        Stack shape : WHILE.
        Start symbol: translation_unit. *)

  | MenhirState300 : ((('s, _menhir_box_translation_unit) _menhir_cell1_WHILE, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_state
    (** State 300.
        Stack shape : WHILE expr.
        Start symbol: translation_unit. *)

  | MenhirState302 : (('s, _menhir_box_translation_unit) _menhir_cell1_SWITCH, _menhir_box_translation_unit) _menhir_state
    (** State 302.
        Stack shape : SWITCH.
        Start symbol: translation_unit. *)

  | MenhirState304 : ((('s, _menhir_box_translation_unit) _menhir_cell1_SWITCH, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_state
    (** State 304.
        Stack shape : SWITCH expr.
        Start symbol: translation_unit. *)

  | MenhirState306 : (((('s, _menhir_box_translation_unit) _menhir_cell1_SWITCH, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_cell1_enter_scope, _menhir_box_translation_unit) _menhir_state
    (** State 306.
        Stack shape : SWITCH expr enter_scope.
        Start symbol: translation_unit. *)

  | MenhirState308 : (('s, _menhir_box_translation_unit) _menhir_cell1_DEFAULT, _menhir_box_translation_unit) _menhir_state
    (** State 308.
        Stack shape : DEFAULT.
        Start symbol: translation_unit. *)

  | MenhirState310 : (('s, _menhir_box_translation_unit) _menhir_cell1_RETURN, _menhir_box_translation_unit) _menhir_state
    (** State 310.
        Stack shape : RETURN.
        Start symbol: translation_unit. *)

  | MenhirState315 : (('s, _menhir_box_translation_unit) _menhir_cell1_IF, _menhir_box_translation_unit) _menhir_state
    (** State 315.
        Stack shape : IF.
        Start symbol: translation_unit. *)

  | MenhirState317 : ((('s, _menhir_box_translation_unit) _menhir_cell1_IF, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_state
    (** State 317.
        Stack shape : IF expr.
        Start symbol: translation_unit. *)

  | MenhirState319 : (('s, _menhir_box_translation_unit) _menhir_cell1_GOTO, _menhir_box_translation_unit) _menhir_state
    (** State 319.
        Stack shape : GOTO.
        Start symbol: translation_unit. *)

  | MenhirState323 : (('s, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_state
    (** State 323.
        Stack shape : FOR.
        Start symbol: translation_unit. *)

  | MenhirState325 : ((('s, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_state
    (** State 325.
        Stack shape : FOR option(expr).
        Start symbol: translation_unit. *)

  | MenhirState327 : (((('s, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_state
    (** State 327.
        Stack shape : FOR option(expr) option(expr).
        Start symbol: translation_unit. *)

  | MenhirState329 : ((((('s, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_state
    (** State 329.
        Stack shape : FOR option(expr) option(expr) option(expr).
        Start symbol: translation_unit. *)

  | MenhirState330 : (('s, _menhir_box_translation_unit) _menhir_cell1_DO, _menhir_box_translation_unit) _menhir_state
    (** State 330.
        Stack shape : DO.
        Start symbol: translation_unit. *)

  | MenhirState337 : ((('s, _menhir_box_translation_unit) _menhir_cell1_DO, _menhir_box_translation_unit) _menhir_cell1_stmt, _menhir_box_translation_unit) _menhir_state
    (** State 337.
        Stack shape : DO stmt.
        Start symbol: translation_unit. *)

  | MenhirState348 : (('s, _menhir_box_translation_unit) _menhir_cell1_ident, _menhir_box_translation_unit) _menhir_state
    (** State 348.
        Stack shape : ident.
        Start symbol: translation_unit. *)

  | MenhirState351 : (('s, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_state
    (** State 351.
        Stack shape : decl_specs.
        Start symbol: translation_unit. *)

  | MenhirState356 : (((('s, _menhir_box_translation_unit) _menhir_cell1_IF, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_cell1_stmt, _menhir_box_translation_unit) _menhir_state
    (** State 356.
        Stack shape : IF expr stmt.
        Start symbol: translation_unit. *)

  | MenhirState359 : (('s, _menhir_box_translation_unit) _menhir_cell1_item, _menhir_box_translation_unit) _menhir_state
    (** State 359.
        Stack shape : item.
        Start symbol: translation_unit. *)

  | MenhirState361 : (('s, _menhir_box_translation_unit) _menhir_cell1_CASE, _menhir_box_translation_unit) _menhir_state
    (** State 361.
        Stack shape : CASE.
        Start symbol: translation_unit. *)

  | MenhirState363 : ((('s, _menhir_box_translation_unit) _menhir_cell1_CASE, _menhir_box_translation_unit) _menhir_cell1_conditional_expr, _menhir_box_translation_unit) _menhir_state
    (** State 363.
        Stack shape : CASE conditional_expr.
        Start symbol: translation_unit. *)

  | MenhirState366 : ((((('s, _menhir_box_translation_unit) _menhir_cell1_SWITCH, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_cell1_enter_scope, _menhir_box_translation_unit) _menhir_cell1_list_case_or_default_, _menhir_box_translation_unit) _menhir_state
    (** State 366.
        Stack shape : SWITCH expr enter_scope list(case_or_default).
        Start symbol: translation_unit. *)

  | MenhirState368 : (('s, _menhir_box_translation_unit) _menhir_cell1_case_or_default, _menhir_box_translation_unit) _menhir_state
    (** State 368.
        Stack shape : case_or_default.
        Start symbol: translation_unit. *)

  | MenhirState372 : ((('s, _menhir_box_translation_unit) _menhir_cell1_enter_scope, _menhir_box_translation_unit) _menhir_cell1_list_item_, _menhir_box_translation_unit) _menhir_state
    (** State 372.
        Stack shape : enter_scope list(item).
        Start symbol: translation_unit. *)


and ('s, 'r) _menhir_cell1_additive_expr = 
  | MenhirCell1_additive_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_and_expr = 
  | MenhirCell1_and_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_argument_expr_list = 
  | MenhirCell1_argument_expr_list of 's * ('s, 'r) _menhir_state * (Ast.expr list)

and ('s, 'r) _menhir_cell1_case_or_default = 
  | MenhirCell1_case_or_default of 's * ('s, 'r) _menhir_state * (Ast.stmt)

and ('s, 'r) _menhir_cell1_conditional_expr = 
  | MenhirCell1_conditional_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_constant_expr = 
  | MenhirCell1_constant_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_decl_specs = 
  | MenhirCell1_decl_specs of 's * ('s, 'r) _menhir_state * (Ast.ty)

and ('s, 'r) _menhir_cell1_decl_specs_sub = 
  | MenhirCell1_decl_specs_sub of 's * ('s, 'r) _menhir_state * (Ast.ds list)

and ('s, 'r) _menhir_cell1_declarator = 
  | MenhirCell1_declarator of 's * ('s, 'r) _menhir_state * (declarator)

and ('s, 'r) _menhir_cell1_desig = 
  | MenhirCell1_desig of 's * ('s, 'r) _menhir_state * (Ast.desig)

and ('s, 'r) _menhir_cell1_direct_abstract_declarator = 
  | MenhirCell1_direct_abstract_declarator of 's * ('s, 'r) _menhir_state * (declarator)

and ('s, 'r) _menhir_cell1_direct_declarator = 
  | MenhirCell1_direct_declarator of 's * ('s, 'r) _menhir_state * (declarator)

and ('s, 'r) _menhir_cell1_enter_scope = 
  | MenhirCell1_enter_scope of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_enum_const = 
  | MenhirCell1_enum_const of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_enum_list = 
  | MenhirCell1_enum_list of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_equality_expr = 
  | MenhirCell1_equality_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_exclusive_or_expr = 
  | MenhirCell1_exclusive_or_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_expr = 
  | MenhirCell1_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_external_decl = 
  | MenhirCell1_external_decl of 's * ('s, 'r) _menhir_state * (int list)

and ('s, 'r) _menhir_cell1_ident = 
  | MenhirCell1_ident of 's * ('s, 'r) _menhir_state * (string)

and ('s, 'r) _menhir_cell1_inclusive_or_expr = 
  | MenhirCell1_inclusive_or_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_init_declarator_list = 
  | MenhirCell1_init_declarator_list of 's * ('s, 'r) _menhir_state * ((declarator * Ast.init option) list)

and ('s, 'r) _menhir_cell1_init_list = 
  | MenhirCell1_init_list of 's * ('s, 'r) _menhir_state * ((Ast.desig * Ast.init) list)

and ('s, 'r) _menhir_cell1_item = 
  | MenhirCell1_item of 's * ('s, 'r) _menhir_state * (Ast.stmt)

and ('s, 'r) _menhir_cell1_list_case_or_default_ = 
  | MenhirCell1_list_case_or_default_ of 's * ('s, 'r) _menhir_state * (Ast.stmt list)

and ('s, 'r) _menhir_cell1_list_item_ = 
  | MenhirCell1_list_item_ of 's * ('s, 'r) _menhir_state * (Ast.stmt list)

and ('s, 'r) _menhir_cell1_logical_and_expr = 
  | MenhirCell1_logical_and_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_logical_or_expr = 
  | MenhirCell1_logical_or_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_multiplicative_expr = 
  | MenhirCell1_multiplicative_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_option_expr_ = 
  | MenhirCell1_option_expr_ of 's * ('s, 'r) _menhir_state * (Ast.expr option)

and ('s, 'r) _menhir_cell1_option_ident_ = 
  | MenhirCell1_option_ident_ of 's * ('s, 'r) _menhir_state * (string option)

and ('s, 'r) _menhir_cell1_parameter_list = 
  | MenhirCell1_parameter_list of 's * ('s, 'r) _menhir_state * (Ast.decl list)

and ('s, 'r) _menhir_cell1_pointer = 
  | MenhirCell1_pointer of 's * ('s, 'r) _menhir_state * (unit)

and ('s, 'r) _menhir_cell1_postfix_expr = 
  | MenhirCell1_postfix_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_relational_expr = 
  | MenhirCell1_relational_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_shift_expr = 
  | MenhirCell1_shift_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_spec_qual_list = 
  | MenhirCell1_spec_qual_list of 's * ('s, 'r) _menhir_state * (Ast.ds list)

and ('s, 'r) _menhir_cell1_stmt = 
  | MenhirCell1_stmt of 's * ('s, 'r) _menhir_state * (Ast.stmt)

and ('s, 'r) _menhir_cell1_struct_decl = 
  | MenhirCell1_struct_decl of 's * ('s, 'r) _menhir_state * (Ast.decl list)

and ('s, 'r) _menhir_cell1_struct_declarator_list = 
  | MenhirCell1_struct_declarator_list of 's * ('s, 'r) _menhir_state * (declarator list)

and ('s, 'r) _menhir_cell1_type_name = 
  | MenhirCell1_type_name of 's * ('s, 'r) _menhir_state * (Ast.ty)

and ('s, 'r) _menhir_cell1_type_qual = 
  | MenhirCell1_type_qual of 's * ('s, 'r) _menhir_state * (Ast.ds)

and ('s, 'r) _menhir_cell1_type_spec = 
  | MenhirCell1_type_spec of 's * ('s, 'r) _menhir_state * (Ast.ds)

and ('s, 'r) _menhir_cell1_unary_expr = 
  | MenhirCell1_unary_expr of 's * ('s, 'r) _menhir_state * (Ast.expr)

and ('s, 'r) _menhir_cell1_AND = 
  | MenhirCell1_AND of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_BANG = 
  | MenhirCell1_BANG of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_CASE = 
  | MenhirCell1_CASE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA = 
  | MenhirCell1_COMMA of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEC = 
  | MenhirCell1_DEC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DEFAULT = 
  | MenhirCell1_DEFAULT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DO = 
  | MenhirCell1_DO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_DOT = 
  | MenhirCell1_DOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_ENUM = 
  | MenhirCell1_ENUM of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_EQ = 
  | MenhirCell1_EQ of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_FOR = 
  | MenhirCell1_FOR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_GOTO = 
  | MenhirCell1_GOTO of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_IF = 
  | MenhirCell1_IF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_INC = 
  | MenhirCell1_INC of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACE = 
  | MenhirCell1_LBRACE of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LBRACKET = 
  | MenhirCell1_LBRACKET of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_LPAREN = 
  | MenhirCell1_LPAREN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_MINUS = 
  | MenhirCell1_MINUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_NOT = 
  | MenhirCell1_NOT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_PLUS = 
  | MenhirCell1_PLUS of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_RETURN = 
  | MenhirCell1_RETURN of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SIZEOF = 
  | MenhirCell1_SIZEOF of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STAR = 
  | MenhirCell1_STAR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRUCT = 
  | MenhirCell1_STRUCT of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_SWITCH = 
  | MenhirCell1_SWITCH of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_UNION = 
  | MenhirCell1_UNION of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_WHILE = 
  | MenhirCell1_WHILE of 's * ('s, 'r) _menhir_state

and _menhir_box_translation_unit = 
  | MenhirBox_translation_unit of (int list) [@@unboxed]

let _menhir_action_001 =
  fun () ->
    (
# 366 "lib/parser.mly"
          ( DeclPtr(DeclIdent "") )
# 1025 "lib/parser.ml"
     : (declarator))

let _menhir_action_002 =
  fun _2 ->
    (
# 367 "lib/parser.mly"
                              ( DeclPtr _2 )
# 1033 "lib/parser.ml"
     : (declarator))

let _menhir_action_003 =
  fun _1 ->
    (
# 368 "lib/parser.mly"
                             ( _1 )
# 1041 "lib/parser.ml"
     : (declarator))

let _menhir_action_004 =
  fun _1 ->
    (
# 133 "lib/parser.mly"
                      ( _1 )
# 1049 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_005 =
  fun _1 _3 ->
    (
# 134 "lib/parser.mly"
                                         ( EBinary( Add,_1,_3) )
# 1057 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_006 =
  fun _1 _3 ->
    (
# 135 "lib/parser.mly"
                                          ( EBinary( Sub,_1,_3) )
# 1065 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_007 =
  fun _1 ->
    (
# 155 "lib/parser.mly"
                ( _1 )
# 1073 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_008 =
  fun _1 _3 ->
    (
# 156 "lib/parser.mly"
                             ( EBinary( BitAnd,_1,_3) )
# 1081 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_009 =
  fun _1 ->
    (
# 105 "lib/parser.mly"
                  ( [_1] )
# 1089 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_010 =
  fun _1 _3 ->
    (
# 106 "lib/parser.mly"
                                           ( _1@[_3] )
# 1097 "lib/parser.ml"
     : (Ast.expr list))

let _menhir_action_011 =
  fun _1 ->
    (
# 179 "lib/parser.mly"
                   ( _1 )
# 1105 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_012 =
  fun _1 _3 ->
    (
# 180 "lib/parser.mly"
                                ( EAssign( _1,_3) )
# 1113 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_013 =
  fun _1 _3 ->
    (
# 181 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( Mul,_1,_3)) )
# 1121 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_014 =
  fun _1 _3 ->
    (
# 182 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( Div,_1,_3)) )
# 1129 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_015 =
  fun _1 _3 ->
    (
# 183 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( Mod,_1,_3)) )
# 1137 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_016 =
  fun _1 _3 ->
    (
# 184 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( Add,_1,_3)) )
# 1145 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_017 =
  fun _1 _3 ->
    (
# 185 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( Sub,_1,_3)) )
# 1153 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_018 =
  fun _1 _3 ->
    (
# 186 "lib/parser.mly"
                                       ( EAssign( _1,EBinary( LShift,_1,_3)) )
# 1161 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_019 =
  fun _1 _3 ->
    (
# 187 "lib/parser.mly"
                                       ( EAssign( _1,EBinary( RShift,_1,_3)) )
# 1169 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_020 =
  fun _1 _3 ->
    (
# 188 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( BitAnd,_1,_3)) )
# 1177 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_021 =
  fun _1 _3 ->
    (
# 189 "lib/parser.mly"
                                    ( EAssign( _1,EBinary( BitXor,_1,_3)) )
# 1185 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_022 =
  fun _1 _3 ->
    (
# 190 "lib/parser.mly"
                                   ( EAssign( _1,EBinary( BitOr,_1,_3)) )
# 1193 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_023 =
  fun _2 _4 ->
    (
# 434 "lib/parser.mly"
                                         ( SCase (_2, _4) )
# 1201 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_024 =
  fun _3 ->
    (
# 435 "lib/parser.mly"
                           ( SDefault ( _3) )
# 1209 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_025 =
  fun _1 ->
    (
# 123 "lib/parser.mly"
             ( _1 )
# 1217 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_026 =
  fun _2 _4 ->
    (
# 124 "lib/parser.mly"
                                    ( ECast( _2,_4) )
# 1225 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_027 =
  fun _3 ->
    (
# 440 "lib/parser.mly"
  (
    SStmts(_3)
  )
# 1235 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_028 =
  fun _1 ->
    (
# 175 "lib/parser.mly"
                  ( _1 )
# 1243 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_029 =
  fun _1 _3 _5 ->
    (
# 176 "lib/parser.mly"
                                                       ( ECond( _1,_3,_5) )
# 1251 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_030 =
  fun _1 ->
    (
# 198 "lib/parser.mly"
  ( _1 )
# 1259 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_031 =
  fun () ->
    (
# 202 "lib/parser.mly"
                  ( [] )
# 1267 "lib/parser.ml"
     : (int list))

let _menhir_action_032 =
  fun _1 _2 ->
    (
# 204 "lib/parser.mly"
  (
    make_decls_with_init _1 _2
  )
# 1277 "lib/parser.ml"
     : (int list))

let _menhir_action_033 =
  fun _1 ->
    (
# 209 "lib/parser.mly"
                     (  [_1] )
# 1285 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_034 =
  fun _1 ->
    (
# 210 "lib/parser.mly"
            ( [_1] )
# 1293 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_035 =
  fun _1 ->
    (
# 211 "lib/parser.mly"
                ( [_1] )
# 1301 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_036 =
  fun _1 ->
    (
# 213 "lib/parser.mly"
  ( 
    [_1]
  )
# 1311 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_037 =
  fun _1 ->
    (
# 219 "lib/parser.mly"
  (
    TDeclSpec _1
  )
# 1321 "lib/parser.ml"
     : (Ast.ty))

let _menhir_action_038 =
  fun _1 ->
    (
# 224 "lib/parser.mly"
            ( _1 )
# 1329 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_039 =
  fun _1 _2 ->
    (
# 226 "lib/parser.mly"
  (  _1@ _2 )
# 1337 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_040 =
  fun _2 ->
    (
# 327 "lib/parser.mly"
                     ( DeclPtr _2 )
# 1345 "lib/parser.ml"
     : (declarator))

let _menhir_action_041 =
  fun _1 ->
    (
# 328 "lib/parser.mly"
                    ( _1 )
# 1353 "lib/parser.ml"
     : (declarator))

let _menhir_action_042 =
  fun _1 ->
    (
# 396 "lib/parser.mly"
  ( _1 )
# 1361 "lib/parser.ml"
     : (Ast.desig))

let _menhir_action_043 =
  fun _2 ->
    (
# 399 "lib/parser.mly"
                                  ( DIdx(_2,Dnone) )
# 1369 "lib/parser.ml"
     : (Ast.desig))

let _menhir_action_044 =
  fun _2 ->
    (
# 400 "lib/parser.mly"
            ( DField(_2,Dnone) )
# 1377 "lib/parser.ml"
     : (Ast.desig))

let _menhir_action_045 =
  fun _2 _4 ->
    (
# 401 "lib/parser.mly"
                                                  (DIdx(_2, _4) )
# 1385 "lib/parser.ml"
     : (Ast.desig))

let _menhir_action_046 =
  fun _2 _3 ->
    (
# 402 "lib/parser.mly"
                            ( DField(_2, _3) )
# 1393 "lib/parser.ml"
     : (Ast.desig))

let _menhir_action_047 =
  fun _2 ->
    (
# 371 "lib/parser.mly"
                                    ( _2 )
# 1401 "lib/parser.ml"
     : (declarator))

let _menhir_action_048 =
  fun _2 ->
    (
# 372 "lib/parser.mly"
                                  ( DeclArr(DeclIdent "",_2) )
# 1409 "lib/parser.ml"
     : (declarator))

let _menhir_action_049 =
  fun _2 ->
    (
# 373 "lib/parser.mly"
                                    ( DeclFun(DeclIdent "",_2) )
# 1417 "lib/parser.ml"
     : (declarator))

let _menhir_action_050 =
  fun _1 _3 ->
    (
# 374 "lib/parser.mly"
                                                             ( DeclArr(_1,_3) )
# 1425 "lib/parser.ml"
     : (declarator))

let _menhir_action_051 =
  fun _1 _3 ->
    (
# 375 "lib/parser.mly"
                                                               ( DeclFun(_1,_3) )
# 1433 "lib/parser.ml"
     : (declarator))

let _menhir_action_052 =
  fun _1 ->
    (
# 331 "lib/parser.mly"
     ( DeclIdent _1 )
# 1441 "lib/parser.ml"
     : (declarator))

let _menhir_action_053 =
  fun _2 ->
    (
# 332 "lib/parser.mly"
                           ( _2 )
# 1449 "lib/parser.ml"
     : (declarator))

let _menhir_action_054 =
  fun _1 _3 ->
    (
# 334 "lib/parser.mly"
                                                    ( DeclArr(_1, _3) )
# 1457 "lib/parser.ml"
     : (declarator))

let _menhir_action_055 =
  fun _1 _3 ->
    (
# 335 "lib/parser.mly"
                                                      ( DeclFun(_1,_3) )
# 1465 "lib/parser.ml"
     : (declarator))

let _menhir_action_056 =
  fun () ->
    (
# 405 "lib/parser.mly"
  (
    enter_scope ()
  )
# 1475 "lib/parser.ml"
     : (unit))

let _menhir_action_057 =
  fun () ->
    (
# 320 "lib/parser.mly"
    (  )
# 1483 "lib/parser.ml"
     : (unit))

let _menhir_action_058 =
  fun () ->
    (
# 320 "lib/parser.mly"
    (  )
# 1491 "lib/parser.ml"
     : (unit))

let _menhir_action_059 =
  fun () ->
    (
# 324 "lib/parser.mly"
    (  )
# 1499 "lib/parser.ml"
     : (unit))

let _menhir_action_060 =
  fun () ->
    (
# 315 "lib/parser.mly"
    ()
# 1507 "lib/parser.ml"
     : (unit))

let _menhir_action_061 =
  fun () ->
    (
# 315 "lib/parser.mly"
    ()
# 1515 "lib/parser.ml"
     : (unit))

let _menhir_action_062 =
  fun () ->
    (
# 310 "lib/parser.mly"
    (  )
# 1523 "lib/parser.ml"
     : (unit))

let _menhir_action_063 =
  fun () ->
    (
# 310 "lib/parser.mly"
    (  )
# 1531 "lib/parser.ml"
     : (unit))

let _menhir_action_064 =
  fun _1 ->
    (
# 150 "lib/parser.mly"
                  ( _1 )
# 1539 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_065 =
  fun _1 _3 ->
    (
# 151 "lib/parser.mly"
                                     ( EBinary( Eq,_1,_3) )
# 1547 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_066 =
  fun _1 _3 ->
    (
# 152 "lib/parser.mly"
                                   ( EBinary( Ne,_1,_3) )
# 1555 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_067 =
  fun _1 ->
    (
# 159 "lib/parser.mly"
           ( _1 )
# 1563 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_068 =
  fun _1 _3 ->
    (
# 160 "lib/parser.mly"
                                 ( EBinary( BitXor,_1,_3) )
# 1571 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_069 =
  fun _1 ->
    (
# 193 "lib/parser.mly"
                  ( _1 )
# 1579 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_070 =
  fun _1 _3 ->
    (
# 194 "lib/parser.mly"
                             ( EBinary( Comma,_1,_3) )
# 1587 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_071 =
  fun _1 ->
    (
# 481 "lib/parser.mly"
  ( [push_def _1] )
# 1595 "lib/parser.ml"
     : (int list))

let _menhir_action_072 =
  fun _1 ->
    (
# 483 "lib/parser.mly"
  ( _1 )
# 1603 "lib/parser.ml"
     : (int list))

let _menhir_action_073 =
  fun _1 _2 _3 ->
    (
# 487 "lib/parser.mly"
  (
    FunctionDef(make_decl _1 _2,_3)
  )
# 1613 "lib/parser.ml"
     : (Ast.item))

let _menhir_action_074 =
  fun () ->
    (
# 283 "lib/parser.mly"
         ( FsInline )
# 1621 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_075 =
  fun () ->
    (
# 284 "lib/parser.mly"
           ( FsNoreturn )
# 1629 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_076 =
  fun _1 ->
    (
# 70 "lib/parser.mly"
     ( _1 )
# 1637 "lib/parser.ml"
     : (string))

let _menhir_action_077 =
  fun _1 ->
    (
# 71 "lib/parser.mly"
          ( _1 )
# 1645 "lib/parser.ml"
     : (string))

let _menhir_action_078 =
  fun _1 ->
    (
# 163 "lib/parser.mly"
                    ( _1 )
# 1653 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_079 =
  fun _1 _3 ->
    (
# 164 "lib/parser.mly"
                                         ( EBinary( BitOr,_1,_3) )
# 1661 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_080 =
  fun _1 ->
    (
# 385 "lib/parser.mly"
                  ( IScal _1 )
# 1669 "lib/parser.ml"
     : (Ast.init))

let _menhir_action_081 =
  fun _2 ->
    (
# 386 "lib/parser.mly"
                                 ( IVect _2 )
# 1677 "lib/parser.ml"
     : (Ast.init))

let _menhir_action_082 =
  fun _1 ->
    (
# 235 "lib/parser.mly"
             ( (_1,None) )
# 1685 "lib/parser.ml"
     : (declarator * Ast.init option))

let _menhir_action_083 =
  fun _1 _3 ->
    (
# 237 "lib/parser.mly"
  ( (_1,Some _3) )
# 1693 "lib/parser.ml"
     : (declarator * Ast.init option))

let _menhir_action_084 =
  fun _1 ->
    (
# 230 "lib/parser.mly"
  ( [_1] )
# 1701 "lib/parser.ml"
     : ((declarator * Ast.init option) list))

let _menhir_action_085 =
  fun _1 _3 ->
    (
# 232 "lib/parser.mly"
  ( _1@[_3] )
# 1709 "lib/parser.ml"
     : ((declarator * Ast.init option) list))

let _menhir_action_086 =
  fun _1 ->
    (
# 389 "lib/parser.mly"
       ([(Dnone,_1)])
# 1717 "lib/parser.ml"
     : ((Ast.desig * Ast.init) list))

let _menhir_action_087 =
  fun _1 _2 ->
    (
# 390 "lib/parser.mly"
             ( [(_1,_2)] )
# 1725 "lib/parser.ml"
     : ((Ast.desig * Ast.init) list))

let _menhir_action_088 =
  fun _1 _3 ->
    (
# 391 "lib/parser.mly"
                       ( _1@[(Dnone,_3)] )
# 1733 "lib/parser.ml"
     : ((Ast.desig * Ast.init) list))

let _menhir_action_089 =
  fun _1 _3 _4 ->
    (
# 392 "lib/parser.mly"
                             ( _1@[(_3,_4)] )
# 1741 "lib/parser.ml"
     : ((Ast.desig * Ast.init) list))

let _menhir_action_090 =
  fun _1 ->
    (
# 415 "lib/parser.mly"
       ( SDef(_1) )
# 1749 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_091 =
  fun _1 ->
    (
# 416 "lib/parser.mly"
       ( _1 )
# 1757 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_092 =
  fun _3 _5 ->
    (
# 458 "lib/parser.mly"
  ( 
    SWhile(_3,_5)
  )
# 1767 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_093 =
  fun _2 _5 ->
    (
# 462 "lib/parser.mly"
  ( 
    SDoWhile(_2,_5)
  )
# 1777 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_094 =
  fun _3 _5 _7 _9 ->
    (
# 466 "lib/parser.mly"
  ( 
    SFor(_3,_5,_7,_9)
  )
# 1787 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_095 =
  fun _2 ->
    (
# 472 "lib/parser.mly"
  ( 
    SGoto _2
  )
# 1797 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_096 =
  fun () ->
    (
# 475 "lib/parser.mly"
                ( SContinue )
# 1805 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_097 =
  fun () ->
    (
# 476 "lib/parser.mly"
             ( SBreak )
# 1813 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_098 =
  fun _2 ->
    (
# 477 "lib/parser.mly"
                    ( SReturn _2 )
# 1821 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_099 =
  fun _1 _3 ->
    (
# 429 "lib/parser.mly"
  ( 
    SLabel(_1,_3)
  )
# 1831 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_100 =
  fun () ->
    (
# 410 "lib/parser.mly"
  (
    leave_scope ()
  )
# 1841 "lib/parser.ml"
     : (unit))

let _menhir_action_101 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 1849 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_102 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 1857 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_103 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 1865 "lib/parser.ml"
     : (int list list))

let _menhir_action_104 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 1873 "lib/parser.ml"
     : (int list list))

let _menhir_action_105 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 1881 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_106 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 1889 "lib/parser.ml"
     : (Ast.stmt list))

let _menhir_action_107 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 1897 "lib/parser.ml"
     : (Ast.decl list list))

let _menhir_action_108 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 1905 "lib/parser.ml"
     : (Ast.decl list list))

let _menhir_action_109 =
  fun () ->
    (
# 208 "<standard.mly>"
    ( [] )
# 1913 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_110 =
  fun x xs ->
    (
# 210 "<standard.mly>"
    ( x :: xs )
# 1921 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_111 =
  fun _1 ->
    (
# 167 "lib/parser.mly"
                    ( _1 )
# 1929 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_112 =
  fun _1 _3 ->
    (
# 168 "lib/parser.mly"
                                            ( EBinary( LogAnd,_1,_3) )
# 1937 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_113 =
  fun _1 ->
    (
# 171 "lib/parser.mly"
                   ( _1 )
# 1945 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_114 =
  fun _1 _3 ->
    (
# 172 "lib/parser.mly"
                                        ( EBinary( LogOr,_1,_3) )
# 1953 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_115 =
  fun _1 ->
    (
# 127 "lib/parser.mly"
            ( _1 )
# 1961 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_116 =
  fun _1 _3 ->
    (
# 128 "lib/parser.mly"
                                     ( EBinary( Mul,_1,_3) )
# 1969 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_117 =
  fun _1 _3 ->
    (
# 129 "lib/parser.mly"
                                    ( EBinary( Div,_1,_3) )
# 1977 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_118 =
  fun _1 _3 ->
    (
# 130 "lib/parser.mly"
                                    ( EBinary( Mod,_1,_3) )
# 1985 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_119 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 1993 "lib/parser.ml"
     : (unit option))

let _menhir_action_120 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2001 "lib/parser.ml"
     : (unit option))

let _menhir_action_121 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2009 "lib/parser.ml"
     : (unit option))

let _menhir_action_122 =
  fun () ->
    let x = 
# 344 "lib/parser.mly"
                                       ()
# 2017 "lib/parser.ml"
     in
    (
# 113 "<standard.mly>"
    ( Some x )
# 2022 "lib/parser.ml"
     : (unit option))

let _menhir_action_123 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2030 "lib/parser.ml"
     : (declarator option))

let _menhir_action_124 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2038 "lib/parser.ml"
     : (declarator option))

let _menhir_action_125 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2046 "lib/parser.ml"
     : (Ast.expr list option))

let _menhir_action_126 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2054 "lib/parser.ml"
     : (Ast.expr list option))

let _menhir_action_127 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2062 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_128 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2070 "lib/parser.ml"
     : (Ast.expr option))

let _menhir_action_129 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2078 "lib/parser.ml"
     : (string option))

let _menhir_action_130 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2086 "lib/parser.ml"
     : (string option))

let _menhir_action_131 =
  fun () ->
    (
# 111 "<standard.mly>"
    ( None )
# 2094 "lib/parser.ml"
     : (declarator list option))

let _menhir_action_132 =
  fun x ->
    (
# 113 "<standard.mly>"
    ( Some x )
# 2102 "lib/parser.ml"
     : (declarator list option))

let _menhir_action_133 =
  fun _1 _2 ->
    (
# 355 "lib/parser.mly"
  ( 
    [make_decl _1 _2]
  )
# 2112 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_134 =
  fun _1 _2 ->
    (
# 359 "lib/parser.mly"
  (
    match _2 with
    | Some d -> [make_decl _1 d]
    | None -> [make_decl _1 (DeclIdent "")]
  )
# 2124 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_135 =
  fun _1 ->
    (
# 349 "lib/parser.mly"
  ( _1 )
# 2132 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_136 =
  fun _1 _3 ->
    (
# 351 "lib/parser.mly"
  ( _1@_3 )
# 2140 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_137 =
  fun () ->
    (
# 343 "lib/parser.mly"
  ( [] )
# 2148 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_138 =
  fun _1 ->
    (
# 345 "lib/parser.mly"
  ( _1 )
# 2156 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_139 =
  fun () ->
    (
# 339 "lib/parser.mly"
  (  )
# 2164 "lib/parser.ml"
     : (unit))

let _menhir_action_140 =
  fun _1 ->
    (
# 84 "lib/parser.mly"
               ( _1 )
# 2172 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_141 =
  fun _1 _3 ->
    (
# 85 "lib/parser.mly"
                                      ( EPostfix(_1,PIdx _3) )
# 2180 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_142 =
  fun _1 _3 ->
    (
# 87 "lib/parser.mly"
  ( 
    match _3 with
    | Some l -> EPostfix( _1,PCall l)
    | None -> EPostfix( _1,PCall [])
  )
# 2192 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_143 =
  fun _1 _3 ->
    (
# 92 "lib/parser.mly"
                         ( EPostfix( _1,PDot _3) )
# 2200 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_144 =
  fun _1 _3 ->
    (
# 93 "lib/parser.mly"
                           ( EPostfix( _1,PArrow _3) )
# 2208 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_145 =
  fun _1 ->
    (
# 95 "lib/parser.mly"
  ( 
  EPostfix(_1,PInc)
  )
# 2218 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_146 =
  fun _1 ->
    (
# 99 "lib/parser.mly"
  ( 
  EPostfix(_1,PDec)
  )
# 2228 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_147 =
  fun _2 _5 ->
    (
# 102 "lib/parser.mly"
                                                         ( ECompoundLit( _2,IVect _5) )
# 2236 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_148 =
  fun _1 ->
    (
# 75 "lib/parser.mly"
     ( EVar ( lookup_var _1) )
# 2244 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_149 =
  fun _1 ->
    (
# 76 "lib/parser.mly"
       ( EConst (VInt _1) )
# 2252 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_150 =
  fun _1 ->
    (
# 77 "lib/parser.mly"
      ( EConst (VInt _1) )
# 2260 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_151 =
  fun _1 ->
    (
# 78 "lib/parser.mly"
        ( EConst (VFloat _1) )
# 2268 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_152 =
  fun _1 ->
    (
# 79 "lib/parser.mly"
      ( EConst ( VStr _1) )
# 2276 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_153 =
  fun _2 ->
    (
# 81 "lib/parser.mly"
   ( _2 )
# 2284 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_154 =
  fun _1 ->
    (
# 143 "lib/parser.mly"
             ( _1 )
# 2292 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_155 =
  fun _1 _3 ->
    (
# 144 "lib/parser.mly"
                                ( EBinary( Lt,_1,_3) )
# 2300 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_156 =
  fun _1 _3 ->
    (
# 145 "lib/parser.mly"
                                ( EBinary( Gt,_1,_3) )
# 2308 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_157 =
  fun _1 _3 ->
    (
# 146 "lib/parser.mly"
                                ( EBinary( Le,_1,_3) )
# 2316 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_158 =
  fun _1 _3 ->
    (
# 147 "lib/parser.mly"
                                ( EBinary( Ge,_1,_3) )
# 2324 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_159 =
  fun _3 _5 ->
    (
# 445 "lib/parser.mly"
                                              ( SIfElse(_3,_5,SStmts []) )
# 2332 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_160 =
  fun _3 _5 _7 ->
    (
# 446 "lib/parser.mly"
                                       ( SIfElse(_3,_5,_7) )
# 2340 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_161 =
  fun _3 _7 ->
    (
# 450 "lib/parser.mly"
  ( 
    SSwitch(_3,_7)
  )
# 2350 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_162 =
  fun _1 ->
    (
# 138 "lib/parser.mly"
                ( _1 )
# 2358 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_163 =
  fun _1 _3 ->
    (
# 139 "lib/parser.mly"
                                  ( EBinary( LShift,_1,_3) )
# 2366 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_164 =
  fun _1 _3 ->
    (
# 140 "lib/parser.mly"
                                  ( EBinary( RShift,_1,_3) )
# 2374 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_165 =
  fun _1 ->
    (
# 263 "lib/parser.mly"
  (
    _1
  )
# 2384 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_166 =
  fun _1 ->
    (
# 269 "lib/parser.mly"
  (
    _1::[]
  )
# 2394 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_167 =
  fun _1 _2 ->
    (
# 273 "lib/parser.mly"
  ( 
    _1::_2
  )
# 2404 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_168 =
  fun _1 _2 ->
    (
# 276 "lib/parser.mly"
                               (  _1::_2 )
# 2412 "lib/parser.ml"
     : (Ast.ds list))

let _menhir_action_169 =
  fun _1 ->
    (
# 419 "lib/parser.mly"
               ( _1 )
# 2420 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_170 =
  fun _1 ->
    (
# 420 "lib/parser.mly"
                ( _1 )
# 2428 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_171 =
  fun _1 ->
    (
# 421 "lib/parser.mly"
             ( SExpr _1 )
# 2436 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_172 =
  fun _1 ->
    (
# 422 "lib/parser.mly"
                   ( _1 )
# 2444 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_173 =
  fun _1 ->
    (
# 423 "lib/parser.mly"
                   ( _1 )
# 2452 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_174 =
  fun _1 ->
    (
# 424 "lib/parser.mly"
                 ( _1 )
# 2460 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_175 =
  fun _1 ->
    (
# 425 "lib/parser.mly"
            ( _1 )
# 2468 "lib/parser.ml"
     : (Ast.stmt))

let _menhir_action_176 =
  fun () ->
    (
# 240 "lib/parser.mly"
          ( ScsTypedef )
# 2476 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_177 =
  fun () ->
    (
# 241 "lib/parser.mly"
         ( ScsExtern )
# 2484 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_178 =
  fun () ->
    (
# 242 "lib/parser.mly"
         ( ScsStatic )
# 2492 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_179 =
  fun () ->
    (
# 243 "lib/parser.mly"
       ( ScsAuto )
# 2500 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_180 =
  fun () ->
    (
# 244 "lib/parser.mly"
           ( ScsRegister )
# 2508 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_181 =
  fun _1 _2 ->
    (
# 295 "lib/parser.mly"
  (
    match _2 with
    | Some dl -> make_decls (TDeclSpec _1) dl
    | None -> failwith "not impl"
  )
# 2520 "lib/parser.ml"
     : (Ast.decl list))

let _menhir_action_182 =
  fun _1 ->
    (
# 305 "lib/parser.mly"
             ( _1 )
# 2528 "lib/parser.ml"
     : (declarator))

let _menhir_action_183 =
  fun _1 ->
    (
# 301 "lib/parser.mly"
                    ( [_1] )
# 2536 "lib/parser.ml"
     : (declarator list))

let _menhir_action_184 =
  fun _1 _3 ->
    (
# 302 "lib/parser.mly"
                                                 ( _1@[_3] )
# 2544 "lib/parser.ml"
     : (declarator list))

let _menhir_action_185 =
  fun _2 _4 ->
    (
# 288 "lib/parser.mly"
                                                ( make_structdef (conv_ident _2)  (List.flatten _4) )
# 2552 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_186 =
  fun _2 ->
    (
# 289 "lib/parser.mly"
               ( make_structdecl _2 )
# 2560 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_187 =
  fun _2 _4 ->
    (
# 290 "lib/parser.mly"
                                               ( make_uniondef (conv_ident _2) (List.flatten _4) )
# 2568 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_188 =
  fun _2 ->
    (
# 291 "lib/parser.mly"
              ( make_uniondecl _2 )
# 2576 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_189 =
  fun _1 ->
    (
# 67 "lib/parser.mly"
  ( List.flatten _1 )
# 2584 "lib/parser.ml"
     : (int list))

let _menhir_action_190 =
  fun _1 ->
    (
# 380 "lib/parser.mly"
                 ( TDeclSpec _1 )
# 2592 "lib/parser.ml"
     : (Ast.ty))

let _menhir_action_191 =
  fun () ->
    (
# 382 "lib/parser.mly"
  ( TDeclSpec [] )
# 2600 "lib/parser.ml"
     : (Ast.ty))

let _menhir_action_192 =
  fun () ->
    (
# 279 "lib/parser.mly"
        ( TqConst )
# 2608 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_193 =
  fun () ->
    (
# 280 "lib/parser.mly"
           ( TqVolatile )
# 2616 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_194 =
  fun () ->
    (
# 247 "lib/parser.mly"
        ( TsVoid )
# 2624 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_195 =
  fun () ->
    (
# 248 "lib/parser.mly"
        ( TsChar )
# 2632 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_196 =
  fun () ->
    (
# 249 "lib/parser.mly"
         ( TsShort)
# 2640 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_197 =
  fun () ->
    (
# 250 "lib/parser.mly"
       ( TsInt )
# 2648 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_198 =
  fun () ->
    (
# 251 "lib/parser.mly"
        ( TsLong )
# 2656 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_199 =
  fun () ->
    (
# 252 "lib/parser.mly"
         ( TsFloat )
# 2664 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_200 =
  fun () ->
    (
# 253 "lib/parser.mly"
          ( TsDouble )
# 2672 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_201 =
  fun () ->
    (
# 254 "lib/parser.mly"
          ( TsSigned )
# 2680 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_202 =
  fun () ->
    (
# 255 "lib/parser.mly"
            ( TsUnsigned  )
# 2688 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_203 =
  fun _1 ->
    (
# 256 "lib/parser.mly"
                       ( _1
  )
# 2697 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_204 =
  fun () ->
    (
# 258 "lib/parser.mly"
            ( TsInt )
# 2705 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_205 =
  fun () ->
    (
# 259 "lib/parser.mly"
          ( TsInt )
# 2713 "lib/parser.ml"
     : (Ast.ds))

let _menhir_action_206 =
  fun _1 ->
    (
# 109 "lib/parser.mly"
               ( _1 )
# 2721 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_207 =
  fun _2 ->
    (
# 110 "lib/parser.mly"
                 ( EUnary(Plus, _2)  )
# 2729 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_208 =
  fun _2 ->
    (
# 111 "lib/parser.mly"
                 ( EUnary(Minus, _2) )
# 2737 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_209 =
  fun _2 ->
    (
# 112 "lib/parser.mly"
                 ( EUnary( Ref, _2) )
# 2745 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_210 =
  fun _2 ->
    (
# 113 "lib/parser.mly"
                 ( EUnary( Deref, _2) )
# 2753 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_211 =
  fun _2 ->
    (
# 114 "lib/parser.mly"
                 ( EUnary( Plus, _2) )
# 2761 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_212 =
  fun _2 ->
    (
# 115 "lib/parser.mly"
                  ( EUnary( Minus, _2) )
# 2769 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_213 =
  fun _2 ->
    (
# 116 "lib/parser.mly"
                ( EUnary( BitNot, _2) )
# 2777 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_214 =
  fun _2 ->
    (
# 117 "lib/parser.mly"
                 ( EUnary( LogNot, _2) )
# 2785 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_215 =
  fun _2 ->
    (
# 118 "lib/parser.mly"
                    ( EUnary( Sizeof, _2) )
# 2793 "lib/parser.ml"
     : (Ast.expr))

let _menhir_action_216 =
  fun _3 ->
    (
# 119 "lib/parser.mly"
                                 ( ETyUnary(Sizeof,_3) )
# 2801 "lib/parser.ml"
     : (Ast.expr))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD_EQ ->
        "ADD_EQ"
    | AND ->
        "AND"
    | ANDAND ->
        "ANDAND"
    | AND_EQ ->
        "AND_EQ"
    | ARROW ->
        "ARROW"
    | AUTO ->
        "AUTO"
    | BANG ->
        "BANG"
    | BREAK ->
        "BREAK"
    | CASE ->
        "CASE"
    | CHAR _ ->
        "CHAR"
    | COLON ->
        "COLON"
    | COMMA ->
        "COMMA"
    | CONST ->
        "CONST"
    | CONTINUE ->
        "CONTINUE"
    | DEC ->
        "DEC"
    | DEFAULT ->
        "DEFAULT"
    | DIV ->
        "DIV"
    | DIV_EQ ->
        "DIV_EQ"
    | DO ->
        "DO"
    | DOT ->
        "DOT"
    | ELLIPSIS ->
        "ELLIPSIS"
    | ELSE ->
        "ELSE"
    | ENUM ->
        "ENUM"
    | EOF ->
        "EOF"
    | EQ ->
        "EQ"
    | EQEQ ->
        "EQEQ"
    | EXTERN ->
        "EXTERN"
    | FLOAT _ ->
        "FLOAT"
    | FOR ->
        "FOR"
    | GE ->
        "GE"
    | GOTO ->
        "GOTO"
    | GT ->
        "GT"
    | HAT ->
        "HAT"
    | ID _ ->
        "ID"
    | IF ->
        "IF"
    | INC ->
        "INC"
    | INLINE ->
        "INLINE"
    | INT _ ->
        "INT"
    | LBRACE ->
        "LBRACE"
    | LBRACKET ->
        "LBRACKET"
    | LE ->
        "LE"
    | LPAREN ->
        "LPAREN"
    | LSHIFT ->
        "LSHIFT"
    | LSHIFT_EQ ->
        "LSHIFT_EQ"
    | LT ->
        "LT"
    | MINUS ->
        "MINUS"
    | MOD ->
        "MOD"
    | MOD_EQ ->
        "MOD_EQ"
    | MUL_EQ ->
        "MUL_EQ"
    | NE ->
        "NE"
    | NORETURN ->
        "NORETURN"
    | NOT ->
        "NOT"
    | OR ->
        "OR"
    | OROR ->
        "OROR"
    | OR_EQ ->
        "OR_EQ"
    | PLUS ->
        "PLUS"
    | QUESTION ->
        "QUESTION"
    | RBRACE ->
        "RBRACE"
    | RBRACKET ->
        "RBRACKET"
    | REGISTER ->
        "REGISTER"
    | RETURN ->
        "RETURN"
    | RPAREN ->
        "RPAREN"
    | RSHIFT ->
        "RSHIFT"
    | RSHIFT_EQ ->
        "RSHIFT_EQ"
    | SEMI ->
        "SEMI"
    | SIZEOF ->
        "SIZEOF"
    | STAR ->
        "STAR"
    | STATIC ->
        "STATIC"
    | STR _ ->
        "STR"
    | STRUCT ->
        "STRUCT"
    | SUB_EQ ->
        "SUB_EQ"
    | SWITCH ->
        "SWITCH"
    | TCHAR ->
        "TCHAR"
    | TDOUBLE ->
        "TDOUBLE"
    | TFLOAT ->
        "TFLOAT"
    | TINT ->
        "TINT"
    | TLONG ->
        "TLONG"
    | TSHORT ->
        "TSHORT"
    | TSIGNED ->
        "TSIGNED"
    | TUNSIGNED ->
        "TUNSIGNED"
    | TVOID ->
        "TVOID"
    | TYPEDEF ->
        "TYPEDEF"
    | TYPE_ID _ ->
        "TYPE_ID"
    | UNION ->
        "UNION"
    | VOLATILE ->
        "VOLATILE"
    | WHILE ->
        "WHILE"
    | XOR_EQ ->
        "XOR_EQ"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37"]
  
  let _menhir_run_279 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _v ->
      let _1 = _v in
      let _v = _menhir_action_189 _1 in
      MenhirBox_translation_unit _v
  
  let rec _menhir_run_283 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_external_decl -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _v ->
      let MenhirCell1_external_decl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_104 x xs in
      _menhir_goto_list_external_decl_ _menhir_stack _v _menhir_s
  
  and _menhir_goto_list_external_decl_ : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState282 ->
          _menhir_run_283 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_279 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_001 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_193 () in
      _menhir_goto_type_qual _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_qual : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState209 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_197 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState182 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState181 ->
          _menhir_run_182 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_197 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_034 _1 in
      _menhir_goto_decl_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_220 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState209 ->
          _menhir_run_210 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_220 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_038 _1 in
      _menhir_goto_decl_specs_sub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_decl_specs_sub : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | UNION ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TYPE_ID _ ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TYPEDEF ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TVOID ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TUNSIGNED ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TSIGNED ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TSHORT ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TLONG ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TINT ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TFLOAT ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TDOUBLE ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | TCHAR ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | STRUCT ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | STATIC ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | REGISTER ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | NORETURN ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | INLINE ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | EXTERN ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | ENUM ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | CONST ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | AUTO ->
          let _menhir_stack = MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _v) in
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState209
      | COMMA | ID _ | LBRACKET | LPAREN | RPAREN | SEMI | STAR ->
          let _1 = _v in
          let _v = _menhir_action_037 _1 in
          _menhir_goto_decl_specs _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_002 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_UNION (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState002
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState002
      | LBRACE ->
          let _v = _menhir_action_129 () in
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState002
      | _ ->
          _eRR ()
  
  and _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_ident : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_347 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState319 ->
          _menhir_run_320 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState002 ->
          _menhir_run_277 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState017 ->
          _menhir_run_274 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState020 ->
          _menhir_run_259 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState157 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState139 ->
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState022 ->
          _menhir_run_023 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_347 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TYPE_ID _v_0 ->
              _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState348
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState348
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState348
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState348
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState348
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState348
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState348
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState348
          | SEMI ->
              let _v_7 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState348 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_297 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_WHILE (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState298 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_028 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_152 _1 in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_primary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_140 _1 in
      _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_postfix_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState047 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | RPAREN ->
              let _v = _menhir_action_125 () in
              _menhir_goto_option_argument_expr_list_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
          | _ ->
              _eRR ())
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState132 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | INC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_145 _1 in
          _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DOT ->
          let _menhir_stack = MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState136 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE_ID _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | DEC ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_146 _1 in
          _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ARROW ->
          let _menhir_stack = MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState139 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE_ID _v ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ID _v ->
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | ADD_EQ | AND | ANDAND | AND_EQ | COLON | COMMA | DIV | DIV_EQ | EQ | EQEQ | GE | GT | HAT | LE | LSHIFT | LSHIFT_EQ | LT | MINUS | MOD | MOD_EQ | MUL_EQ | NE | OR | OROR | OR_EQ | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | RSHIFT_EQ | SEMI | STAR | SUB_EQ | XOR_EQ ->
          let _1 = _v in
          let _v = _menhir_action_206 _1 in
          _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STAR (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState029 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_030 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SIZEOF (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState030 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
          let _menhir_s = MenhirState249 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TYPE_ID _ ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_031 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_PLUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState031 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_032 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_NOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState032 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_033 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_MINUS (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState033 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState034 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_007 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_205 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState209 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_196 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState006 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState146 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_196 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_036 _1 in
      _menhir_goto_decl_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_145 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | UNION ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TYPE_ID _ ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TVOID ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TUNSIGNED ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TSIGNED ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TSHORT ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TLONG ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TINT ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TFLOAT ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TDOUBLE ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | TCHAR ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | STRUCT ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | ENUM ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | CONST ->
          let _menhir_stack = MenhirCell1_type_spec (_menhir_stack, _menhir_s, _v) in
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
      | ID _ | LBRACKET | LPAREN | RPAREN | SEMI | STAR ->
          let _1 = _v in
          let _v = _menhir_action_166 _1 in
          _menhir_goto_spec_qual_list_sub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_008 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_194 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_202 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_010 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_201 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_011 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_196 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_012 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_198 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_013 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_197 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_014 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_199 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_015 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_200 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_016 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_195 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_017 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STRUCT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState017
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState017
      | LBRACE ->
          let _v = _menhir_action_129 () in
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState017
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_076 _1 in
      _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_018 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_STRUCT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_option_ident_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState019
      | RBRACE ->
          let _v_1 = _menhir_action_107 () in
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_020 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_ENUM (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState020
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState020
      | LBRACE ->
          let _v = _menhir_action_129 () in
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState020
      | _ ->
          _eRR ()
  
  and _menhir_run_021 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_option_ident_ (_menhir_stack, _menhir_s, _v) in
      let _menhir_s = MenhirState022 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_144 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_192 () in
      _menhir_goto_type_qual _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_272 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_STRUCT, _menhir_box_translation_unit) _menhir_cell1_option_ident_ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_option_ident_ (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_STRUCT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_185 _2 _4 in
      _menhir_goto_struct_or_union_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_struct_or_union_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_203 _1 in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_spec_qual_list_sub : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState006 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_179 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_150 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState146 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_179 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_165 _1 in
      _menhir_goto_spec_qual_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_spec_qual_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState006 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState019 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState260 ->
          _menhir_run_261 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_180 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_261 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
      | LPAREN ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState261
      | ID _v_0 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState261
      | SEMI ->
          let _v = _menhir_action_131 () in
          _menhir_goto_option_struct_declarator_list_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_run_181 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_STAR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState181
      | COMMA | ID _ | LBRACKET | LPAREN | RPAREN | STAR ->
          let _ = _menhir_action_109 () in
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_184 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_STAR -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_STAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_139 () in
      _menhir_goto_pointer _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_pointer : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState351 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState288 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState284 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState263 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState262 ->
          _menhir_run_263 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState214 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_214 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState180 ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState199 ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_199 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_263 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState263
      | LPAREN ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState263
      | ID _v_0 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState263
      | _ ->
          _eRR ()
  
  and _menhir_run_262 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState262 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_213 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_direct_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TYPE_ID _ ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState216
          | RPAREN ->
              let _v_1 = _menhir_action_137 () in
              _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState221 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | EQ | LBRACE | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_041 _1 in
          _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_186 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_176 () in
      _menhir_goto_storage_class_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_storage_class_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_033 _1 in
      _menhir_goto_decl_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_187 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_178 () in
      _menhir_goto_storage_class_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_188 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_180 () in
      _menhir_goto_storage_class_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_189 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_075 () in
      _menhir_goto_function_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_function_spec : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_035 _1 in
      _menhir_goto_decl_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_193 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_074 () in
      _menhir_goto_function_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_194 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_177 () in
      _menhir_goto_storage_class_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_195 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _v = _menhir_action_179 () in
      _menhir_goto_storage_class_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_217 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_direct_declarator -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_055 _1 _3 in
          _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_035 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_150 _1 in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_036 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_INC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState036 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_037 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState037 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_038 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_148 _1 in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_039 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_151 _1 in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_040 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEC (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState040 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_037 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_149 _1 in
      _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_042 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_BANG (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState042 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_043 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_AND (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState043 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState284 ->
          _menhir_run_294 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState351 ->
          _menhir_run_290 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState288 ->
          _menhir_run_290 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState261 ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_267 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState211 ->
          _menhir_run_233 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState262 ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState212 ->
          _menhir_run_228 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState263 ->
          _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState214 ->
          _menhir_run_224 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_294 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_stack = MenhirCell1_declarator (_menhir_stack, _menhir_s, _v) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState294
      | LBRACE ->
          let _menhir_stack = MenhirCell1_declarator (_menhir_stack, _menhir_s, _v) in
          let _v_0 = _menhir_action_056 () in
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState294
      | COMMA | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_082 _1 in
          _menhir_goto_init_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_291 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_declarator as 'stack) -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_EQ (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState291 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACE ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_162 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState162 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACE ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DOT ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_154 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState154 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_157 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DOT (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState157 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_295 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_enter_scope (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TYPE_ID _v_0 ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState296
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | SWITCH ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | STR _v_1 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState296
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | RETURN ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | INT _v_2 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState296
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | IF ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | ID _v_3 ->
          _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState296
      | GOTO ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | FOR ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | FLOAT _v_4 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState296
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | DO ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | CONTINUE ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | CHAR _v_5 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState296
      | BREAK ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState296
      | LBRACE ->
          let _v_6 = _menhir_action_056 () in
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState296
      | RBRACE ->
          let _v_7 = _menhir_action_105 () in
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState296 _tok
      | SEMI ->
          let _v_8 = _menhir_action_127 () in
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState296 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_309 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _1 = _v in
          let _v = _menhir_action_077 _1 in
          _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | AUTO | CONST | ENUM | EXTERN | ID _ | INLINE | LPAREN | NORETURN | REGISTER | SEMI | STAR | STATIC | STRUCT | TCHAR | TDOUBLE | TFLOAT | TINT | TLONG | TSHORT | TSIGNED | TUNSIGNED | TVOID | TYPEDEF | TYPE_ID _ | UNION | VOLATILE ->
          let _v = _menhir_action_205 () in
          _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_301 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_SWITCH (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState302 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_310 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_RETURN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState310
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState310
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState310
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState310
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState310
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState310
      | SEMI ->
          let _v = _menhir_action_127 () in
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_311 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_RETURN -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_RETURN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_098 _2 in
          _menhir_goto_jump_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_jump_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_175 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState300 ->
          _menhir_run_370 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState356 ->
          _menhir_run_357 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState317 ->
          _menhir_run_355 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_354 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_349 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_335 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_370 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_WHILE, _menhir_box_translation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_WHILE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_092 _3 _5 in
      _menhir_goto_iteration_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_iteration_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_174 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_357 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_IF, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_cell1_stmt -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_stmt (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_160 _3 _5 _7 in
      _menhir_goto_selection_stmt_1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_selection_stmt_1 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_172 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_355 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_IF, _menhir_box_translation_unit) _menhir_cell1_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | TYPE_ID _v_0 ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState356
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState356
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState356
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState356
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState356
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState356
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState356
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState356
          | SEMI ->
              let _v_7 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState356 _tok
          | _ ->
              _eRR ())
      | AND | AUTO | BANG | BREAK | CASE | CHAR _ | CONST | CONTINUE | DEC | DEFAULT | DO | ENUM | EXTERN | FLOAT _ | FOR | GOTO | ID _ | IF | INC | INLINE | INT _ | LBRACE | LPAREN | MINUS | NORETURN | NOT | PLUS | RBRACE | REGISTER | RETURN | SEMI | SIZEOF | STAR | STATIC | STR _ | STRUCT | SWITCH | TCHAR | TDOUBLE | TFLOAT | TINT | TLONG | TSHORT | TSIGNED | TUNSIGNED | TVOID | TYPEDEF | TYPE_ID _ | UNION | VOLATILE | WHILE ->
          let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_IF (_menhir_stack, _menhir_s) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_159 _3 _5 in
          _menhir_goto_selection_stmt_1 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_314 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_IF (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_s = MenhirState315 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_318 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _1 = _v in
          let _v = _menhir_action_076 _1 in
          _menhir_goto_ident _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ADD_EQ | AND | ANDAND | AND_EQ | ARROW | COMMA | DEC | DIV | DIV_EQ | DOT | EQ | EQEQ | GE | GT | HAT | INC | LBRACKET | LE | LPAREN | LSHIFT | LSHIFT_EQ | LT | MINUS | MOD | MOD_EQ | MUL_EQ | NE | OR | OROR | OR_EQ | PLUS | QUESTION | RSHIFT | RSHIFT_EQ | SEMI | STAR | SUB_EQ | XOR_EQ ->
          let _1 = _v in
          let _v = _menhir_action_148 _1 in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_319 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_GOTO (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState319 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | ID _v ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_322 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_FOR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState323
          | SEMI ->
              let _v = _menhir_action_127 () in
              _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState323 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_324 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_FOR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v_0 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState325
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | INT _v_1 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState325
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | ID _v_2 ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState325
          | FLOAT _v_3 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState325
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | CHAR _v_4 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState325
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState325
          | SEMI ->
              let _v_5 = _menhir_action_127 () in
              _menhir_run_326 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState325 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_326 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v_0 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState327
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | INT _v_1 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState327
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | ID _v_2 ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState327
          | FLOAT _v_3 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState327
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | CHAR _v_4 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState327
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState327
          | RPAREN ->
              let _v_5 = _menhir_action_127 () in
              _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState327 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_328 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_option_expr_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | TYPE_ID _v_0 ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState329
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState329
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState329
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState329
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState329
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState329
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState329
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState329
          | SEMI ->
              let _v_7 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState329 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_330 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DO (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | TYPE_ID _v ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | SWITCH ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | RETURN ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | IF ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | ID _v ->
          _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | GOTO ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | FOR ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | DO ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | CONTINUE ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | BREAK ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState330
      | LBRACE ->
          let _v = _menhir_action_056 () in
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330
      | SEMI ->
          let _v = _menhir_action_127 () in
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState330 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_331 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_096 () in
          _menhir_goto_jump_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_333 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_097 () in
          _menhir_goto_jump_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_342 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_171 _1 in
          _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_354 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_FOR, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_, _menhir_box_translation_unit) _menhir_cell1_option_expr_ -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_option_expr_ (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_option_expr_ (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_option_expr_ (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_FOR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _9 = _v in
      let _v = _menhir_action_094 _3 _5 _7 _9 in
      _menhir_goto_iteration_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_349 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_goto_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_item : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_359 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_350 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_359 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_item (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TYPE_ID _v_0 ->
          _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState359
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | SWITCH ->
          _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | STR _v_1 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState359
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | RETURN ->
          _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | INT _v_2 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState359
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | IF ->
          _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | ID _v_3 ->
          _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState359
      | GOTO ->
          _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | FOR ->
          _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | FLOAT _v_4 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState359
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | DO ->
          _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | CONTINUE ->
          _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | CHAR _v_5 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState359
      | BREAK ->
          _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState359
      | LBRACE ->
          let _v_6 = _menhir_action_056 () in
          _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState359
      | CASE | DEFAULT | RBRACE ->
          let _v_7 = _menhir_action_105 () in
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 _tok
      | SEMI ->
          let _v_8 = _menhir_action_127 () in
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState359 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_360 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_item -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_item (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_106 x xs in
      _menhir_goto_list_item_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_item_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_371 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState359 ->
          _menhir_run_360 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState308 ->
          _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_371 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_enter_scope as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_list_item_ (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RBRACE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _ = _menhir_action_100 () in
          let MenhirCell1_list_item_ (_menhir_stack, _, _3) = _menhir_stack in
          let MenhirCell1_enter_scope (_menhir_stack, _menhir_s, _) = _menhir_stack in
          let _v = _menhir_action_027 _3 in
          _menhir_goto_compound_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_compound_stmt : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState294 ->
          _menhir_run_374 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_353 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_374 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_cell1_declarator -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_declarator (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_073 _1 _2 _3 in
      let _1 = _v in
      let _v = _menhir_action_071 _1 in
      _menhir_goto_external_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_external_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_external_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState282
      | EOF ->
          let _v_1 = _menhir_action_103 () in
          _menhir_run_283 _menhir_stack _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_353 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_170 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_364 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_CASE, _menhir_box_translation_unit) _menhir_cell1_conditional_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_conditional_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_CASE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_023 _2 _4 in
      _menhir_goto_case_or_default _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_case_or_default : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_case_or_default (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | DEFAULT ->
          _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState368
      | CASE ->
          _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState368
      | RBRACE ->
          let _v_0 = _menhir_action_101 () in
          _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_307 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TYPE_ID _v ->
              _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | ID _v ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState308
          | LBRACE ->
              let _v = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308
          | CASE | DEFAULT | RBRACE ->
              let _v = _menhir_action_105 () in
              _menhir_run_358 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | SEMI ->
              let _v = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState308 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_358 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DEFAULT -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_DEFAULT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_024 _3 in
      _menhir_goto_case_or_default _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_361 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_CASE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState361 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_369 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_case_or_default -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_case_or_default (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_102 x xs in
      _menhir_goto_list_case_or_default_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_case_or_default_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState368 ->
          _menhir_run_369 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState306 ->
          _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_365 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_SWITCH, _menhir_box_translation_unit) _menhir_cell1_expr, _menhir_box_translation_unit) _menhir_cell1_enter_scope as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_list_case_or_default_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _ = _menhir_action_100 () in
      let MenhirCell1_list_case_or_default_ (_menhir_stack, _, _7) = _menhir_stack in
      let MenhirCell1_enter_scope (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_SWITCH (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_161 _3 _7 in
      let _1 = _v in
      let _v = _menhir_action_173 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_350 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ident -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_ident (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_099 _1 _3 in
      let _1 = _v in
      let _v = _menhir_action_169 _1 in
      _menhir_goto_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_335 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DO as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_stmt (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LPAREN ->
              let _menhir_s = MenhirState337 in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | STR _v ->
                  _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | STAR ->
                  _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | SIZEOF ->
                  _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | PLUS ->
                  _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | NOT ->
                  _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | MINUS ->
                  _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | LPAREN ->
                  _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | INT _v ->
                  _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | INC ->
                  _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | ID _v ->
                  _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | FLOAT _v ->
                  _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | DEC ->
                  _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | CHAR _v ->
                  _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
              | BANG ->
                  _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | AND ->
                  _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_init_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState351 ->
          _menhir_run_293 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState284 ->
          _menhir_run_293 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState288 ->
          _menhir_run_289 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_293 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_084 _1 in
      _menhir_goto_init_declarator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_init_declarator_list : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_032 _1 _2 in
          _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_init_declarator_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState288 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_375 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_375 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_352 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_375 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_072 _1 in
      _menhir_goto_external_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_352 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_goto_item _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_289 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs, _menhir_box_translation_unit) _menhir_cell1_init_declarator_list -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_init_declarator_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_085 _1 _3 in
      _menhir_goto_init_declarator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_290 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_stack = MenhirCell1_declarator (_menhir_stack, _menhir_s, _v) in
          _menhir_run_291 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState290
      | COMMA | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_082 _1 in
          _menhir_goto_init_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_267 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_182 _1 in
      _menhir_goto_struct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_struct_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState261 ->
          _menhir_run_268 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState265 ->
          _menhir_run_266 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_268 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_183 _1 in
      _menhir_goto_struct_declarator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_struct_declarator_list : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_struct_declarator_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState265 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STAR ->
              _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | SEMI ->
          let x = _v in
          let _v = _menhir_action_132 x in
          _menhir_goto_option_struct_declarator_list_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_struct_declarator_list_ : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_181 _1 _2 in
      let _menhir_stack = MenhirCell1_struct_decl (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState260
      | RBRACE ->
          let _v_1 = _menhir_action_107 () in
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_271 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_struct_decl -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_struct_decl (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_108 x xs in
      _menhir_goto_list_struct_decl_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_goto_list_struct_decl_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState006 ->
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState019 ->
          _menhir_run_272 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState260 ->
          _menhir_run_271 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_275 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_UNION, _menhir_box_translation_unit) _menhir_cell1_option_ident_ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_option_ident_ (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_UNION (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_187 _2 _4 in
      _menhir_goto_struct_or_union_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_266 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list, _menhir_box_translation_unit) _menhir_cell1_struct_declarator_list -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_struct_declarator_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_184 _1 _3 in
      _menhir_goto_struct_declarator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_233 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_133 _1 _2 in
      _menhir_goto_parameter_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_decl : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState185 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_219 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_207 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_219 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_135 _1 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TYPE_ID _ ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | ELLIPSIS ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _ = _menhir_action_122 () in
              _menhir_goto_option___anonymous_0_ _menhir_stack _menhir_lexbuf _menhir_lexer _tok
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState205
          | _ ->
              _eRR ())
      | RPAREN ->
          let _ = _menhir_action_121 () in
          _menhir_goto_option___anonymous_0_ _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_option___anonymous_0_ : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_parameter_list -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _v = _menhir_action_138 _1 in
      _menhir_goto_parameter_type_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_parameter_type_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState185 ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState212 ->
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState216 ->
          _menhir_run_217 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState201 ->
          _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_226 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_049 _2 in
          _menhir_goto_direct_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_direct_abstract_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LPAREN ->
          let _menhir_stack = MenhirCell1_direct_abstract_declarator (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TYPE_ID _ ->
              _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState201
          | RPAREN ->
              let _v_1 = _menhir_action_137 () in
              _menhir_run_202 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 _tok
          | _ ->
              _eRR ())
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_direct_abstract_declarator (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState236 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_003 _1 in
          _menhir_goto_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_202 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_direct_abstract_declarator -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_direct_abstract_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_051 _1 _3 in
          _menhir_goto_direct_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_abstract_declarator : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState180 ->
          _menhir_run_239 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState211 ->
          _menhir_run_234 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState185 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState212 ->
          _menhir_run_230 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState199 ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState214 ->
          _menhir_run_225 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_239 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_spec_qual_list -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_191 () in
      _menhir_goto_type_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_name : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState249 ->
          _menhir_run_250 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_243 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_250 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_SIZEOF, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _menhir_stack = MenhirCell1_type_name (_menhir_stack, _menhir_s, _v) in
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState251
          | ADD_EQ | AND | ANDAND | AND_EQ | COLON | COMMA | DIV | DIV_EQ | EQ | EQEQ | GE | GT | HAT | LE | LSHIFT | LSHIFT_EQ | LT | MINUS | MOD | MOD_EQ | MUL_EQ | NE | OR | OROR | OR_EQ | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | RSHIFT_EQ | SEMI | STAR | SUB_EQ | XOR_EQ ->
              let MenhirCell1_LPAREN (_menhir_stack, _) = _menhir_stack in
              let MenhirCell1_SIZEOF (_menhir_stack, _menhir_s) = _menhir_stack in
              let _3 = _v in
              let _v = _menhir_action_216 _3 in
              _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_153 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name as 'stack) -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACE (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState153 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACKET ->
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LBRACE ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DOT ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_unary_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState030 ->
          _menhir_run_252 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState036 ->
          _menhir_run_242 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState040 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState029 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState031 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState032 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState033 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState244 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState042 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState055 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState053 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState043 ->
          _menhir_run_044 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_252 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_SIZEOF -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_SIZEOF (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_215 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_242 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_INC -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_INC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_207 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_143 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DEC -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_DEC (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_208 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | XOR_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState049 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | SUB_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState106 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RSHIFT_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState108 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | OR_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState110 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | MUL_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState112 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | MOD_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState114 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | LSHIFT_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState116 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState118 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | DIV_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState120 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState122 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | ADD_EQ ->
          let _menhir_stack = MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState124 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | AND | ANDAND | COLON | COMMA | DIV | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | MINUS | MOD | NE | OR | OROR | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI | STAR ->
          let _1 = _v in
          let _v = _menhir_action_025 _1 in
          _menhir_goto_cast_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_cast_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState029 ->
          _menhir_run_253 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState031 ->
          _menhir_run_248 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState032 ->
          _menhir_run_247 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState033 ->
          _menhir_run_246 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState244 ->
          _menhir_run_245 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState042 ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState043 ->
          _menhir_run_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState063 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_058 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState055 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState053 ->
          _menhir_run_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_253 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_STAR -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STAR (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_210 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_248 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_PLUS -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_PLUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_211 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_247 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_NOT -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_NOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_213 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_246 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_MINUS -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_MINUS (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_212 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_245 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_name (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_026 _2 _4 in
      _menhir_goto_cast_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_142 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_BANG -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_BANG (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_214 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_141 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_AND -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_AND (_menhir_stack, _menhir_s) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_209 _2 in
      _menhir_goto_unary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_115 _1 in
      _menhir_goto_multiplicative_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_multiplicative_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState063 ->
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState061 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_064 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_additive_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | MINUS | NE | OR | OROR | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_006 _1 _3 in
          _menhir_goto_additive_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState053 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_055 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState055 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_057 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState057 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_additive_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState075 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState065 ->
          _menhir_run_066 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_070 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_162 _1 in
          _menhir_goto_shift_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_additive_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState061 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_additive_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState063 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_shift_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState075 ->
          _menhir_run_076 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState073 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState068 ->
          _menhir_run_069 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_050 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_076 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_158 _1 _3 in
          _menhir_goto_relational_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_051 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_shift_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState051 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_shift_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState065 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_relational_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState086 ->
          _menhir_run_087 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState091 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_067 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_089 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_equality_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | HAT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_065 _1 _3 in
          _menhir_goto_equality_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_068 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState068 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState071 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState073 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_075 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState075 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_equality_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState091 ->
          _menhir_run_092 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_092 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_and_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NE ->
          let _menhir_stack = MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQEQ ->
          let _menhir_stack = MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | HAT | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_and_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_008 _1 _3 in
          _menhir_goto_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_equality_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState086 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_088 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_equality_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState088 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_and_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_093 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState084 ->
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_093 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | HAT | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_067 _1 in
          _menhir_goto_exclusive_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_091 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_and_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState091 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_exclusive_or_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState082 ->
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_094 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | HAT ->
          let _menhir_stack = MenhirCell1_exclusive_or_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_078 _1 in
          _menhir_goto_inclusive_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_exclusive_or_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState084 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_inclusive_or_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState103 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState080 ->
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_095 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_inclusive_or_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_goto_logical_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_inclusive_or_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState082 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_logical_and_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState103 ->
          _menhir_run_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState361 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_104 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ANDAND ->
          let _menhir_stack = MenhirCell1_logical_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | COMMA | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_logical_or_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_114 _1 _3 in
          _menhir_goto_logical_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_080 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_logical_and_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState080 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_logical_or_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUESTION ->
          let _menhir_stack = MenhirCell1_logical_or_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState078 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | OROR ->
          let _menhir_stack = MenhirCell1_logical_or_expr (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState103 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COLON | COMMA | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_028 _1 in
          _menhir_goto_conditional_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_goto_conditional_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState361 ->
          _menhir_run_362 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState027 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState236 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState221 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState190 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState154 ->
          _menhir_run_161 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState100 ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState291 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState047 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState122 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState120 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState118 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState116 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState114 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState112 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState110 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState108 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState106 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState049 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_098 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_362 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_CASE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_conditional_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COLON ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | VOLATILE ->
              _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | UNION ->
              _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TYPE_ID _v_0 ->
              _menhir_run_309 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState363
          | TYPEDEF ->
              _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TVOID ->
              _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TUNSIGNED ->
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TSIGNED ->
              _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TSHORT ->
              _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TLONG ->
              _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TINT ->
              _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TFLOAT ->
              _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TDOUBLE ->
              _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | TCHAR ->
              _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | STRUCT ->
              _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState363
          | STATIC ->
              _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | REGISTER ->
              _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | NORETURN ->
              _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState363
          | INLINE ->
              _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState363
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState363
          | EXTERN ->
              _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | ENUM ->
              _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | CONST ->
              _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState363
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | AUTO ->
              _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState363
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState363
          | CASE | DEFAULT | RBRACE ->
              let _v_7 = _menhir_action_105 () in
              _menhir_run_364 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 _tok
          | SEMI ->
              let _v_8 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_8 MenhirState363 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_161 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_030 _1 in
      _menhir_goto_constant_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_constant_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState027 ->
          _menhir_run_254 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState236 ->
          _menhir_run_237 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState221 ->
          _menhir_run_222 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState190 ->
          _menhir_run_191 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_254 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_enum_const -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_enum_const (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _ = _menhir_action_058 () in
      _menhir_goto_enum _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
  
  and _menhir_goto_enum : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      match _menhir_s with
      | MenhirState022 ->
          _menhir_run_258 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_255 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_258 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_ as 'stack) -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_060 () in
      _menhir_goto_enum_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_enum_list : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_ as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_enum_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TYPE_ID _v_0 ->
              let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, MenhirState024) in
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState025
          | ID _v_1 ->
              let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, MenhirState024) in
              _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState025
          | RBRACE ->
              let _ =
                let x = () in
                _menhir_action_120 x
              in
              _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
          | _ ->
              _eRR ())
      | RBRACE ->
          let _ = _menhir_action_119 () in
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_256 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_cell1_enum_list -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_enum_list (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_option_ident_ (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_ENUM (_menhir_stack, _menhir_s) = _menhir_stack in
      let _ = _menhir_action_062 () in
      _menhir_goto_enum_spec _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
  
  and _menhir_goto_enum_spec : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_204 () in
      _menhir_goto_type_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_255 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM, _menhir_box_translation_unit) _menhir_cell1_option_ident_, _menhir_box_translation_unit) _menhir_cell1_enum_list, _menhir_box_translation_unit) _menhir_cell1_COMMA -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _tok ->
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_enum_list (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_061 () in
      _menhir_goto_enum_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_237 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_direct_abstract_declarator -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_direct_abstract_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_050 _1 _3 in
          _menhir_goto_direct_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_222 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_direct_declarator -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_direct_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_054 _1 _3 in
          _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_191 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACKET -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_048 _2 in
          _menhir_goto_direct_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_155 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACKET as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACKET ->
              let _menhir_stack = MenhirCell1_constant_expr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
          | DOT ->
              let _menhir_stack = MenhirCell1_constant_expr (_menhir_stack, _menhir_s, _v) in
              _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState156
          | EQ ->
              let MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) = _menhir_stack in
              let _2 = _v in
              let _v = _menhir_action_043 _2 in
              _menhir_goto_designator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_goto_designator_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState153 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState162 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState164 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState156 ->
          _menhir_run_160 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_166 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let _1 = _v in
      let _v = _menhir_action_042 _1 in
      _menhir_goto_desig _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_desig : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState153 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_174 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_168 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_174 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_desig (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STR _v_0 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState174
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | LBRACE ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | INT _v_1 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState174
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | ID _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState174
      | FLOAT _v_3 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState174
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | CHAR _v_4 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState174
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState174
      | _ ->
          _eRR ()
  
  and _menhir_run_168 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_cell1_COMMA as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_desig (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STR _v_0 ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState168
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | LBRACE ->
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | INT _v_1 ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState168
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | ID _v_2 ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState168
      | FLOAT _v_3 ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState168
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | CHAR _v_4 ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState168
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState168
      | _ ->
          _eRR ()
  
  and _menhir_run_160 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACKET, _menhir_box_translation_unit) _menhir_cell1_constant_expr -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_constant_expr (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_045 _2 _4 in
      _menhir_goto_designator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_159 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DOT, _menhir_box_translation_unit) _menhir_cell1_ident -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let MenhirCell1_ident (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_DOT (_menhir_stack, _menhir_s) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_046 _2 _3 in
      _menhir_goto_designator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr, _menhir_box_translation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_logical_or_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_029 _1 _3 _5 in
      _menhir_goto_conditional_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_098 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_011 _1 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_assignment_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState291 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState153 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState174 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState130 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState047 ->
          _menhir_run_128 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState124 ->
          _menhir_run_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState122 ->
          _menhir_run_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState120 ->
          _menhir_run_121 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState118 ->
          _menhir_run_119 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState116 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState114 ->
          _menhir_run_115 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState112 ->
          _menhir_run_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState110 ->
          _menhir_run_111 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState108 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState106 ->
          _menhir_run_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState049 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState296 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState337 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_102 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState097 ->
          _menhir_run_099 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_170 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_080 _1 in
      _menhir_goto_init _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_init : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState291 ->
          _menhir_run_292 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState174 ->
          _menhir_run_175 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState153 ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_173 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState168 ->
          _menhir_run_169 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_165 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_292 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_declarator, _menhir_box_translation_unit) _menhir_cell1_EQ -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_EQ (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_083 _1 _3 in
      _menhir_goto_init_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_175 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_desig -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_desig (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_087 _1 _2 in
      _menhir_goto_init_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_init_list : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState153 ->
          _menhir_run_176 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState162 ->
          _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_176 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_init_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState176
      | RBRACE ->
          let _ = _menhir_action_119 () in
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_164 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list as 'stack) -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164
      | STAR ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | SIZEOF ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | PLUS ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | NOT ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | MINUS ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LPAREN ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | LBRACE ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_162 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | INT _v ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164
      | INC ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | ID _v ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164
      | FLOAT _v ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164
      | DOT ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | DEC ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | CHAR _v ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164
      | BANG ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | AND ->
          let _menhir_stack = MenhirCell1_COMMA (_menhir_stack, _menhir_s) in
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | RBRACE ->
          let x = () in
          let _ = _menhir_action_120 x in
          _menhir_goto_option_COMMA_ _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_COMMA_ : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      match _menhir_s with
      | MenhirState024 ->
          _menhir_run_256 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MenhirState176 ->
          _menhir_run_177 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MenhirState163 ->
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_177 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN, _menhir_box_translation_unit) _menhir_cell1_type_name, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_init_list (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_LBRACE (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_type_name (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_147 _2 _5 in
      _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_171 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_init_list (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_LBRACE (_menhir_stack, _menhir_s) = _menhir_stack in
      let _v = _menhir_action_081 _2 in
      _menhir_goto_init _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_163 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_init_list (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | RBRACE ->
          let _ = _menhir_action_119 () in
          _menhir_run_171 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_173 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_086 _1 in
      _menhir_goto_init_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_169 : type  ttv_stack. ((((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_cell1_COMMA, _menhir_box_translation_unit) _menhir_cell1_desig -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_desig (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_init_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _4 = _v in
      let _v = _menhir_action_089 _1 _3 _4 in
      _menhir_goto_init_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_165 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LBRACE, _menhir_box_translation_unit) _menhir_cell1_init_list, _menhir_box_translation_unit) _menhir_cell1_COMMA -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_init_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_088 _1 _3 in
      _menhir_goto_init_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr, _menhir_box_translation_unit) _menhir_cell1_argument_expr_list -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_argument_expr_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_010 _1 _3 in
      _menhir_goto_argument_expr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_argument_expr_list : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_argument_expr_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState130 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | RPAREN ->
          let x = _v in
          let _v = _menhir_action_126 x in
          _menhir_goto_option_argument_expr_list_ _menhir_stack _menhir_lexbuf _menhir_lexer _v
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_argument_expr_list_ : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_142 _1 _3 in
      _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_128 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_goto_argument_expr_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_125 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_016 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_123 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_020 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_121 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_014 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_119 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_012 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_117 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_018 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_115 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_015 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_113 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_013 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_111 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_022 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_019 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_107 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_017 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_105 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_unary_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_021 _1 _3 in
      _menhir_goto_assignment_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_102 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_069 _1 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expr : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState337 ->
          _menhir_run_338 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState315 ->
          _menhir_run_316 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState296 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_313 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState302 ->
          _menhir_run_303 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState298 ->
          _menhir_run_299 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState249 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState037 ->
          _menhir_run_240 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState132 ->
          _menhir_run_133 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState078 ->
          _menhir_run_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_338 : type  ttv_stack. (((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DO, _menhir_box_translation_unit) _menhir_cell1_stmt as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_stmt (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_DO (_menhir_stack, _menhir_s) = _menhir_stack in
          let _5 = _v in
          let _v = _menhir_action_093 _2 _5 in
          _menhir_goto_iteration_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_097 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_expr -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _menhir_s = MenhirState097 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_316 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_IF as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | TYPE_ID _v_0 ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState317
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState317
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState317
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState317
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState317
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState317
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState317
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState317
          | SEMI ->
              let _v_7 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState317 _tok
          | _ ->
              _eRR ())
      | COMMA ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_313 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | RPAREN | SEMI ->
          let x = _v in
          let _v = _menhir_action_128 x in
          _menhir_goto_option_expr_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_expr_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState300 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState317 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState356 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState329 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState330 ->
          _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState327 ->
          _menhir_run_328 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState325 ->
          _menhir_run_326 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState323 ->
          _menhir_run_324 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState310 ->
          _menhir_run_311 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_303 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_SWITCH as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_056 () in
          let (_v, _menhir_s) = (_v_0, MenhirState304) in
          let _menhir_stack = MenhirCell1_enter_scope (_menhir_stack, _menhir_s, _v) in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | DEFAULT ->
                  _menhir_run_307 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
              | CASE ->
                  _menhir_run_361 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState306
              | RBRACE ->
                  let _v_0 = _menhir_action_101 () in
                  _menhir_run_365 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState306
              | _ ->
                  _eRR ())
          | _ ->
              _eRR ())
      | COMMA ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_299 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_WHILE as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              _menhir_run_297 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | TYPE_ID _v_0 ->
              _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState300
          | SWITCH ->
              _menhir_run_301 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | STR _v_1 ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1 MenhirState300
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | RETURN ->
              _menhir_run_310 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | INT _v_2 ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v_2 MenhirState300
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | IF ->
              _menhir_run_314 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | ID _v_3 ->
              _menhir_run_318 _menhir_stack _menhir_lexbuf _menhir_lexer _v_3 MenhirState300
          | GOTO ->
              _menhir_run_319 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | FOR ->
              _menhir_run_322 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | FLOAT _v_4 ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState300
          | DO ->
              _menhir_run_330 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | CONTINUE ->
              _menhir_run_331 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | CHAR _v_5 ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState300
          | BREAK ->
              _menhir_run_333 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState300
          | LBRACE ->
              let _v_6 = _menhir_action_056 () in
              _menhir_run_295 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState300
          | SEMI ->
              let _v_7 = _menhir_action_127 () in
              _menhir_run_342 _menhir_stack _menhir_lexbuf _menhir_lexer _v_7 MenhirState300 _tok
          | _ ->
              _eRR ())
      | COMMA ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_240 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_153 _2 in
          _menhir_goto_primary_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_133 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RBRACKET ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_141 _1 _3 in
          _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA ->
          let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | _ ->
          _eRR ()
  
  and _menhir_run_096 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_logical_or_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expr (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON ->
          let _menhir_s = MenhirState100 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_099 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_070 _1 _3 in
      _menhir_goto_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_079 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ANDAND ->
          let _menhir_stack = MenhirCell1_logical_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer
      | COLON | COMMA | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_113 _1 in
          _menhir_goto_logical_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_081 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_logical_and_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OR ->
          let _menhir_stack = MenhirCell1_inclusive_or_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_logical_and_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_112 _1 _3 in
          _menhir_goto_logical_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_083 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_inclusive_or_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | HAT ->
          let _menhir_stack = MenhirCell1_exclusive_or_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_inclusive_or_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_079 _1 _3 in
          _menhir_goto_inclusive_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_090 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_exclusive_or_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND ->
          let _menhir_stack = MenhirCell1_and_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ANDAND | COLON | COMMA | HAT | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_exclusive_or_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_068 _1 _3 in
          _menhir_goto_exclusive_or_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_085 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NE ->
          let _menhir_stack = MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer
      | EQEQ ->
          let _menhir_stack = MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_088 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | HAT | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_007 _1 in
          _menhir_goto_and_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_087 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_equality_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | HAT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_equality_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_066 _1 _3 in
          _menhir_goto_equality_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_067 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_068 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GT ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE ->
          let _menhir_stack = MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_075 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | HAT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_064 _1 in
          _menhir_goto_equality_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_074 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_156 _1 _3 in
          _menhir_goto_relational_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_072 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_157 _1 _3 in
          _menhir_goto_relational_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_069 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_relational_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let MenhirCell1_relational_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_155 _1 _3 in
          _menhir_goto_relational_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_050 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | RSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LSHIFT ->
          let _menhir_stack = MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_154 _1 in
          _menhir_goto_relational_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_066 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_shift_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_163 _1 _3 in
          _menhir_goto_shift_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_060 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_shift_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | PLUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MINUS ->
          let _menhir_stack = MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | NE | OR | OROR | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let MenhirCell1_shift_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_164 _1 _3 in
          _menhir_goto_shift_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_062 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_additive_expr as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | MINUS | NE | OR | OROR | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let MenhirCell1_additive_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_005 _1 _3 in
          _menhir_goto_additive_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_052 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_055 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV ->
          let _menhir_stack = MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _v) in
          _menhir_run_057 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND | ANDAND | COLON | COMMA | EQEQ | GE | GT | HAT | LE | LSHIFT | LT | MINUS | NE | OR | OROR | PLUS | QUESTION | RBRACE | RBRACKET | RPAREN | RSHIFT | SEMI ->
          let _1 = _v in
          let _v = _menhir_action_004 _1 in
          _menhir_goto_additive_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_058 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_117 _1 _3 in
      _menhir_goto_multiplicative_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_118 _1 _3 in
      _menhir_goto_multiplicative_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_054 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_multiplicative_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_116 _1 _3 in
      _menhir_goto_multiplicative_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_044 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_025 _1 in
      _menhir_goto_cast_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_243 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_name (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_s = MenhirState244 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LBRACE ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_151 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_name (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _menhir_s = MenhirState152 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | LBRACE ->
              _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_234 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let x = _v in
      let _v = _menhir_action_124 x in
      _menhir_goto_option_abstract_declarator_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_goto_option_abstract_declarator_ : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_134 _1 _2 in
      _menhir_goto_parameter_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_230 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_047 _2 in
          _menhir_goto_direct_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_225 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_pointer -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_pointer (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_002 _2 in
      _menhir_goto_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_207 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_parameter_list -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_136 _1 _3 in
      _menhir_goto_parameter_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_228 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | RPAREN ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_LPAREN (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_053 _2 in
          _menhir_goto_direct_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_224 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_pointer -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_pointer (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_040 _2 in
      _menhir_goto_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_214 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState214
      | LPAREN ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState214
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState214
      | ID _v_0 ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState214
      | COMMA | RPAREN ->
          let _v = _menhir_action_001 () in
          _menhir_goto_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_212 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | LPAREN ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | LBRACKET ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | ID _v ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState212
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState212
      | RPAREN ->
          let _v = _menhir_action_137 () in
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_190 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LBRACKET (_menhir_stack, _menhir_s) in
      let _menhir_s = MenhirState190 in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STR _v ->
          _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | STAR ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | SIZEOF ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | PLUS ->
          _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | NOT ->
          _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | MINUS ->
          _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | LPAREN ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | INT _v ->
          _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | INC ->
          _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | ID _v ->
          _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | FLOAT _v ->
          _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | DEC ->
          _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | CHAR _v ->
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | BANG ->
          _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | AND ->
          _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_199 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
      | LPAREN ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_pointer (_menhir_stack, _menhir_s, _v) in
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState199
      | RPAREN ->
          let _v = _menhir_action_001 () in
          _menhir_goto_abstract_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_185 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_LPAREN (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | LPAREN ->
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | LBRACKET ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState185
      | RPAREN ->
          let _v = _menhir_action_137 () in
          _menhir_run_226 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_180 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_LPAREN as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR ->
          let _menhir_stack = MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | LPAREN ->
          let _menhir_stack = MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_185 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_spec_qual_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState180
      | RPAREN ->
          let _1 = _v in
          let _v = _menhir_action_190 _1 in
          _menhir_goto_type_name _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_150 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_type_spec -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_spec (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_167 _1 _2 in
      _menhir_goto_spec_qual_list_sub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_148 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_type_qual -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_qual (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_168 _1 _2 in
      _menhir_goto_spec_qual_list_sub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_320 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_GOTO -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_GOTO (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_095 _2 in
          _menhir_goto_jump_stmt _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_277 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_UNION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let x = _v in
          let _v = _menhir_action_130 x in
          _menhir_goto_option_ident_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AUTO | COMMA | CONST | ENUM | EXTERN | ID _ | INLINE | LBRACKET | LPAREN | NORETURN | REGISTER | RPAREN | SEMI | STAR | STATIC | STRUCT | TCHAR | TDOUBLE | TFLOAT | TINT | TLONG | TSHORT | TSIGNED | TUNSIGNED | TVOID | TYPEDEF | TYPE_ID _ | UNION | VOLATILE ->
          let MenhirCell1_UNION (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_188 _2 in
          _menhir_goto_struct_or_union_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_option_ident_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      match _menhir_s with
      | MenhirState020 ->
          _menhir_run_021 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState017 ->
          _menhir_run_018 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | MenhirState002 ->
          _menhir_run_005 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_005 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_UNION as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _menhir_stack = MenhirCell1_option_ident_ (_menhir_stack, _menhir_s, _v) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState006
      | RBRACE ->
          let _v_1 = _menhir_action_107 () in
          _menhir_run_275 _menhir_stack _menhir_lexbuf _menhir_lexer _v_1
      | _ ->
          _eRR ()
  
  and _menhir_run_274 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_STRUCT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LBRACE ->
          let x = _v in
          let _v = _menhir_action_130 x in
          _menhir_goto_option_ident_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | AUTO | COMMA | CONST | ENUM | EXTERN | ID _ | INLINE | LBRACKET | LPAREN | NORETURN | REGISTER | RPAREN | SEMI | STAR | STATIC | STRUCT | TCHAR | TDOUBLE | TFLOAT | TINT | TLONG | TSHORT | TSIGNED | TUNSIGNED | TVOID | TYPEDEF | TYPE_ID _ | UNION | VOLATILE ->
          let MenhirCell1_STRUCT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_186 _2 in
          _menhir_goto_struct_or_union_spec _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_259 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_ENUM as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AUTO | COMMA | CONST | ENUM | EXTERN | ID _ | INLINE | LBRACKET | LPAREN | NORETURN | REGISTER | RPAREN | SEMI | STAR | STATIC | STRUCT | TCHAR | TDOUBLE | TFLOAT | TINT | TLONG | TSHORT | TSIGNED | TUNSIGNED | TVOID | TYPEDEF | TYPE_ID _ | UNION | VOLATILE ->
          let MenhirCell1_ENUM (_menhir_stack, _menhir_s) = _menhir_stack in
          let _ = _menhir_action_063 () in
          _menhir_goto_enum_spec _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | LBRACE ->
          let x = _v in
          let _v = _menhir_action_130 x in
          _menhir_goto_option_ident_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_158 : type  ttv_stack. ((ttv_stack, _menhir_box_translation_unit) _menhir_cell1_DOT as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | LBRACKET ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
          _menhir_run_154 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | DOT ->
          let _menhir_stack = MenhirCell1_ident (_menhir_stack, _menhir_s, _v) in
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | EQ ->
          let MenhirCell1_DOT (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_044 _2 in
          _menhir_goto_designator_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
      | _ ->
          _eRR ()
  
  and _menhir_run_140 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_144 _1 _3 in
      _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_137 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_postfix_expr -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_postfix_expr (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_143 _1 _3 in
      _menhir_goto_postfix_expr _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_023 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok ->
      let _v = _menhir_action_059 () in
      match (_tok : MenhirBasics.token) with
      | EQ ->
          let _menhir_stack = MenhirCell1_enum_const (_menhir_stack, _menhir_s, _v) in
          let _menhir_s = MenhirState027 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | STR _v ->
              _menhir_run_028 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | STAR ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | SIZEOF ->
              _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | PLUS ->
              _menhir_run_031 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | NOT ->
              _menhir_run_032 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | MINUS ->
              _menhir_run_033 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | LPAREN ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | INT _v ->
              _menhir_run_035 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | INC ->
              _menhir_run_036 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | ID _v ->
              _menhir_run_038 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | FLOAT _v ->
              _menhir_run_039 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | DEC ->
              _menhir_run_040 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | CHAR _v ->
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s
          | BANG ->
              _menhir_run_042 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | AND ->
              _menhir_run_043 _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s
          | _ ->
              _eRR ())
      | COMMA | RBRACE ->
          let _ = _menhir_action_057 () in
          _menhir_goto_enum _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_decl_specs : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState296 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState363 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState308 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState359 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState348 ->
          _menhir_run_351 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState282 ->
          _menhir_run_284 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState185 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState201 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState212 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState216 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState205 ->
          _menhir_run_211 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_351 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | SEMI ->
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState351
      | ID _v_0 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState351
      | _ ->
          _eRR ()
  
  and _menhir_run_285 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _) = _menhir_stack in
      let _v = _menhir_action_031 () in
      _menhir_goto_decl _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_284 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState284
      | SEMI ->
          _menhir_run_285 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LPAREN ->
          _menhir_run_262 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState284
      | ID _v_0 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState284
      | _ ->
          _eRR ()
  
  and _menhir_run_211 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_decl_specs (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR ->
          _menhir_run_181 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LPAREN ->
          _menhir_run_212 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | LBRACKET ->
          _menhir_run_190 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState211
      | ID _v_0 ->
          _menhir_run_213 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState211
      | COMMA | RPAREN ->
          let _v = _menhir_action_123 () in
          _menhir_goto_option_abstract_declarator_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_210 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_decl_specs_sub -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_decl_specs_sub (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_039 _1 _2 in
      _menhir_goto_decl_specs_sub _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_182 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_qual (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState182
      | COMMA | ID _ | LBRACKET | LPAREN | RPAREN | STAR ->
          let _v_0 = _menhir_action_109 () in
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_183 : type  ttv_stack. (ttv_stack, _menhir_box_translation_unit) _menhir_cell1_type_qual -> _ -> _ -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_type_qual (_menhir_stack, _menhir_s, x) = _menhir_stack in
      let xs = _v in
      let _v = _menhir_action_110 x xs in
      _menhir_goto_list_type_qual_ _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_list_type_qual_ : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState181 ->
          _menhir_run_184 _menhir_stack _menhir_lexbuf _menhir_lexer _tok
      | MenhirState182 ->
          _menhir_run_183 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_146 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_translation_unit) _menhir_state -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_qual (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState146
      | _ ->
          _eRR ()
  
  let _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_translation_unit =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | VOLATILE ->
          _menhir_run_001 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | UNION ->
          _menhir_run_002 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TYPE_ID _ ->
          _menhir_run_007 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TYPEDEF ->
          _menhir_run_186 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TVOID ->
          _menhir_run_008 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TUNSIGNED ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TSIGNED ->
          _menhir_run_010 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TSHORT ->
          _menhir_run_011 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TLONG ->
          _menhir_run_012 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TINT ->
          _menhir_run_013 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TFLOAT ->
          _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TDOUBLE ->
          _menhir_run_015 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | TCHAR ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | STRUCT ->
          _menhir_run_017 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | STATIC ->
          _menhir_run_187 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | REGISTER ->
          _menhir_run_188 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | NORETURN ->
          _menhir_run_189 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | INLINE ->
          _menhir_run_193 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EXTERN ->
          _menhir_run_194 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | ENUM ->
          _menhir_run_020 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | CONST ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | AUTO ->
          _menhir_run_195 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState000
      | EOF ->
          let _v = _menhir_action_103 () in
          _menhir_run_279 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let translation_unit =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_translation_unit v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 490 "lib/parser.mly"
  
# 11891 "lib/parser.ml"
