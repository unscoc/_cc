%{
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
  List.map (function 
    | (d,Some init) -> push_def (VarDef(make_decl ty d,init))
    | (d,None) -> push_def (Decl (make_decl ty d))
  ) init_decl_list

let conv_ident = function
  | Some s -> s 
  | None -> ""
%}
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE DOT COMMA
%token AND STAR PLUS MINUS NOT BANG DIV MOD LT GT HAT OR 
%token COLON QUESTION SEMI EQ INLINE NORETURN
%token SIZEOF EOF
%token ARROW INC DEC LSHIFT RSHIFT LE GE EQEQ NE ELLIPSIS
%token ANDAND OROR MUL_EQ DIV_EQ MOD_EQ ADD_EQ
%token SUB_EQ LSHIFT_EQ RSHIFT_EQ AND_EQ
%token XOR_EQ OR_EQ
%token TYPEDEF EXTERN STATIC AUTO REGISTER
%token TCHAR TSHORT TINT TLONG TSIGNED TUNSIGNED TFLOAT TDOUBLE CONST VOLATILE TVOID
%token STRUCT UNION ENUM 
%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%token <string> CHAR INT
%token <string> FLOAT
%token<string> STR
%token<string> ID TYPE_ID

%nonassoc NO_ELSE
%nonassoc  ELSE 

%type<id list> translation_unit
%start translation_unit
%%

translation_unit:
| list(external_decl) EOF
  { List.flatten $1 }

ident:
| ID { $1 }
| TYPE_ID { $1 }


primary_expr:
| ID { EVar ( lookup_var $1) }
| CHAR { EConst (VInt $1) }
| INT { EConst (VInt $1) }
| FLOAT { EConst (VFloat $1) }
| STR { EConst ( VStr $1) }
| LPAREN expr RPAREN { $2 }

postfix_expr:
| primary_expr { $1 }
| postfix_expr LBRACKET expr RBRACKET { EPostfix($1,PIdx $3) }
| postfix_expr LPAREN argument_expr_list? RPAREN  { match $3 with
                                                    | Some l -> EPostfix( $1,PCall l)
                                                    | None -> EPostfix( $1,PCall []) }
| postfix_expr DOT ident { EPostfix( $1,PDot $3) }
| postfix_expr ARROW ident { EPostfix( $1,PArrow $3) }
| postfix_expr INC { EPostfix($1,PInc) }
| postfix_expr DEC { EPostfix($1,PDec) }
| LPAREN type_name RPAREN LBRACE init_list COMMA? RBRACE { ECompoundLit( $2,IVect $5) }

argument_expr_list:
| assignment_expr { [$1] }
| argument_expr_list COMMA assignment_expr { $1@[$3] }

unary_expr:
| postfix_expr { $1 }
| INC unary_expr { EUnary(Plus, $2)  }
| DEC unary_expr { EUnary(Minus, $2) }
| AND cast_expr  { EUnary( Ref, $2) }
| STAR cast_expr { EUnary( Deref, $2) }
| PLUS cast_expr { EUnary( Plus, $2) }
| MINUS cast_expr { EUnary( Minus, $2) }
| NOT cast_expr { EUnary( BitNot, $2) }
| BANG cast_expr { EUnary( LogNot, $2) }
| SIZEOF unary_expr { EUnary( Sizeof, $2) }
| SIZEOF LPAREN type_name RPAREN { ETyUnary(Sizeof,$3) }


cast_expr:
| unary_expr { $1 }
| LPAREN type_name RPAREN cast_expr { ECast( $2,$4) }

multiplicative_expr:
| cast_expr { $1 }
| multiplicative_expr STAR cast_expr { EBinary( Mul,$1,$3) }
| multiplicative_expr DIV cast_expr { EBinary( Div,$1,$3) }
| multiplicative_expr MOD cast_expr { EBinary( Mod,$1,$3) }

additive_expr:
| multiplicative_expr { $1 }
| additive_expr PLUS multiplicative_expr { EBinary( Add,$1,$3) }
| additive_expr MINUS multiplicative_expr { EBinary( Sub,$1,$3) }

shift_expr:
| additive_expr { $1 }
| shift_expr LSHIFT additive_expr { EBinary( LShift,$1,$3) }
| shift_expr RSHIFT additive_expr { EBinary( RShift,$1,$3) }

relational_expr:
| shift_expr { $1 }
| relational_expr LT shift_expr { EBinary( Lt,$1,$3) }
| relational_expr GT shift_expr { EBinary( Gt,$1,$3) }
| relational_expr LE shift_expr { EBinary( Le,$1,$3) }
| relational_expr GE shift_expr { EBinary( Ge,$1,$3) }

equality_expr:
| relational_expr { $1 }
| equality_expr EQEQ relational_expr { EBinary( Eq,$1,$3) }
| equality_expr NE relational_expr { EBinary( Ne,$1,$3) }

and_expr:
| equality_expr { $1 }
| and_expr AND equality_expr { EBinary( BitAnd,$1,$3) }

exclusive_or_expr:
| and_expr { $1 }
| exclusive_or_expr HAT and_expr { EBinary( BitXor,$1,$3) }

inclusive_or_expr:
| exclusive_or_expr { $1 }
| inclusive_or_expr OR exclusive_or_expr { EBinary( BitOr,$1,$3) }

logical_and_expr:
| inclusive_or_expr { $1 }
| logical_and_expr ANDAND inclusive_or_expr { EBinary( LogAnd,$1,$3) }

logical_or_expr:
| logical_and_expr { $1 }
| logical_or_expr OROR logical_and_expr { EBinary( LogOr,$1,$3) }

conditional_expr:
| logical_or_expr { $1 }
| logical_or_expr QUESTION expr COLON conditional_expr { ECond( $1,$3,$5) }

assignment_expr:
| conditional_expr { $1 }
| unary_expr EQ assignment_expr { EAssign( $1,$3) }
| unary_expr MUL_EQ assignment_expr { EAssign( $1,EBinary( Mul,$1,$3)) }
| unary_expr DIV_EQ assignment_expr { EAssign( $1,EBinary( Div,$1,$3)) }
| unary_expr MOD_EQ assignment_expr { EAssign( $1,EBinary( Mod,$1,$3)) }
| unary_expr ADD_EQ assignment_expr { EAssign( $1,EBinary( Add,$1,$3)) }
| unary_expr SUB_EQ assignment_expr { EAssign( $1,EBinary( Sub,$1,$3)) }
| unary_expr LSHIFT_EQ assignment_expr { EAssign( $1,EBinary( LShift,$1,$3)) }
| unary_expr RSHIFT_EQ assignment_expr { EAssign( $1,EBinary( RShift,$1,$3)) }
| unary_expr AND_EQ assignment_expr { EAssign( $1,EBinary( BitAnd,$1,$3)) }
| unary_expr XOR_EQ assignment_expr { EAssign( $1,EBinary( BitXor,$1,$3)) }
| unary_expr OR_EQ assignment_expr { EAssign( $1,EBinary( BitOr,$1,$3)) }

expr:
| assignment_expr { $1 }
| expr COMMA assignment_expr { EBinary( Comma,$1,$3) }

constant_expr:
| conditional_expr { $1 } 


decl:
| decl_specs SEMI { [] }
| decl_specs init_declarator_list SEMI { make_decls_with_init $1 $2 }

decl_spec:
| storage_class_spec {  [$1] }
| type_qual { [$1] }
| function_spec { [$1] }
| type_spec { [$1] }

decl_specs:
| decl_specs_sub { TDeclSpec $1 }

decl_specs_sub:
| decl_spec { $1 }
| decl_specs_sub decl_spec {  $1 @ $2 }

init_declarator_list:
| init_declarator { [$1] }
| init_declarator_list COMMA init_declarator { $1 @ [$3] }

init_declarator:
| declarator { ($1,None) }
| declarator EQ init { ($1,Some $3) }

storage_class_spec:
| TYPEDEF { ScsTypedef }
| EXTERN { ScsExtern }
| STATIC { ScsStatic }
| AUTO { ScsAuto }
| REGISTER { ScsRegister }

type_spec:
| TVOID { TsVoid }
| TCHAR { TsChar }
| TSHORT { TsShort}
| TINT { TsInt }
| TLONG { TsLong }
| TFLOAT { TsFloat }
| TDOUBLE { TsDouble }
| TSIGNED { TsSigned }
| TUNSIGNED { TsUnsigned  }
| struct_or_union_spec { $1 }
| enum_spec { TsInt }
| TYPE_ID { TsInt }

spec_qual_list:
| spec_qual_list_sub { $1 }

spec_qual_list_sub:
| type_spec { $1::[] }
| type_spec spec_qual_list_sub { $1::$2 }
| type_qual spec_qual_list_sub { $1::$2 }

type_qual:
| CONST { TqConst }
| VOLATILE { TqVolatile }

function_spec:
| INLINE { FsInline }
| NORETURN { FsNoreturn }


struct_or_union_spec:
| STRUCT ident? LBRACE list(struct_decl) RBRACE { make_structdef (conv_ident $2)  (List.flatten $4) }
| STRUCT ident { make_structdecl $2 } 
| UNION ident? LBRACE list(struct_decl) RBRACE { make_uniondef (conv_ident $2) (List.flatten $4) }
| UNION ident { make_uniondecl $2 }

struct_decl:
| spec_qual_list struct_declarator_list? SEMI { match $2 with
                                                | Some dl -> make_decls (TDeclSpec $1) dl
                                                | None -> failwith "not impl" }
struct_declarator_list:
| struct_declarator { [$1] }
| struct_declarator_list COMMA struct_declarator { $1@[$3] }

struct_declarator:
| declarator { $1 }

enum_spec:
| ENUM ident? LBRACE enum_list COMMA? RBRACE
| ENUM ident { }

enum_list:
| enum
| enum_list COMMA enum { }

enum:
| enum_const
| enum_const EQ constant_expr { }

enum_const:
| ident { }

declarator:
| pointer declarator { DeclPtr $2 }
| direct_declarator { $1 }

direct_declarator:
| ID { DeclIdent $1 }
| LPAREN declarator RPAREN { $2 }

| direct_declarator LBRACKET constant_expr RBRACKET { DeclArr($1, $3) }
| direct_declarator LPAREN parameter_type_list RPAREN { DeclFun($1,$3) }

pointer:
| STAR list(type_qual) { }

parameter_type_list:
| { [] }
| parameter_list option(COMMA ELLIPSIS {}) { $1 }

parameter_list:
| parameter_decl { $1 }
| parameter_list COMMA parameter_decl { $1 @ $3 }

parameter_decl:
| decl_specs declarator { [make_decl $1 $2] }
| decl_specs abstract_declarator? { match $2 with
                                    | Some d -> [make_decl $1 d]
                                    | None -> [make_decl $1 (DeclIdent "")] }

abstract_declarator:
| pointer { DeclPtr(DeclIdent "") }
| pointer abstract_declarator { DeclPtr $2 }
| direct_abstract_declarator { $1 }

direct_abstract_declarator:
| LPAREN abstract_declarator RPAREN { $2 }
| LBRACKET constant_expr RBRACKET { DeclArr(DeclIdent "",$2) }
| LPAREN parameter_type_list RPAREN { DeclFun(DeclIdent "",$2) }
| direct_abstract_declarator LBRACKET constant_expr RBRACKET { DeclArr($1,$3) }
| direct_abstract_declarator LPAREN parameter_type_list RPAREN { DeclFun($1,$3) }



type_name:
| spec_qual_list { TDeclSpec $1 }
| spec_qual_list abstract_declarator { TDeclSpec [] }

init:
| assignment_expr { IScal $1 }
| LBRACE init_list COMMA? RBRACE { IVect $2 }

init_list:
| init {[(Dnone,$1)]}
| desig init { [($1,$2)] }
| init_list COMMA init { $1@[(Dnone,$3)] }
| init_list COMMA desig init { $1@[($3,$4)] }

desig:
| designator_list EQ { $1 }

designator_list:
| LBRACKET constant_expr RBRACKET { DIdx($2,Dnone) }
| DOT ident { DField($2,Dnone) }
| LBRACKET constant_expr RBRACKET designator_list {DIdx($2, $4) } 
| DOT ident designator_list { DField($2, $3) }

enter_scope: { enter_scope () }

leave_scope: { leave_scope () }

item:
| decl { SDef($1) }
| stmt { $1 }

stmt:
| labeled_stmt { $1 }
| compound_stmt { $1 }
| expr? SEMI { SExpr $1 }
| selection_stmt_1 { $1 }
| selection_stmt_2 { $1 }
| iteration_stmt { $1 }
| jump_stmt { $1 }

labeled_stmt:
| ident COLON item { SLabel($1,$3) }

case_or_default:
| CASE conditional_expr COLON list(item) { SCase ($2, $4) }
| DEFAULT COLON list(item) { SDefault ( $3) }


compound_stmt:
| enter_scope LBRACE list(item) RBRACE leave_scope { SStmts($3) }

selection_stmt_1:
| IF LPAREN expr RPAREN stmt    %prec NO_ELSE { SIfElse($3,$5,SStmts []) }
| IF LPAREN expr RPAREN stmt ELSE stmt { SIfElse($3,$5,$7) }

selection_stmt_2:
| SWITCH LPAREN expr RPAREN enter_scope LBRACE list(case_or_default) RBRACE leave_scope
  { SSwitch($3,$7) }



iteration_stmt:
| WHILE LPAREN expr RPAREN stmt { SWhile($3,$5) }
| DO stmt WHILE LPAREN expr RPAREN { SDoWhile($2,$5) }
| FOR LPAREN expr? SEMI expr? SEMI expr? RPAREN stmt { SFor($3,$5,$7,$9) }

jump_stmt:
| GOTO ident SEMI { SGoto $2 }
| CONTINUE SEMI { SContinue }
| BREAK SEMI { SBreak }
| RETURN expr? SEMI { SReturn $2 }

external_decl:
| function_def { [push_def $1] }
| decl { $1 }

function_def:
| decl_specs declarator compound_stmt { FunctionDef(make_decl $1 $2,$3) }
