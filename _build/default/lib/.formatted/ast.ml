exception ASTError of string

let raise exn =
  match exn with
  (*| ASTError msg -> Printf.printf "%s\n" msg;raise exn*)
  | _ -> raise exn

let spr fmt s = Printf.sprintf fmt s

type value = VInt of string | VFloat of string | VStr of string
[@@deriving show]

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | LShift
  | RShift
  | BitAnd
  | BitXor
  | BitOr
  | LogAnd
  | LogOr
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
  | Comma
[@@deriving show]

type unary = Plus | Minus | BitNot | LogNot | Ref | Deref | Sizeof
[@@deriving show]

type item =
  | Decl of decl
  | StructDecl of string
  | UnionDecl of string
  | EnumDecl of string
  | VarDef of decl * init
  | StructDef of string * decl list
  | UnionDef of string * decl list
  | EnumDef of string * (string * int) list
  | FunctionDef of decl * stmt
[@@deriving show]

and id = int [@@deriving show]
and id_list = int list [@@deriving show]
and id_list_list = int list list [@@deriving show]
and program = item list [@@deriving show]
and programi = (id * item) list [@@deriving show]

and expr =
  | ENone
  | EConst of value
  | EVar of id
  | EBinary of binary * expr * expr
  | EAssign of expr * expr
  | EUnary of unary * expr
  | ETyUnary of unary * ty
  | EPostfix of expr * postfix
  | ECond of expr * expr * expr
  | ECast of ty * expr
  | ECompoundLit of ty * init
[@@deriving show]

and postfix =
  | PCall of expr list
  | PIdx of expr
  | PDot of string
  | PArrow of string
  | PInc
  | PDec
[@@deriving show]

and init = IScal of expr | IVect of (desig * init) list [@@deriving show]

and desig = Dnone | DIdx of expr * desig | DField of string * desig
[@@deriving show]

and stmt =
  | SDef of id list
  | SStmts of stmt list
  | SWhile of expr * stmt
  | SDoWhile of stmt * expr
  | SFor of expr option * expr option * expr option * stmt
  | SIfElse of expr * stmt * stmt
  | SReturn of expr option
  | SLabel of string * stmt
  | SGoto of string
  | SContinue
  | SBreak
  | SSwitch of expr * stmt list
  | SCase of expr * stmt list
  | SDefault of stmt list
  | SExpr of expr option
[@@deriving show]

and ty =
  | TFun of ty * decl list
  | TPtr of ty
  | TArr of ty * expr
  | TDeclSpec of ds list
[@@deriving show]

and decl = string * ty [@@deriving show]

and ds =
  | TsInt
  | TsShort
  | TsLong
  | TsChar
  | TsSigned
  | TsUnsigned
  | TsFloat
  | TsDouble
  | TsVoid
  | TsStruct of int
  | TsUnion of int
  | TsTypedef of int
  | ScsTypedef
  | ScsExtern
  | ScsStatic
  | ScsAuto
  | ScsRegister
  | TqConst
  | TqVolatile
  | FsInline
  | FsNoreturn
[@@deriving show]

let rec get_declspec = function
  | TFun (ty, _) -> get_declspec ty
  | TPtr ty -> get_declspec ty
  | TArr (ty, _) -> get_declspec ty
  | TDeclSpec l -> l
