open Ast

let value = function
  | VInt x -> Printf.sprintf "[\"VInt\", %s]" x
  | VChar x -> Printf.sprintf "[\"VChar\", \"%s\"]" x
  | VFloat x -> Printf.sprintf "[\"VFloat\", %s]" x
  | VStr x -> Printf.sprintf "[\"VStr\",%S]" x

let binary = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | LShift -> "LShift"
  | RShift -> "RShift"
  | BitAnd -> "BitAnd"
  | BitXor -> "BitXor"
  | BitOr -> "BitOr"
  | LogAnd -> "LogAnd"
  | LogOr -> "LogOr"
  | Lt -> "Lt"
  | Le -> "Le"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Eq -> "Eq"
  | Ne -> "Ne"
  | Comma -> "Comma"

let unary = function
  | Plus -> "Plus"
  | Minus -> "Minus"
  | BitNot -> "BitNot"
  | LogNot -> "LogNot"
  | Ref -> "Ref"
  | Deref -> "Deref"
  | Sizeof -> "Sizeof"

let x_int_list xis =
  xis |> List.map(fun (x, i) -> Printf.sprintf "%S: %d" x i)
      |> String.concat ", "
      |> Printf.sprintf "{%s}"

let rec item = function
  | Decl d -> Printf.sprintf "[\"Decl\", %s]" (decl d)
  | StructDecl x -> Printf.sprintf "[\"StructDecl\", %S]" x
  | UnionDecl x -> Printf.sprintf "[\"UnionDecl\", %S]" x
  | EnumDecl x -> Printf.sprintf "[\"EnumDecl\", %S]" x
  | VarDef (d,i) -> Printf.sprintf "[\"VarDef\", %s, %s]" (decl d) (init i)
  | StructDef(x,ds) -> Printf.sprintf "[\"StructDef\", %S, %s]" x (decl_list ds)
  | UnionDef(x,ds) -> Printf.sprintf "[\"UnionDef\", %S, %s]" x (decl_list ds)
  | EnumDef(x,xis) -> Printf.sprintf "[\"EnumDef\", %S, %s]" x (x_int_list xis)
  | FunctionDef(d,s) -> Printf.sprintf "[\"FunctionDef\", %s, %s]" (decl d) (stmt s)
and id i = Printf.sprintf "%d" i
and id_list is = is |> List.map id |> String.concat ", " |> Printf.sprintf "[%s]"
and id_list_list iss = iss |> List.map id_list |> String.concat ", " |> Printf.sprintf "[%s]"
and program is = is |> List.map item |> String.concat ", " |> Printf.sprintf "[%s]"
and programi iis = iis
  |> List.map (fun (i,it) -> Printf.sprintf "\"%d\": %s" i (item it))
  |> String.concat ", "
  |> Printf.sprintf "{%s}"
and expr = function
  | ENone -> "[\"ENone\"]"
  | EConst(v) -> Printf.sprintf "[\"EConst\", %s]" (value v)
  | EVar(i) -> Printf.sprintf "[\"EVar\", %s]" (id i)
  | EBinary(b,e1,e2) -> Printf.sprintf "[\"EBinary\", %S, %s, %s]" (binary b) (expr e1) (expr e2)
  | EAssign(e1,e2) -> Printf.sprintf "[\"EAssign\", %s, %s]" (expr e1) (expr e2)
  | EUnary(u,e) -> Printf.sprintf "[\"EUnary\", %S, %s]" (unary u) (expr e)
  | ETyUnary(u,t) -> Printf.sprintf "[\"ETyUnary\", %S, %s]" (unary u) (ty t)
  | EPostfix(e,p) -> Printf.sprintf "[\"EPostfix\", %s, %s]" (expr e) (postfix p)
  | ECond(e1,e2,e3) -> Printf.sprintf "[\"ECond\", %s, %s, %s]" (expr e1) (expr e2) (expr e3)
  | ECast(t,e) -> Printf.sprintf "[\"ECast\", %s, %s]" (ty t) (expr e)
  | ECompoundLit(t,i) -> Printf.sprintf "[\"ECompoundLit\", %s, %s]" (ty t) (init i)
and expr_list es =  es |> List.map expr |> String.concat ", " |> Printf.sprintf "[%s]"
and expr_option = function
  | Some(e) -> Printf.sprintf "[\"Some\", %s]" (expr e)
  | None -> "[\"None\"]"
and postfix = function
  | PCall(es) -> Printf.sprintf "[\"PCall\", %s]" (expr_list es)
  | PIdx(e) -> Printf.sprintf "[\"PIdx\", %s]" (expr e)
  | PDot(x) -> Printf.sprintf "[\"PDot\", %S]" x
  | PArrow(x) -> Printf.sprintf "[\"PArrow\", %S]" x
  | PInc -> "[\"PInc\"]"
  | PDec -> "[\"PDec\"]"
and init = function
  | IScal(e) -> Printf.sprintf "[\"IScal\", %s]" (expr e)
  | IVect(dis) -> Printf.sprintf "[\"IVect\", %s]" (desigi_list dis)
and desig = function
  | Dnone -> "[\"Dnone\"]"
  | DIdx(e,d) -> Printf.sprintf "[\"DIdx\", %s, %s]" (expr e) (desig d)
  | DField(x,d) -> Printf.sprintf "[\"DField\", %S, %s]" x (desig d)
and desigi_list dis =
  dis 
  |> List.map (fun (d,i) -> Printf.sprintf "[%s, %s]" (desig d) (init i))
  |> String.concat ", "
  |> Printf.sprintf "[%s]"
and stmt = function
  | SDef(ids) -> Printf.sprintf "[\"SDef\", %s]" (id_list ids)
  | SStmts(ss) -> Printf.sprintf "[\"SStmts\", %s]" (stmt_list ss)
  | SWhile(e,s) -> Printf.sprintf "[\"SWhile\", %s, %s]" (expr e) (stmt s)
  | SDoWhile(s,e) -> Printf.sprintf "[\"SDoWhile\", %s, %s]" (stmt s) (expr e)
  | SFor(e1,e2,e3,s) -> Printf.sprintf "[\"SFor\", %s, %s, %s, %s]" (expr_option e1) (expr_option e2) (expr_option e3) (stmt s)
  | SIfElse(e,s1,s2) -> Printf.sprintf "[\"SIfElse\", %s, %s, %s]" (expr e) (stmt s1) (stmt s2)
  | SReturn(eo) -> Printf.sprintf "[\"SReturn\", %s]" (expr_option eo)
  | SLabel(x,s) -> Printf.sprintf "[\"SLabel\", %S, %s]" x (stmt s)
  | SGoto(x) -> Printf.sprintf "[\"SGoto\", %S]" x
  | SContinue -> Printf.sprintf "[\"SContinue\"]"
  | SBreak -> Printf.sprintf "[\"SBreak\"]"
  | SSwitch(e, ss) -> Printf.sprintf "[\"SSwitch\", %s, %s]" (expr e) (stmt_list ss)
  | SCase(e,ss) -> Printf.sprintf "[\"SCase\", %s, %s]" (expr e) (stmt_list ss)
  | SDefault(ss) -> Printf.sprintf "[\"SDefault\", %s]" (stmt_list ss)
  | SExpr(oe) -> Printf.sprintf "[\"SExpr\", %s]" (expr_option oe)
and stmt_list ss = ss |> List.map stmt |> String.concat ", " |> Printf.sprintf "[%s]"

and ty = function
  | TFun(t,ds) -> Printf.sprintf "[\"TFun\", %s, %s]" (ty t) (decl_list ds)
  | TPtr(t) -> Printf.sprintf "[\"TPtr\", %s]" (ty t)
  | TArr(t,e) -> Printf.sprintf "[\"TArr\", %s, %s]" (ty t) (expr e)
  | TDeclSpec(dss) -> Printf.sprintf "[\"TDeclSpec\", %s]" (ds_list dss)
and decl (x,t) = Printf.sprintf "[%S, %s]" x (ty t) 
and decl_list ds = 
  ds
  |> List.map (fun (x,t) -> Printf.sprintf "%S: %s" x (ty t))
  |> String.concat ", "
  |> Printf.sprintf "{%s}"
and ds = function
  | TsInt -> "[\"TsInt\"]"
  | TsShort -> "[\"TsShort\"]"
  | TsLong -> "[\"TsLong\"]"
  | TsChar -> "[\"TsChar\"]"
  | TsSigned -> "[\"TsSigned\"]"
  | TsUnsigned -> "[\"TsUnsigned\"]"
  | TsFloat -> "[\"TsFloat\"]"
  | TsDouble -> "[\"TsDouble\"]"
  | TsVoid -> "[\"TsVoid\"]"
  | TsStruct(i) -> Printf.sprintf "[\"TsStruct(%d)\"]" i
  | TsUnion(i) -> Printf.sprintf "[\"TsUnion(%d)\"]" i
  | TsTypedef(i) -> Printf.sprintf "[\"TsTypedef(%d)\"]" i
  | ScsTypedef -> "[\"ScsTypedef\"]"
  | ScsExtern -> "[\"ScsExtern\"]"
  | ScsStatic -> "[\"ScsStatic\"]"
  | ScsAuto -> "[\"ScsAuto\"]"
  | ScsRegister -> "[\"ScsRegister\"]"
  | TqConst -> "[\"TqConst\"]"
  | TqVolatile -> "[\"TqVolatile\"]"
  | FsInline -> "[\"FsInline\"]"
  | FsNoreturn -> "[\"FsNoreturn\"]"
and ds_list dss = dss |> List.map ds |> String.concat ", " |> Printf.sprintf "[%s]"
