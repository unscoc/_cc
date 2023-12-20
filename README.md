# _CC Under Score C Compiler

Usage

```bash
$ dune exec -- _cc example.c
```

Result

```
[(12,
  (Ast.VarDef (
     ("var",
      (Ast.TArr ((Ast.TPtr (Ast.TDeclSpec [Ast.TsInt])),
         (Ast.EBinary (Ast.Add, (Ast.EConst (Ast.VInt "4")),
            (Ast.EConst (Ast.VInt "5"))))
         ))),
     (Ast.IScal (Ast.EConst (Ast.VStr "abc"))))));
  (11,
   (Ast.VarDef (("x", (Ast.TDeclSpec [(Ast.TsStruct 0)])),
      (Ast.IVect
         [((Ast.DField ("x", Ast.Dnone)),
           (Ast.IScal (Ast.EConst (Ast.VInt "0"))))])
      )));
  (10,
   (Ast.VarDef (("i", (Ast.TDeclSpec [Ast.TsUnsigned])),
      (Ast.IScal (Ast.EConst (Ast.VInt "5"))))));
  (9,
   (Ast.VarDef (("c", (Ast.TDeclSpec [Ast.TsChar])),
      (Ast.IScal (Ast.EConst (Ast.VChar "\\n"))))));
  (8,
   (Ast.FunctionDef (("main", (Ast.TFun ((Ast.TDeclSpec [Ast.TsInt]), []))),
      (Ast.SStmts
         [(Ast.SDef [3]); (Ast.SGoto "label"); (Ast.SDef [5]); (Ast.SDef []);
           (Ast.SExpr (Some (Ast.EVar 1)));
           (Ast.SLabel ("label", (Ast.SDef [6])));
           (Ast.SFor (
              (Some (Ast.EAssign ((Ast.EVar 6), (Ast.EConst (Ast.VInt "0"))))),
              (Some (Ast.EBinary (Ast.Lt, (Ast.EVar 6),
                       (Ast.EConst (Ast.VInt "5"))))),
              (Some (Ast.EPostfix ((Ast.EVar 6), Ast.PInc))),
              (Ast.SStmts
                 [(Ast.SExpr
                     (Some (Ast.EPostfix ((Ast.EVar 2),
                              (Ast.PCall [(Ast.EConst (Ast.VStr "%d"))])))));
                   (Ast.SIfElse (
                      (Ast.EBinary (Ast.Eq, (Ast.EVar 6),
                         (Ast.EConst (Ast.VInt "4")))),
                      Ast.SBreak, (Ast.SStmts [])));
                   (Ast.SIfElse (
                      (Ast.EBinary (Ast.Lt, (Ast.EVar 6),
                         (Ast.EConst (Ast.VInt "5")))),
                      Ast.SContinue, (Ast.SStmts [])))
                   ])
              ));
           (Ast.SSwitch ((Ast.EConst (Ast.VInt "0")),
              [(Ast.SCase ((Ast.EConst (Ast.VInt "0")), []));
                (Ast.SCase ((Ast.EConst (Ast.VInt "1")), []));
                (Ast.SDefault [(Ast.SDef [7]); Ast.SBreak])]
              ));
           (Ast.SReturn (Some (Ast.EConst (Ast.VInt "0"))))])
      )));
  (7,
   (Ast.VarDef (("printf", (Ast.TDeclSpec [Ast.TsInt])),
      (Ast.IScal (Ast.EConst (Ast.VInt "0"))))));
  (6,
   (Ast.VarDef (("i", (Ast.TDeclSpec [Ast.TsInt])),
      (Ast.IScal (Ast.EConst (Ast.VInt "0"))))));
  (5, (Ast.Decl ("a", (Ast.TDeclSpec [(Ast.TsStruct 4)]))));
  (4, (Ast.StructDef ("Y", [("a", (Ast.TDeclSpec [Ast.TsInt]))])));
  (3,
   (Ast.VarDef (("b", (Ast.TDeclSpec [(Ast.TsStruct 0)])),
      (Ast.IScal
         (Ast.EPostfix ((Ast.EVar 1),
            (Ast.PCall
               [(Ast.ECompoundLit ((Ast.TDeclSpec [(Ast.TsStruct 0)]),
                   (Ast.IVect
                      [((Ast.DField ("x", Ast.Dnone)),
                        (Ast.IScal (Ast.EConst (Ast.VInt "0"))))])
                   ))
                 ])
            )))
      )));
  (2,
   (Ast.Decl
      ("printf",
       (Ast.TFun ((Ast.TDeclSpec [Ast.ScsExtern; Ast.TsInt]),
          [("", (Ast.TPtr (Ast.TDeclSpec [Ast.TsChar])))])))));
  (1,
   (Ast.FunctionDef (
      ("func",
       (Ast.TFun ((Ast.TDeclSpec [Ast.ScsStatic; (Ast.TsStruct 0)]),
          [("a", (Ast.TDeclSpec [(Ast.TsStruct 0)]))]))),
      (Ast.SStmts
         [(Ast.SReturn
             (Some (Ast.ECompoundLit ((Ast.TDeclSpec [(Ast.TsStruct 0)]),
                      (Ast.IVect
                         [(Ast.Dnone, (Ast.IScal (Ast.EConst (Ast.VInt "0"))))
                           ])
                      ))))
           ])
      )));
  (0, (Ast.StructDef ("X", [("x", (Ast.TDeclSpec [Ast.TsUnsigned]))])))]
```

Output JSON

```bash
$ dune exec -- _cc -json example.c
```

Result

```json
{"0": ["StructDef", "X", {"x": ["TDeclSpec", [["TsUnsigned"]]]}], "1": ["FunctionDef", ["func", ["TFun", ["TDeclSpec", [["ScsStatic"], ["TsStruct(0)"]]], {"a": ["TDeclSpec", [["TsStruct(0)"]]]}]], ["SStmts", [["SReturn", ["Some", ["ECompoundLit", ["TDeclSpec", [["TsStruct(0)"]]], ["IVect", [[["Dnone"], ["IScal", ["EConst", ["VInt", 0]]]]]]]]]]]], "2": ["Decl", ["printf", ["TFun", ["TDeclSpec", [["ScsExtern"], ["TsInt"]]], {"": ["TPtr", ["TDeclSpec", [["TsChar"]]]]}]]], "3": ["VarDef", ["b", ["TDeclSpec", [["TsStruct(0)"]]]], ["IScal", ["EPostfix", ["EVar", 1], ["PCall", [["ECompoundLit", ["TDeclSpec", [["TsStruct(0)"]]], ["IVect", [[["DField", "x", ["Dnone"]], ["IScal", ["EConst", ["VInt", 0]]]]]]]]]]]], "4": ["StructDef", "Y", {"a": ["TDeclSpec", [["TsInt"]]]}], "5": ["Decl", ["a", ["TDeclSpec", [["TsStruct(4)"]]]]], "6": ["VarDef", ["i", ["TDeclSpec", [["TsInt"]]]], ["IScal", ["EConst", ["VInt", 0]]]], "7": ["VarDef", ["printf", ["TDeclSpec", [["TsInt"]]]], ["IScal", ["EConst", ["VInt", 0]]]], "8": ["FunctionDef", ["main", ["TFun", ["TDeclSpec", [["TsInt"]]], {}]], ["SStmts", [["SDef", [3]], ["SGoto", "label"], ["SDef", [5]], ["SDef", []], ["SExpr", ["Some", ["EVar", 1]]], ["SLabel", "label", ["SDef", [6]]], ["SFor", ["Some", ["EAssign", ["EVar", 6], ["EConst", ["VInt", 0]]]], ["Some", ["EBinary", "Lt", ["EVar", 6], ["EConst", ["VInt", 5]]]], ["Some", ["EPostfix", ["EVar", 6], ["PInc"]]], ["SStmts", [["SExpr", ["Some", ["EPostfix", ["EVar", 2], ["PCall", [["EConst", ["VStr","%d"]]]]]]], ["SIfElse", ["EBinary", "Eq", ["EVar", 6], ["EConst", ["VInt", 4]]], ["SBreak"], ["SStmts", []]], ["SIfElse", ["EBinary", "Lt", ["EVar", 6], ["EConst", ["VInt", 5]]], ["SContinue"], ["SStmts", []]]]]], ["SSwitch", ["EConst", ["VInt", 0]], [["SCase", ["EConst", ["VInt", 0]], []], ["SCase", ["EConst", ["VInt", 1]], []], ["SDefault", [["SDef", [7]], ["SBreak"]]]]], ["SReturn", ["Some", ["EConst", ["VInt", 0]]]]]]], "9": ["VarDef", ["c", ["TDeclSpec", [["TsChar"]]]], ["IScal", ["EConst", ["VChar", "\n"]]]], "10": ["VarDef", ["i", ["TDeclSpec", [["TsUnsigned"]]]], ["IScal", ["EConst", ["VInt", 5]]]], "11": ["VarDef", ["x", ["TDeclSpec", [["TsStruct(0)"]]]], ["IVect", [[["DField", "x", ["Dnone"]], ["IScal", ["EConst", ["VInt", 0]]]]]]], "12": ["VarDef", ["var", ["TArr", ["TPtr", ["TDeclSpec", [["TsInt"]]]], ["EBinary", "Add", ["EConst", ["VInt", 4]], ["EConst", ["VInt", 5]]]]], ["IScal", ["EConst", ["VStr","abc"]]]]}
```
