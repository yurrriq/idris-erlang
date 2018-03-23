Constr (Module (Atom "example")
        (ModHeader [(Atom "module_info",0),(Atom "module_info",1),(Atom "p",0)] [])
        [FunDef (Constr (Atom "p",0))
         (Constr
          (Fun []
           (Exp
            (Constr
             (Let (["_cor1"],
                   Exp (Constr (App (Exp (Constr (FunName (Atom "f",1)))))
                                [Exp (Constr (Lit (LInt 2)))]))
               (Exp (Constr (Case (Exp (Constr (Var "_cor1")))
                             [Constr (Clause (Pats [PLit (LAtom (Atom "true"))])
                                      (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                                      (Exp (Ann (Var "_cor1") [CLit (LAtom (Atom "compiler_generated"))]))),
                               Ann (Clause (Pats [PVar "_cor0"])
                                    (Guard (Exp (Constr (Lit (LAtom (Atom "true"))))))
                                    (Exp (Constr (PrimOp (Atom "match_fail")
                                                  [Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "badmatch")))),
                                                                       Exp (Constr (Var "_cor0"))]))])))) [CLit (LAtom (Atom "compiler_generated"))]])))))))),
          FunDef (Constr (Atom "f",1)) (Constr (Fun [Constr "_cor0"] (Exp (Constr (Let (["_cor1"],Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),Exp (Constr (Lit (LAtom (Atom "+"))))) [Exp (Constr (Lit (LInt 2))),Exp (Constr (Var "_cor0"))]))) (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),Exp (Constr (Lit (LAtom (Atom "=:="))))) [Exp (Constr (Lit (LInt 4))),Exp (Constr (Var "_cor1"))])))))))),FunDef (Constr (Atom "module_info",0)) (Ann (Fun [] (Exp (Ann (ModCall (Exp (Ann (Lit (LAtom (Atom "erlang"))) [CLit (LAtom (Atom "compiler_generated"))]),Exp (Ann (Lit (LAtom (Atom "get_module_info"))) [CLit (LAtom (Atom "compiler_generated"))])) [Exp (Ann (Lit (LAtom (Atom "example"))) [CLit (LAtom (Atom "compiler_generated"))])]) [CLit (LAtom (Atom "compiler_generated"))]))) [CLit (LAtom (Atom "compiler_generated"))]),FunDef (Constr (Atom "module_info",1)) (Ann (Fun [Ann "_cor0" [CLit (LAtom (Atom "compiler_generated"))]] (Exp (Ann (ModCall (Exp (Ann (Lit (LAtom (Atom "erlang"))) [CLit (LAtom (Atom "compiler_generated"))]),Exp (Ann (Lit (LAtom (Atom "get_module_info"))) [CLit (LAtom (Atom "compiler_generated"))])) [Exp (Ann (Lit (LAtom (Atom "example"))) [CLit (LAtom (Atom "compiler_generated"))]),Exp (Ann (Var "_cor0") [CLit (LAtom (Atom "compiler_generated"))])]) [CLit (LAtom (Atom "compiler_generated"))]))) [CLit (LAtom (Atom "compiler_generated"))])])


(Let (["_cor1"],
      Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                            Exp (Constr (Lit (LAtom (Atom "+")))))
                    [Exp (Constr (Lit (LInt 2))),
                     Exp (Constr (Var "_cor0"))])))
 (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                        Exp (Constr (Lit (LAtom (Atom "=:=")))))
                [Exp (Constr (Lit (LInt 4))),Exp (Constr (Var "_cor1"))]))))
