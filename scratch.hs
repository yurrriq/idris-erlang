Constr
(Module (Atom "example")
  (ModHeader [ (Atom "add_four",1)
             , (Atom "add_two",1)
             , (Atom "module_info",0)
             , (Atom "module_info",1)
             ]
    [])
  [FunDef (Constr (Atom "add_two",1))
   (Constr (Fun [Constr "_cor0"]
            (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                     Exp (Constr (Lit (LAtom (Atom "+")))))
                             [Exp (Constr (Var "_cor0")),
                              Exp (Constr (Lit (LInt 2)))]))))),
    FunDef (Constr (Atom "add_four",1))
    (Constr (Fun [Constr "_cor0"]
             (Exp (Constr (Let (["_cor1"],
                                Exp (Constr (App (Exp (Constr (FunName (Atom "add_two",1))))
                                             [Exp (Constr (Var "_cor0"))])))
                            (Exp (Constr (App (Exp (Constr (FunName (Atom "add_two",1))))
                                          [Exp (Constr (Var "_cor1"))])))))))),
    FunDef (Constr (Atom "module_info",0))
    (Ann (Fun []
          (Exp (Ann (ModCall (Exp (Ann (Lit (LAtom (Atom "erlang")))
                                   [CLit (LAtom (Atom "compiler_generated"))]),
                               Exp (Ann (Lit (LAtom (Atom "get_module_info")))
                                    [CLit (LAtom (Atom "compiler_generated"))]))
                      [Exp (Ann (Lit (LAtom (Atom "example")))
                            [CLit (LAtom (Atom "compiler_generated"))])])
                 [CLit (LAtom (Atom "compiler_generated"))])))
      [CLit (LAtom (Atom "compiler_generated"))]),
    FunDef (Constr (Atom "module_info",1))
    (Ann (Fun [Ann "_cor0" [CLit (LAtom (Atom "compiler_generated"))]]
          (Exp (Ann (ModCall (Exp (Ann (Lit (LAtom (Atom "erlang")))
                                   [CLit (LAtom (Atom "compiler_generated"))]),
                               Exp (Ann (Lit (LAtom (Atom "get_module_info")))
                                    [CLit (LAtom (Atom "compiler_generated"))]))
                      [Exp (Ann (Lit (LAtom (Atom "example")))
                            [CLit (LAtom (Atom "compiler_generated"))]),
                        Exp (Ann (Var "_cor0")
                             [CLit (LAtom (Atom "compiler_generated"))])])
                 [CLit (LAtom (Atom "compiler_generated"))])))
      [CLit (LAtom (Atom "compiler_generated"))])])




  FunDef (Constr (Atom "main",1))
  (Constr (Fun [Constr "_cor0"]
           (Exp (Constr (Seq (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "dbg")))),
                                                    Exp (Constr (Lit (LAtom (Atom "tracer"))))) [])))
                          (Exp (Constr (Let (["_cor1"],Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                                                             Exp (Constr (Lit (LAtom (Atom "self"))))) [])))
                                         (Exp (Constr (Seq (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "dbg")))),
                                                                                  Exp (Constr (Lit (LAtom (Atom "p"))))) [Exp (Constr (Var "_cor1")),
                                                                                                                          Exp (Constr (Lit (LAtom (Atom "c"))))])))
                                                        (Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "dbg")))),Exp (Constr (Lit (LAtom (Atom "tpl")))))
                                                                      [Exp (Constr (Lit (LAtom (Atom "example")))),
                                                                       Exp (Constr (List (L [Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "_")))),
                                                                                                                 Exp (Constr (Lit LNil)),Exp (Constr (List (L [Exp (Constr (Tuple [Exp (Constr (Lit (LAtom (Atom "return_trace"))))]))])))]))])))]))))))))))))))
