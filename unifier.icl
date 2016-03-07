module unifier

import Type

Start = unify (List NormalList (Var "x") NormalSpine) (List NormalList (Type "Y" ["a"]) NormalSpine)
//Start = toString type
//where
//    //type = Function [Var "a", Var "b"] (Type "String" []) [(Class "toString", "a"), (Class "toString", "b")]
//    type = Function [] (Type "Int" []) []

