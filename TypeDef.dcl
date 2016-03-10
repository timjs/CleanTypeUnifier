definition module TypeDef

from Data.Maybe import :: Maybe

:: Type = Type String [TypeVar]
        | List ListKind Type SpineStrictness
        | Tuple [(Strict, Type)]
        | Array ArrayKind Type
        | Func [Type] Type ClassContext //TODO UnqTypeUnEqualities
        | Var TypeVar
        | Uniq Type

:: TypeVar :== String
:: TypeVarAssignment :== (TypeVar, Type)

:: ListKind = HeadStrict | HeadStrictUnboxed | NormalList
:: SpineStrictness = SpineStrict | NormalSpine
:: Strict = Strict | NotStrict
:: ArrayKind = ArrayStrict | ArrayUnboxed | NormalArray
:: ClassContext :== [ClassRestriction]
:: ClassRestriction :== (ClassOrGeneric, TypeVar)
:: ClassOrGeneric = Class String | Generic //TODO generic?

class unify a :: a a -> Maybe [TypeVarAssignment]
instance unify Type

assign :: TypeVarAssignment Type -> Type // replace arg1 with arg2 in arg3
assignAll :: [TypeVarAssignment] Type -> Type

