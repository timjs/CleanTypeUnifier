implementation module TypeDef

from StdOverloaded import class ==
from GenEq import generic gEq, ===

derive gEq ListKind, SpineStrictness, Strict, ArrayKind, ClassOrGeneric, Type,
    Instance

instance == Type where (==) a b = a === b
instance == Instance where (==) a b = a === b

