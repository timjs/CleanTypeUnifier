implementation module TypeDef

from StdOverloaded import class ==
from GenEq import generic gEq, ===

derive gEq ListKind, SpineStrictness, Strict, ArrayKind, ClassOrGeneric, Type

instance == Type where (==) a b = a === b

