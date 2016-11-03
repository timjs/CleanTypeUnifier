definition module TypeUnify

import TypeDef

from StdFunc import flip

from Control.Monad import class Applicative, class Monad, foldM
from Data.Functor import class Functor
from Data.Maybe import ::Maybe

prepare_unification :: !Bool /* True iff left */ !Type -> Type
finish_unification :: ![TVAssignment] -> Unifier
unify :: ![Instance] !Type !Type -> Maybe [TVAssignment]

assign :: !TVAssignment !Type -> Maybe Type
assignAll :== flip (foldM (flip assign)) // ([TVAssignment] Type -> Maybe Type)
