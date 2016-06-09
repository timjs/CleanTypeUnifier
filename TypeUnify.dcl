definition module TypeUnify

import TypeDef

from Data.Maybe import ::Maybe

prepare_unification :: !Bool /* True iff left */ !Type -> Type
finish_unification :: ![TVAssignment] -> Unifier
unify :: ![Instance] !Type !Type -> Maybe [TVAssignment]
