--higher-order functions
quadruple::(a->a)->a->a
quadruple f x=f(f(f(f x)))
