# See https://github.com/rebcabin/rebcabin.github.io/blob/main/YCombinator005.pdf
# to see how this works, particularly, to see what the type SQRT_FI2I means.

from typing import Callable, Dict, Tuple
import types

# Types

FI2I = Callable[[int], int]  # int -> int
FI2I2FI2I = Callable[[FI2I], FI2I]  # (int -> int) -> (int -> int)
SQRT_FI2I = Callable[["SQRT_FI2I"], FI2I]  # *a -> (int -> int)


# Note that SQRT_FI2I is a SQRT_FI2I -> FI2I! They're the same type!


def self_apply(g: SQRT_FI2I) -> FI2I:
    """Square the square root of an int->int function by
    self-applying it."""
    result: FI2I = g(g)
    return result


def yc(d: FI2I2FI2I) -> FI2I:
    """I am the redoubtable Y Combinator of one parameter. Return
    a FI2I given a FI2I2FI2I, which is a FI2I -> FI2I."""

    def lsf(sf: SQRT_FI2I) -> FI2I:
        """Lambda of the square root of a FI2I. Return domain code d
        applied to a delayed square of the square root sf, d(delayed).
        My type is SQRT_FI2I -> FI2I, which is the same as
        SQRT_FI2I!"""

        def delayed(m: int) -> int:
            """My type is FI2I. Give me an int and I'll give you an int.
            I know all about sf because I'm also a closure over the
            environment that contains the parameter of lsf."""
            result_delay: int = (sf(sf))(m)
            return result_delay

        # d takes a FI2I and returns a FI2I
        result_lsf: FI2I = d(delayed)
        return result_lsf

    result_yc: FI2I = self_apply(lsf)
    return result_yc


def fully_typed(domain_code: FI2I2FI2I, k: int) -> int:
    """Set breakpoints under here, especially on 'result' variables,
     to see how it works. This is fully type-checked"""
    result_fully_typed = yc(domain_code)(k)
    return result_fully_typed


# from https://gist.github.com/divs1210/d218d4b747b08751b2a232260321cdeb
# (thank you kindly, divs1210!)

# Helpers
# =======
def _obj():
    """Dummy object"""
    return lambda: None


_FILLER = _obj()


# API
# ===
def Y(d):
    """Y combinator - makes recursive lambdas
    ex: Y(lambda fact:
          lambda n:
            1 if n < 2 else n * fact(n - 1))(5)
    gives: 120
    In the y's below, self application is
    explicitly modeled with a lambda g: g(g).
    """
    lsf = lambda sf: d(lambda n: sf(sf)(n))
    return lsf(lsf)


def COND(cond_body_pairs, _else=lambda: None):
    """Functional if-elif-...-else expression
    ex: COND((1==0, lambda: 'a',
              2==0, lambda: 'b',
              3==0, lambda: 'c'),
             _else= lambda: 'd')
    gives: 'd'
    Note: All conditions are evaluated immediately!
    For conditions that should be evaluated only
    when required, use IF.
    """
    if len(cond_body_pairs) == 0:
        return _else()

    cond, body = cond_body_pairs[:2]
    if cond:
        return body()
    else:
        return COND(cond_body_pairs[2:], _else)


def IF(cond, then, _else=lambda: None):
    """Functional if-then-else expression
    ex: IF(1==0, lambda: 'a',
           _else= lambda: 'b')
    gives: 'b'
    """
    return COND((cond, then), _else)


def LET(bindings, body, env=None):
    """Introduce local bindings.
    ex: LET(('a', 1,
             'b', 2),
            lambda o: [o.a, o.b])
    gives: [1, 2]
    Bindings down the chain can depend on
    the ones above them through a lambda.
    ex: LET(('a', 1,
             'b', lambda o: o.a + 1),
            lambda o: o.b)
    gives: 2
    """
    if len(bindings) == 0:
        return body(env)

    env = env or _obj()
    k, v = bindings[:2]
    if isinstance(v, types.FunctionType):
        v = v(env)

    setattr(env, k, v)
    # recurse: env now has prior binding
    return LET(bindings[2:], body, env)


def FOR(bindings, body, env=None):
    """Clojure-style List comprehension.
    ex: FOR(('a', range(2),
             'b', range(2)),
            lambda o: (o.a, o.b))
    gives: [(0, 0), (0, 1), (1, 0), (1, 1)]
    Bindings down the chain can depend on
    the ones above them through a lambda
    as in LET.
    Special bindings take lambdas as values
    and can be used any number of times:
      * ':LET' - Temporary bindings
      * ':IF' - don't produce a value if this
        returns a falsey value
      * ':WHILE' - break out of the innermost
        loop if this returns a falsey value
    """
    if len(bindings) == 0:
        tmp = body(env)
        return [] if tmp is _FILLER else [tmp]

    env = env or _obj()
    k, v = bindings[:2]
    if k == ':IF':
        cond = v(env)
        return FOR(bindings[2:],
                   lambda e: body(e) if cond else _FILLER,
                   env)
    elif k == ':LET':
        return LET(v,
                   lambda e: FOR(bindings[2:], body, e),
                   env)
    elif k == ':WHILE':
        if v(env):
            return FOR(bindings[2:], body, env)
        else:
            return []
    elif isinstance(v, types.FunctionType):
        v = v(env)

    res = []
    for x in v:
        setattr(env, k, x)
        res += FOR(bindings[2:], body, env)
        delattr(env, k)

    return res


# From https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative


# 1.1 Simple Recursion
#
# This section is obvious; no example needed. Just need to create
# 'LABELS', which is like 'letrec'

# To model closures, every lambda gets an env as its last arg,
# defaulting to None. Any number of material arguments can
# precede the env=None argument.

def LABELS(bindings, body, env=None):
    """Like letrec; mutually recursive. Symbols presumed to exist
    in 'env'. Monkey patching lets you refer to attributes that
    may not exist yet. Every value must be a lambda of any number
    of arguments and an env in the final slot. The lambdas may
    refer each other's vars. The lambdas are not evaluated now;
    that's why mutual recursion works."""
    if len(bindings) == 0:
        return body(env)
    vars = bindings[0::2]
    vals = bindings[1::2]
    env = env or _obj()
    # Sequential set!
    [setattr(env, k, v)
     for k, v in zip(vars, vals)]
    return body(env)


assert LABELS(
    # 'f' refers to g before g is defined
    ['f', lambda x, env=None: env.g(x, env) + x,
     'g', lambda x, env=None: x * x],
    # body:
    lambda env: env.f(6, env),
    env=None
) == 42

fact = lambda m, env=None: \
    LABELS(
        # bindings
        ['fact1', lambda m, ans, env:
        ans if m < 1 else
        env.fact1(m - 1, m * ans, env)],
        # body
        lambda env: env.fact1(m, 1, env),
        env=env
    )

assert 720 == fact(6)


# 1.2 Iteration


def DO(triples, pred, value, body, env=None):
    """(DO ((<var1> <init1> <step1>)
            (<var2> <init2> <step2>
            . . .
            (<varñ> <initñ> <stepñ))
            (<pred> <value>)
            <body>
            <env=None>).
    The <init>s and <step>s are evaluated in parallel, as with
    Scheme "let". They may refer to any variables in the env.
    See "dofact" for an example. """
    vars = triples[0::3]  # symbols in strings
    inits = triples[1::3]  # lambdas with env in last slot
    steps = triples[2::3]  # lambdas with env in last slot
    env = env or _obj()
    result = LABELS([  # bindings
        'DOLOOP', lambda DUMMYbody, DUMMYvars, env:
        # Here is what DOLOOP does:
        value(env) if pred(env) else  # recurse
        env.DOLOOP(
            DUMMYbody=body(env),
            # parallel update (not sequential!)
            DUMMYvars=LET(
                ['new_vals', [step(env) for step in steps]],
                lambda env: [setattr(env, var, val)
                             for var, val in zip(vars, env.new_vals)],
                env=env),  # end of the nearest LET
            env=env)],  # end of DOLOOP and LABELS bindings
        # body of LABELS
        lambda env: env.DOLOOP(
            DUMMYbody=None,
            # parallel initialization
            DUMMYvars=LET(
                ['init_vals', [init(env) for init in inits]],
                lambda env: [setattr(env, var, init(env))
                             for var, init in zip(vars, inits)],
                env=env),  # end of LET
            env=env),  # end of last DOLOOP
        env=env)  # end of LABELS
    return result


dofact = lambda m, env=None: \
    DO(['m', lambda env: m, lambda env: env.m - 1,
        'a', lambda env: 1, lambda env: env.m * env.a],
       pred=lambda env: env.m <= 1,
       value=lambda env: env.a,
       body=lambda env: None,
       env=env)

print({'dofact(6)': dofact(6)})
assert 720 == dofact(6)


# 2.1 Compound Statements (Sequencing)

class IllegalArgumentError(ValueError):
    pass


def BLOCK(stmts, env=None):
    """
    ex: LET(['x', 0],
             BLOCK([lambda e: e.x = 6, lambda e: 7 * e.x]),
             env)
    gives: 42
    """
    if len(stmts) < 2:
        raise IllegalArgumentError("A BLOCK must have at least two statements.")

    env = env or _obj()

    def BLOCK2(s1, s2, env):
        v1 = s1(env)
        result = ((lambda _, env: s2(env))(v1, env))
        return result

    result = BLOCK2(stmts[-2], stmts[-1], env)
    return result


print({"LET(['x', 6], lambda e: e.x)": LET(['x', 6], lambda e: e.x)})
print({"BLOCK[x = 6, 7 * x]":
    BLOCK([
        lambda e: LET(['x', 6], lambda e: e.x, e),
        lambda e: 7 * e.x])})

# Tests
# =====
# LET form
assert LET(('a', 2,
            'b', lambda o: o.a * 3),
           lambda o: o.b - 1) == 5

# Y combinator (recursive lambda) and IF form
assert Y(lambda fact:
         lambda n:
         IF(n < 2, lambda: 1,
            _else=lambda: n * fact(n - 1)))(5) == 120

# FOR comprehension
assert FOR(('a', range(3)),
           lambda o: o.a + 1) == [1, 2, 3]

# Chained FOR comprehension
assert FOR(('a', range(3),
            ':IF', lambda o: o.a > 0,
            'b', lambda o: range(3 - o.a),
            ':LET', ('res', lambda o: [o.a, o.b]),
            ':WHILE', lambda o: o.a < 2),
           lambda o: o.res) == [
           # filtered a == 0
           [1, 0], [1, 1],
           # stopped at a == 2
       ]

if __name__ == '__main__':
    print({"no types 6!":
               ((lambda d:
                 (lambda g: g(g))
                 (lambda sf:
                  d(lambda m: (sf(sf))(m))))
                (lambda f:
                 (lambda n:
                  1 if n < 1 else n * f(n - 1))))(6)
           })


    def factorial_domain_code(factorial: FI2I) -> FI2I:
        """Apply a FI2I factorial and return a FI2I."""

        def fn(n: int) -> int:
            """My type is FI2I."""
            return 1 if n < 1 else n * factorial(n - 1)

        return fn


    print(f'fully typed 6! = {fully_typed(factorial_domain_code, 6)}')


    def fibonacci_slow_domain_code(fib_slow: FI2I) -> FI2I:
        def fn(n: int) -> int:
            return 1 if n < 2 else fib_slow(n - 1) + fib_slow(n - 2)

        return fn


    print(f'fully typed slow fib(20) = {fully_typed(fibonacci_slow_domain_code, 20)}')

    # All that was the 1-parameter Y. Let's do the 2-parameter Y for memoization
    # so we can have a fast Fibonacci.

    ASSOC = Dict[int, int]
    MEMO = Tuple[ASSOC, int]

    FIMEMO = Callable[[int], MEMO]
    FAFIMEMO = Callable[[ASSOC], FIMEMO]
    FAFIM2FAFIM = Callable[[FAFIMEMO], FAFIMEMO]
    SQRT_FAFIMEMO = Callable[["SQRT_FAFIMEMO"], FAFIMEMO]


    def ff(f: FAFIMEMO) -> FAFIMEMO:
        """My type is FAFIM2FAFIM."""

        def fa(a: ASSOC) -> FIMEMO:
            """My type is FAFIMEMO."""

            def fn(n: int) -> MEMO:
                """My type is FIMEMO."""
                if n < 2:
                    return a, 1
                else:
                    if n - 1 in a:
                        a1 = a
                        r1 = a[n - 1]
                    else:
                        a1, r1 = f(a)(n - 1)
                        a1[n - 1] = r1
                    if n - 2 in a1:
                        a2 = a1
                        r2 = a1[n - 2]
                    else:
                        a2, r2 = f(a1)(n - 2)
                        a2[n - 2] = r2
                    result_fn: MEMO = (a2, r1 + r2)
                    return result_fn

            result_fa: FIMEMO = fn
            return result_fa

        result_ff: FAFIMEMO = fa
        return result_ff


    ff_e = (lambda f:
            (lambda a:
             (lambda n:
              (a, 1) if n < 2 else
              ((lambda n_1:
                (a, a[n_1]) if n_1 in a else
                ((lambda fim1:
                  ((lambda m1:
                    ((lambda r1:
                      ((lambda a1:
                        ((lambda n_2:
                          (a1, r1 + a1[n_2]) if n_2 in a1 else
                          ((lambda fim2:
                            ((lambda m2:
                              ((lambda r2:
                                ((lambda a2:
                                  (a2, r1 + r2))
                                 (m2[0] | {n_2: r2})))
                               (m2[1])))
                             (fim2(n_2))))
                           (f(a1))))
                         (n - 2)))
                       (m1[0] | {n_1: r1})))
                     (m1[1])))
                   (fim1(n_1))))
                 (f(a))))
               (n - 1)))))


    def self_apply_2(g: SQRT_FAFIMEMO) -> FAFIMEMO:
        result_gg: FAFIMEMO = g(g)
        return result_gg


    def yc2(domain_code: FAFIM2FAFIM) -> FAFIMEMO:
        def lsf(sf: SQRT_FAFIMEMO) -> FAFIMEMO:
            def dmn(m: ASSOC) -> FIMEMO:
                """My type is FAFIMEMO."""

                def dn(n: int) -> MEMO:
                    """My type is FIMEMO."""
                    result_dn: MEMO = (sf(sf))(m)(n)
                    return result_dn

                result_dm: FIMEMO = dn
                return result_dm

            result_lsf: FAFIMEMO = domain_code(dmn)
            return result_lsf

        result_y2c: FAFIMEMO = self_apply_2(lsf)
        return result_y2c


    temp0: FAFIMEMO = yc2(ff)
    temp1: FIMEMO = temp0({})
    temp3: MEMO = temp1(400)
    temp4: int = temp3[1]

    print({"fully typed fast fib(400)": temp4})
    print({"fully typed fast fib(400)":
               yc2  # same as
               # (lambda d:
               #  (lambda g: g(g))
               #  (lambda sf:
               #   d(lambda m:
               #     (lambda n:
               #      (sf(sf))(m)(n)))))
               (ff)
               ({})(400)[1]  # [1] picks the integer from the MEMO.
           })
    import sys

    print({"max recursion limit": sys.getrecursionlimit()})
    print({"setting recursion limit to 2000": sys.setrecursionlimit(2000)})
    print({"untyped fast fib(400)":
               yc2(ff_e)({})(400)[1]})

    print({"pure expression, untyped fast fib(400) with no definitions":
               (lambda d:
                (lambda g: g(g))
                (lambda sf:
                 d(lambda m:
                   (lambda n:
                    (sf(sf))(m)(n)))))
               ((lambda f:
                 (lambda a:
                  (lambda n:
                   (a, 1) if n < 2 else
                   ((lambda n_1:
                     (a, a[n_1]) if n_1 in a else
                     ((lambda fim1:
                       ((lambda m1:
                         ((lambda r1:
                           ((lambda a1:
                             ((lambda n_2:
                               (a1, r1 + a1[n_2]) if n_2 in a1 else
                               ((lambda fim2:
                                 ((lambda m2:
                                   ((lambda r2:
                                     ((lambda a2:
                                       (a2, r1 + r2))
                                      (m2[0] | {n_2: r2})))
                                    (m2[1])))
                                  (fim2(n_2))))
                                (f(a1))))
                              (n - 2)))
                            (m1[0] | {n_1: r1})))
                          (m1[1])))
                        (fim1(n_1))))
                      (f(a))))
                    (n - 1))))))({})(400)[1]})

    sqrt = (lambda x, epsilon: 0)
# See PyCharm help at https://www.jetbrains.com/help/pycharm/
