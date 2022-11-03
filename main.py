# See https://github.com/rebcabin/rebcabin.github.io/blob/main/YCombinator005.pdf
# to see how this works, particularly, to see what the type SQRT_FI2I means.

from typing import Callable, Dict, Tuple

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

# See PyCharm help at https://www.jetbrains.com/help/pycharm/
