import numpy
import pytest

from pprint import pprint

from debugging import (
    APPLY,
    Application,
    BLOCK,
    DEFINE,
    DO,
    ECHO,
    EVAL,
    Environment,
    IllegalArgumentsError,
    LABELS,
    LET,
    LETREC,
    LET_STAR,
    LOOP2,
    LOOP3,
    LOOP5,
    Procedure,
    SET_BANG,
    Var,
    ΓΠ,
    Λ,
    Ξ,
)


class TestEnvironment:

    def test_ΓΠ(self):
        # pprint({'ΓΠ': ΓΠ, 'ΓΠ.ϕ': ΓΠ.ϕ.__dict__})
        assert ΓΠ.π is None
        assert ΓΠ._is_global()
        assert not ΓΠ._is_empty()

    def test_setattr(self):
        setattr(ΓΠ.ϕ, 'γόὂ', 43)
        # pprint({'ΓΠ': ΓΠ, 'ΓΠ.ϕ': ΓΠ.ϕ.__dict__})
        assert ΓΠ.γόὂ == 43
        del ΓΠ.ϕ.γόὂ


class TestProcedure:

    def test_set_square(self):
        setattr(  # Bind a variable ...
            ΓΠ.ϕ,  # ... in the frame of the global environment ...
            "square",  # ... a variable named "square" ...
            Procedure(  # ... to this Schemulator procedure.
                {"body": lambda π: π.x * π.x,  # no free vars
                 "parameters": ['x']}))  # Don't forget the parameter list!
        assert ΓΠ.square is not None
        del ΓΠ.ϕ.square

    def test_square(self, square):
        assert ΓΠ.square(5) == 25
        with pytest.raises(IllegalArgumentsError):
            assert ΓΠ.square(5, 6) == (5, 6)

    def test_dupes(self):
        with pytest.raises(ValueError):
            setattr(
                ΓΠ.ϕ,
                "square",
                Procedure(
                    {"body": lambda π: π.x * π.y,
                     "parameters": ['x', 'x']}))  # whoops

    def test_Λ(self):
        # not worth a fixture:
        setattr(  # Give a name ...
            ΓΠ.ϕ,  # ... in the frame of the global environment ...
            "square_Λ",
            Λ(lambda π: π.x * π.x, ['x']))  # ... to this anonymous procedure
        assert ΓΠ.square_Λ(5) == 25
        with pytest.raises(IllegalArgumentsError):
            assert ΓΠ.square_Λ(5, 6) == (5, 6)
        del ΓΠ.ϕ.square_Λ


class TestEval:

    def test_var_lookup(self, γόὂ):
        assert 43 == EVAL(Var('γόὂ'))

    def test_string_literal(self):
        assert 'γόὂ' == EVAL('γόὂ')

    def test_free_vars(self):
        assert 1806 == \
               Λ(lambda πy:
                 Λ(lambda π: π.x * π.y, ['x'], πy)(42),
                 ['y'])(43)


@pytest.fixture
def square():
    DEFINE('square', Λ(lambda π: π.x * π.x, ['x']))
    yield None
    del ΓΠ.ϕ.square


class TestApply:

    def test_squares(self, square):
        assert 1764 == APPLY(ΓΠ.square, [42])

    def test_call_syntax(self, square):
        assert 1764 == ΓΠ.square(42)

    def test_anonym(self):
        assert 1764 == Λ(lambda π: π.x * π.x, ['x'])(42)

    def test_multi(self):
        assert 56 == Λ(lambda π: π.x * π.y, ['x', 'y'])(8, 7)

    def test_leakage_x(self):
        with pytest.raises(NameError):
            ΓΠ.x


@pytest.fixture
def γόὂ():
    yield setattr(ΓΠ.ϕ, 'γόὂ', 43)
    del ΓΠ.ϕ.γόὂ
    pass


class TestApplication:

    def test_definition(self, square):
        ωfoo = Application(ΓΠ.square, [42])
        assert ωfoo is not None

    def test_evaluation(self, square):
        ωfoo = Application(ΓΠ.square, [42])
        assert 1764 == EVAL(ωfoo)

    def test_Ξ(self, square):
        assert 1764 == EVAL(Ξ('square', [42]))

    def test_Var(self, square, γόὂ):
        assert 1849 == \
               EVAL(
                   Ξ('square',  # find proc in global env
                     [Var('γόὂ')]))  # find binding in global env

    def test_in_body(self, square):
        assert 1849 == ΓΠ.square(43)
        assert 1849 == \
               Λ(lambda π:
                 ΓΠ.square(π.foobar),
                 ['foobar'])(43)
        assert 1849 == \
               Λ(lambda π:
                 EVAL(Ξ(ΓΠ.square, [Var('ϕοοβαρ')]),
                      π),
                 ['ϕοοβαρ'])(43)

    def test_forgetting_π(self, square):
        with pytest.raises(NameError):
            r = Λ(lambda π:
                  EVAL(Ξ(ΓΠ.square, [Var('ϕοοβαρ')])
                       ),  # whoops
                  ['ϕοοβαρ'])(43)
            assert r == 1849

    def test_forgetting_Var(self, square):
        with pytest.raises(TypeError):
            r = Λ(lambda π:
                  EVAL(Ξ(ΓΠ.square,
                         ['ϕοοβαρ']),  # whoops
                       π),
                  ['ϕοοβαρ'])(43)
            assert 1849 == r

    def test_forgetting_EVAL(self, square):
        with pytest.raises(AssertionError):
            r = Λ(lambda π:
                  Ξ(ΓΠ.square, [Var('ϕοοβαρ')], π),  # whoops
                  ['ϕοοβαρ'])(43)
            assert r == 1849

    def test_explicit_Λ(self, γόὂ):
        r = EVAL(Ξ(Λ(lambda π: π.x * π.x, ['x']),
                   [Var('γόὂ')]))  # find binding in global env
        assert 1849 == r

    def test_explicit_Λ_2(self, γόὂ):
        r = EVAL(Ξ(Λ(lambda π: π.x * π.x, ['x']),
                   [43]))  # find binding in global env
        assert 1849 == r

    def test_invocation_styles(self):
        r = EVAL(
            Ξ(Λ(lambda π: π.x, ['x']), [42])
        )
        assert 42 == r
        r = EVAL(
            Ξ(Λ(lambda π0:
                Λ(lambda π: π.x, ['x']))))
        assert 42 == r(42)

    def test_sub_environments(self, square):
        r = Λ(lambda π:
              EVAL(Ξ('square', [Var('ϕοοβαρ')]), π),
              ['ϕοοβαρ'])(42)
        assert 1764 == r

    def test_leakage(self, square):
        with pytest.raises(NameError):
            r = Λ(lambda π:
                  EVAL(Ξ('square',
                         [Var('ϕοοβαρ')]),
                       ΓΠ),  # wrong env
                  ['ϕοοβαρ'])(42)
            assert r == 1849


@pytest.fixture
def saxpy():
    DEFINE(
        'saxpy',
        Λ(lambda π:
          numpy.dot(π.a, π.x) \
              if isinstance(π.a, numpy.ndarray)
                 and isinstance(π.x, numpy.ndarray) \
              else π.a * π.x + π.y,
          ['a', 'x', 'y']))
    yield None
    del ΓΠ.ϕ.saxpy


@pytest.fixture
def fact_iter_0():
    DEFINE('fact_iter_0',
           Λ(lambda π: π.product if π.counter > π.max_count else \
               π.fact_iter_0(
                   π.counter * π.product,
                   π.counter + 1,
                   π.max_count
               ), ['product', 'counter', 'max_count']));
    yield None
    del ΓΠ.ϕ.fact_iter_0


class TestDefine:

    def test_fancy(self, square):
        import numpy
        assert numpy.all(
            numpy.array([[9, 16], [1, 4]]) -
            ΓΠ.square(numpy.array([[3, 4], [1, 2]]))
            == 0)

    def test_saxpy(self, saxpy):
        assert numpy.all(
            numpy.array([[68], [161]]) -
            ΓΠ.saxpy(
                numpy.array([[1, 2, 3], [4, 5, 6]]),
                numpy.array([[7], [11], [13]]),
                numpy.array([[42], [43]]))
            == 0)

    def test_recursion(self, fact_iter_0):
        assert 720 == ΓΠ.fact_iter_0(1, 1, 6)


class TestFirstClassness:

    def test_procs_as_args(self, square):
        # Outer parens necessary to break lines for comments (Python syntax booger).
        r = (Λ(lambda π:  # Create environment E1 in ΓΠ
               π.f(π.x),  # Apply E1.f to E1.x.
               ['f', 'x'])  # parameters
             (ΓΠ.square, 42))  # <~~~ Bind f to square, x to 42.
        assert r == 1764

    def test_anon_sibling(self):
        r = Λ(lambda π:  # Create environment E1 in ΓΠ.
              Λ(lambda π:  # Create environment E2 in ΓΠ.
                π.n * π.n,  # <~~~ n is bound in E2.
                ['n']  # (E2 is sibling to E1)
                )  # Parent environment implicitly ΓΠ.
              (π.m),  # <~~~ Look up m in E1, bind to n in E2.
              ['m'])(42)  # <~~~ Bind m to 42 in E1.
        assert r == 1764

    def test_non_shadowing(self):
        r = Λ(lambda π:  # Create environment E1 in ΓΠ.
              Λ(lambda π:  # Create environment E2 in ΓΠ.
                π.n * π.n,  # <~~~ n is bound in E2.
                ['n']  # (E2 is sibling to E1)
                )  # Parent environment implicitly ΓΠ.
              # DIFFERENT ............................
              (π.n),  # <~~~ Look up n in E1, bind to n in E2.
              ['n'])(42)  # <~~~ a different n bound to 42 in E1.
        assert r == 1764

    def test_anon_child(self):
        r = Λ(lambda π:  # Create environment E1 in ΓΠ.
              Λ(lambda π:  # Create environment E2 in E1 !!!!
                π.x * π.n,  # <~~~ n in E1, x in E2.
                ['x'],  # (E2 is child of E1, written E1<--E2)
                π)  # Parent environment *explicitly* E1.
              (π.n),  # <~~~ Look up n in E1, bind x in E1<--E2
              ['n'])(42)  # <~~~ Bind n to 42 in E1
        assert r == 1764

    def test_returned_proc(self, square):
        """Calling parens must start on same line as callee!"""
        r = Λ(lambda π:  # Create environment E1 in ΓΠ.
              π.f,  # Just return the value of parameter f.
              ['f'])(
            ΓΠ.square)(
            42)  # Apply the returned procedure.
        assert r == 1764


class TestAnonRecursion:

    def test_square_of_function(self):
        r = Λ(lambda π:
              Λ(lambda π:
                # Observe the "multiplication" sf(sf):
                1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1),
                ['n'], π), ['sf'])(  # <~~~ apply to copy of itself
            Λ(lambda π:  # <~~~ this gets bound to 'sf'
              Λ(lambda π:
                1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1),
                ['n'], π), ['sf']))(6)
        assert r == 720

    def test_delayed_square(self):
        r = Λ(lambda π:  # sf
              Λ(lambda π:  # f
                Λ(lambda π:  # n
                  1 if π.n < 1 else π.n * π.f(π.n - 1),
                  ['n'], π),
                ['f'], π)(Λ(lambda π:  # m
                            π.sf(π.sf)(π.m), ['m'], π)),
              ['sf'])(  # <~~~ apply to copy of self
            Λ(lambda π:  # sf
              Λ(lambda π:  # f
                Λ(lambda π:  # n
                  1 if π.n < 1 else π.n * π.f(π.n - 1),
                  ['n'], π),
                ['f'], π)(Λ(lambda π:  # m
                            π.sf(π.sf)(π.m), ['m'], π)),
              ['sf']))(6)
        assert 720 == r

    def test_abstracted_domain_code(self):
        r = Λ(lambda π:  # d
              Λ(lambda π:  # sf
                π.d(Λ(lambda π: π.sf(π.sf)(π.m),
                      ['m'], π)),
                ['sf'], π)(  # <~~~ "squaring," i.e., self-application
                  Λ(lambda π:  # sf
                    π.d(Λ(lambda π: π.sf(π.sf)(π.m),
                          ['m'], π)),
                    ['sf'], π)),
              ['d'])(  # domain code
            Λ(lambda π:  # f
              Λ(lambda π:  # n -- busines code
                1 if π.n < 1 else π.n * π.f(π.n - 1),
                ['n'], π),
              ['f'])
        )(6)
        assert r == 720

    def test_second_square(self):
        r = Λ(lambda π:  # function of domain code, d
              Λ(lambda π: π.g(π.g), ['g'], π)(
                  Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
                    ['sf'], π)),
              ['d'])(
            Λ(lambda π:  # domain code; function of business code, f
              Λ(lambda π:
                1 if π.n < 1 else π.n * π.f(π.n - 1),  # business code
                ['n'], π),  # business parameter, n
              ['f'])  # recursive function
        )(6)
        assert r == 720


@pytest.fixture
def fact_recursive():
    DEFINE('fact_recursive',
           Λ(lambda π:  # domain code; function of business code, f
             Λ(lambda π:
               1 if π.n < 1 else π.n * π.f(π.n - 1),  # business code
               ['n'], π),  # 1 business parameter, n
             ['f']))  # recursive function
    yield None
    del ΓΠ.ϕ.fact_recursive


@pytest.fixture
def fact_tc():
    DEFINE('fact_tc',
           Λ(lambda π:
             Λ(lambda π:
               π.a if π.n < 1 else π.f(π.n - 1, π.a * π.n),
               ['n', 'a'], π),
             ['f']))
    yield None
    del ΓΠ.ϕ.fact_tc


class TestΥ1:

    def test_recursive_factorial(self, fact_recursive):
        r = ΓΠ.Υ1(ΓΠ.fact_recursive)(6)
        assert r == 720

    def test_tail_recursive_factorial(self, fact_tc):
        r = LOOP2(ΓΠ.fact_tc)(6, 1)
        assert r == ΓΠ.Υ2(ΓΠ.fact_tc)(6, 1)


@pytest.fixture
def fact_iter():
    # λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
    DEFINE('fact_iter',  # domain code is a function of f ...
           Λ(lambda π:  # ... which is business code.
             Λ(lambda π:
               π.m
               if π.c > π.x
               else π.f(π.m * π.c, π.c + 1, π.x),  # business code
               ['m', 'c', 'x'], π),  # business parameters
             ['f']))
    yield None
    del ΓΠ.ϕ.fact_iter


class TestΥ3:

    def test_iterative_factorial(self, fact_iter):
        assert 720 == \
               ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 6)


@pytest.fixture
def fib_slow():
    DEFINE('fib_slow',
           Λ(lambda π:
             Λ(lambda π: 1 if π.n < 2 else
             π.f(π.n - 1) + π.f(π.n - 2), ['n'], π),
             ['f']))
    yield None
    del ΓΠ.ϕ.fib_slow


@pytest.fixture
def fib_iter():
    DEFINE('fib_iter',
           Λ(lambda π:
             Λ(lambda π: π.b if π.n < 1 else
             π.f(π.b, π.a + π.b, π.n - 1),
               ['a', 'b', 'n'], π),
             ['f']))
    yield None
    del ΓΠ.ϕ.fib_iter


@pytest.fixture
def fib_fast():
    DEFINE('fib_fast',
           Λ(lambda π:  # of f; level 1
             Λ(lambda π:  # of a; level 2
               Λ(lambda π:  # of n; level 3
                 (π.a, 1) if π.n < 2 else
                 Λ(lambda π:  # of n_1; level 4
                   (π.a, π.a[π.n_1])  # optimizer should remove these two lines
                   if π.n_1 in π.a else  # ^^^
                   Λ(lambda π:  # of fim1; level 5
                     Λ(lambda π:  # of m1; level 6
                       Λ(lambda π:  # of r1; level 7
                         Λ(lambda π:  # of a1; level 8
                           Λ(lambda π:  # of n_2; level 9
                             (π.a1, π.r1 + π.a1[π.n_2])  # <~~~ a quick exit
                             if π.n_2 in π.a1 else
                             Λ(lambda π:  # of fim2; level 10
                               Λ(lambda π:  # of m2; level 11
                                 Λ(lambda π:  # of r2; level 12
                                   Λ(lambda π:  # of a2; level 13
                                     (π.a2, π.r1 + π.r2),  # <~~~ the money line
                                     ['a2'], π)(π.m2[0] | {π.n_2: π.r2}),  # <~~~ update memo
                                   ['r2'], π)(π.m2[1]),  # unpack
                                 ['m2'], π)(π.fim2(π.n_2)),  # unpack
                               ['fim2'], π)(π.f(π.a1)),  # <~~~ recurse
                             ['n_2'], π)(π.n - 2),  # DRY
                           ['a1'], π)(π.m1[0] | {π.n_1: π.r1}),  # <~~~ update memo
                         ['r1'], π)(π.m1[1]),  # unpack
                       ['m1'], π)(π.fim1(π.n_1)),  # unpack
                     ['fim1'], π)(π.f(π.a)),  # <~~~ recurse
                   ['n_1'], π)(π.n - 1),  # DRY
                 ['n'], π),  # business parameter
               ['a'], π),  # curried memo
             ['f']))  # domain code
    yield None
    del ΓΠ.ϕ.fib_fast


@pytest.fixture
def fib_fast_uncurried():
    DEFINE('fib_fast_uncurried',
           Λ(lambda π:  # of f; level 1
             Λ(lambda π:  # of a, n; level 2
               (π.a, 1) if π.n < 2 else
               Λ(lambda π:  # of n_1; level 3
                 Λ(lambda π:  # of t1; level 4
                   Λ(lambda π:  # of m1; level 5
                     Λ(lambda π:  # of r1; level 6
                       Λ(lambda π:  # of a1; level 7
                         Λ(lambda π:  # of n_2; level 8
                           (π.a1, π.r1 + π.a1[π.n_2])  # <~~~ quick exit
                           if π.n_2 in π.a1 else
                           Λ(lambda π:  # of t_2; level 9
                             Λ(lambda π:  # of m_2; level 10
                               Λ(lambda π:  # of r_2; level 11
                                 Λ(lambda π:  # of a_2; level 12
                                   (π.a2, π.r1 + π.r2),  # <~~~ the money line
                                   ['a2'], π)(π.m2 | {π.n_2: π.r2}),  # <~~~ update memo
                                 ['r2'], π)(π.t2[1]),  # nupaci
                               ['m2'], π)(π.t2[0]),  # unpack
                             ['t2'], π)(π.f(π.a1, π.n_2)),  # <~~~ recurse
                           ['n_2'], π)(π.n - 2),  # dry
                         ['a1'], π)(π.m1 | {π.n_1: π.r1}),  # <~~~ update memo
                       ['r1'], π)(π.t1[1]),  # unpac
                     ['m1'], π)(π.t1[0]),  # unpack
                   ['t1'], π)(π.f(π.a, π.n_1)),  # <~~~ recurse
                 ['n_1'], π)(π.n - 1),  # DRY
               ['a', 'n'], π),  # busines parameters
             ['f']))  # domain-code signature
    yield None
    del ΓΠ.ϕ.fib_fast_uncurried


@pytest.fixture
def fib_tc_memo():
    DEFINE('fib_tc_memo',
           Λ(lambda π:
             Λ(lambda π:
               (π.a | {π.x: π.r2}, π.r2) if π.n < 1 else \
                   π.f(π.r2, π.r1 + π.r2,
                       π.a | {π.x - π.n: π.r2},
                       π.n - 1,
                       π.x),
               ['r1', 'r2', 'a', 'n', 'x'], π),
             ['f']))
    yield None
    del ΓΠ.ϕ.fib_tc_memo


class TestTailRecursion:

    def test_tail_fact_iter(self, fact_iter):
        assert 720 == \
               LOOP3(ΓΠ.fact_iter)(1, 1, 6)

    def test_blown_recursion(self, fact_iter, fact_iter_0):
        with pytest.raises(RecursionError):
            ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 400)
        with pytest.raises(RecursionError):
            ΓΠ.fact_iter_0(1, 1, 400)

    def test_safe_recursion(self, fact_iter):
        assert \
            LOOP3(ΓΠ.fact_iter)(1, 1, 400) \
            == 64034522846623895262347970319503005850702583026002959458684445942802397169186831436278478647463264676294350575035856810848298162883517435228961988646802997937341654150838162426461942352307046244325015114448670890662773914918117331955996440709549671345290477020322434911210797593280795101545372667251627877890009349763765710326350331533965349868386831339352024373788157786791506311858702618270169819740062983025308591298346162272304558339520759611505302236086810433297255194852674432232438669948422404232599805551610635942376961399231917134063858996537970147827206606320217379472010321356624613809077942304597360699567595836096158715129913822286578579549361617654480453222007825818400848436415591229454275384803558374518022675900061399560145595206127211192918105032491008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

    def test_fib_slow(self, fib_slow):
        assert 46_368 == \
               ΓΠ.Υ1(ΓΠ.fib_slow)(23)
        assert 13 == \
               ΓΠ.Υ1(ΓΠ.fib_slow)(6)

    def test_fib_iter(self, fib_iter):
        assert 46_368 == \
               LOOP3(ΓΠ.fib_iter)(0, 1, 23)

    def test_Υ2C(self, fib_fast):
        r = ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1]
        assert 46_368 == r

    def test_blown_recursion_2(self, fib_fast):
        with pytest.raises(RecursionError):
            _ = ΓΠ.Υ2C(ΓΠ.fib_fast)({})(200)[1]

    def test_blown_fib_fast_uncurried(self, fib_fast_uncurried):
        with pytest.raises(RecursionError):
            assert ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 400)[1] == \
                   453973694165307953197296969697410619233826

    def test_blown_fib_memo(self, fib_tc_memo):
        with pytest.raises(RecursionError):
            ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)

    def test_fib_memo(self, fib_tc_memo):
        assert 225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626 == \
               LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1]


@pytest.fixture
def x():
    DEFINE('x', 0)  # <~~~ in ΓΠ
    yield None
    del ΓΠ.ϕ.x


@pytest.fixture
def y():
    DEFINE('y', 42)  # <~~~ in ΓΠ
    yield None
    del ΓΠ.ϕ.y


class TestBlock:

    def test_no_x(self, x):
        r = BLOCK(
            Λ(lambda π: SET_BANG('x', 6, π)),
            Λ(lambda π: π.x * 7))
        assert r == 42

    def test_yes_y(self, x, y):
        r = BLOCK(
            Λ(lambda π: SET_BANG('x', 6, π)),
            Λ(lambda π: SET_BANG('x', π.x * 7, π)),
            Λ(lambda π: π.x * π.y))
        assert r == 1764

    def test_undefined_z(self, x):
        with pytest.raises(NameError):
            BLOCK(
                Λ(lambda π: SET_BANG('x', 6, π)),
                Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                Λ(lambda π: {'expect 0': π.x * π.y}),
                Λ(lambda π: π.z))  # <~~~ no binding

    def test_non_global_environment(self):
        r = Λ(lambda π0:  # <~~~ Make a non-global π.
              BLOCK(
                  Λ(lambda π: SET_BANG('x1', 7, π), π=π0),
                  Λ(lambda π: SET_BANG('y1', 6, π), π=π0),
                  Λ(lambda π: π.x1 * π.y1, π=π0),
                  π=π0),
              ['x1', 'y1'])(0, 0)
        assert r == 42

    def test_non_global_propagated(self):
        r = Λ(lambda π0:  # <~~~ Make a non-global π.
              BLOCK(
                  Λ(lambda π: SET_BANG('x1', 7, π)),  # implicit
                  Λ(lambda π: SET_BANG('y1', 6, π)),  # implicit
                  Λ(lambda π: π.x1 * π.y1),  # implicit
                  π=π0),  # necessary
              ['x1', 'y1'])(0, 0)
        assert r == 42

    def test_forget_non_global(self):
        with pytest.raises(NameError):
            Λ(lambda π0:  # <~~~ Make a non-global π.
              BLOCK(
                  Λ(lambda π: SET_BANG('x1', 7, π)),
                  Λ(lambda π: SET_BANG('y1', 6, π)),
                  Λ(lambda π: π.x1 * π.y1),
              ),  # <~~~ Whoops!
              ['x1', 'y1'])(0, 0)

    def test_no_leak(self):
        with pytest.raises(NameError):
            ΓΠ.x1
        with pytest.raises(NameError):
            ΓΠ.y1

    def test_not_none_returns(self, x, y):
        r = BLOCK(
            Λ(lambda π: SET_BANG('x', 6, π)),
            Λ(lambda π: SET_BANG('x', π.x * 7, π)),
            Λ(lambda π: π.x * π.y),
            Λ(lambda π: π.x * π.y))
        assert r == 1764

    def test_nested_envs_ignore_wrapper(self, x, y):
        r = BLOCK(
            Λ(lambda π: SET_BANG('x', 6, π)),
            Λ(lambda π0:  # <~~~ Don't forget to wrap it!
              BLOCK(Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                    Λ(lambda π: π.x * π.y))))  # ignore the wrapper
        assert 1764 == r

    def test_nested_envs(self, x, y):
        r = BLOCK(
            Λ(lambda π: SET_BANG('x', 6, π)),
            Λ(lambda π0:  # <~~~ Don't forget to wrap it!
              BLOCK(Λ(lambda π: SET_BANG('x', π0.x * 7, π0)),
                    Λ(lambda π: π0.x * π0.y))))
        assert 1764 == r


class TestLetStar:

    def test_depth_0_Ξ_no_vars(self):
        r = LET_STAR([], Ξ(Λ(lambda π: 43 * 42)))
        assert 1806 == r

    def test_depth_1_Ξ_no_free_vars(self, square):
        r = LET_STAR([('z', 42)],
                     Ξ(ΓΠ.square, [Var('z')]))
        assert 1764 == r

    def test_depth_1_Ξ_free_var(self):
        r = LET_STAR([('z', 42)],
                     Ξ(Λ(lambda π: 43 * π.z)))
        assert r == 1806

    def test_depth_1_Λ_free_var(self):
        """Test monkey-patching of procedures."""
        ρ = LET_STAR([('z', 42)],
                     Λ(lambda π: 43 * π.z))
        r = ρ()
        assert r == 1806

    def test_depth_2_Λ_free_var(self):
        """Test monkey-patching of procedures."""
        ρ = LET_STAR([('z', 42),
                      ('y', 43)],
                     Λ(lambda π: π.y * π.z))
        r = ρ()
        assert r == 1806

    def test_depth_1_Λ_free_var_too_early(self):
        """Difficult to chain Λ envs without macros.
        The Λ is evaluated before let*'s env exists.
        Instead of macros, we have Ξ applications."""
        with pytest.raises(NameError):
            r = LET_STAR([('z', 42)],
                         Λ(lambda π: 43 * π.z)())
            assert r == 1806

    def test_depth_2_Ξ_free_vars(self):
        """Instead of macros, we have Ξ applications."""
        r = LET_STAR([('z', 42),
                      ('y', 43)],
                     Ξ(Λ(lambda π: π.z * π.y)))
        assert 1806 == r

    def test_depth_3_Ξ_free_vars(self):
        """Instead of macros, we have Ξ applications."""
        r = LET_STAR([('z', 42),
                      ('y', Ξ(Λ(lambda π: π.z + 1))),
                      ('w', Ξ(Λ(lambda π: π.z * π.y)))],
                     body=Ξ(Λ(lambda π: π.w)))
        assert 1806 == r

    def test_depth_3_Ξ_params(self):
        r = LET_STAR([('z', 42),
                      ('y', Ξ(Λ(lambda π: π.z + 1, ['z']),
                              [Var('z')])),
                      ('w', Ξ(Λ(lambda π: π.z * π.y, ['z', 'y']),
                              [Var('z'), Var('y')])
                       )],
                     body=Ξ(Λ(lambda π: π.w)))
        assert 1806 == r

    def test_procs_as_vars(self):
        r = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
                     body=Ξ(Λ(lambda π: π.g(42))))
        assert r == 1764

    def test_no_leak(self):
        with pytest.raises(NameError):
            _ = ΓΠ.g

    def test_bound_procs(self):
        r = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
                     body=Ξ(Λ(lambda π: π.gg(42), ['gg']),
                            [Var('g')]))
        assert 1764 == r


class TestLet:

    def test_depth_0(self):
        assert 1806 == \
               LET([],
                   Ξ(Λ(lambda π: 43 * 42)))

    def test_depth_1(self, square):
        assert 1764 == \
               LET([('z', 42)],
                   Ξ(ΓΠ.square, [Var('z')]))

    def test_depth_2(self):
        assert 1806 == \
               LET([('z', 42),
                    ('y', 43)],
                   Ξ(Λ(lambda π: π.z * π.y)))

    def test_depth_2_reversed(self):
        assert 1806 == \
               LET([('y', 42),
                    ('z', 43)],
                   Ξ(Λ(lambda π: π.z * π.y)))

    def test_no_downward_leakage(self):
        DEFINE('y', 0)
        assert 42 == \
               LET([('y', 42),
                    ('z', Ξ(Λ(lambda π: π.y + 1)))],  # Outer y = 0, not inner y = 42
                   Ξ(Λ(lambda π: π.z * π.y)))  # Inner y = 42 * inner z = 1

    def test_no_downward_leakage_reversed(self, y):
        SET_BANG('y', 0)
        assert 42 == \
               LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
                    ('y', 42)],
                   Ξ(Λ(lambda π: π.z * π.y)))  # Inner y = 42 * inner z = 1

    def test_nested(self):
        assert 1806 == \
               LET([('z0', 42)],
                   Ξ(Λ(lambda π:
                       LET([('y0', Ξ(Λ(lambda π: π.z0 + 1)))],
                           Ξ(Λ(lambda π: π.z0 * π.y0)),
                           π=π))))  # <~~~ Don't forget to chain!

    def test_del(self):
        try:
            del ΓΠ.ϕ.x
        except:
            pass
        assert 1764 == \
               LET([('x', 42)],
                   Ξ(Λ(lambda π: π.x * π.x)))

    def test_local_to_bound(self):
        assert 1764 == \
               LET([('x', 42)],
                   Ξ(Λ(lambda π: π.y * π.y, ['y']),
                     [Var('x')]))


@pytest.fixture
def bogue():
    yield [42, 43, 42, 43, 42, 43]


class TestCollections:

    def test_lists_with_free_vars(self, bogue):
        assert bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.x * π.y)))

    def test_lists_with_bound_vars(self, bogue):
        assert bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.xx * π.yy,
                       ['xx', 'yy']),
                     [Var('x'), Var('y')]))

    def test_tuples(self, bogue):
        assert tuple(bogue) == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: (π.forty_two, π.forty_two + 1)))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.x * π.y)))

    def test_dictionaries(self, bogue):
        assert bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: {'π.forty_two': π.forty_two,
                                        'forty-three': π.forty_two + 1}))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: [π.x['π.forty_two'],
                                  π.x['forty-three']] * π.y)))

    def test_numpy_array(self):
        assert numpy.all(
            numpy.array([126, 129]) ==
            LET_STAR([
                ('forty_two', 42),
                ('x', Ξ(Λ(lambda π: numpy.array(
                    [π.forty_two,
                     π.forty_two + 1])))),
                ('y', 3)],
                Ξ(Λ(lambda π: π.x * π.y))))


@pytest.fixture
def racket_bindings():
    yield [('is_even',
            Λ(lambda π: True if π.n == 0 else (not π.is_odd(π.n)),
              ['n'])),
           ('is_odd',
            Λ(lambda π: π.n != 0 and π.is_even(abs(π.n) - 1),
              ['n']))]


@pytest.fixture
def prime_bindings():
    yield [('x7', Λ(lambda π: π.x3() + π.x4())),
           ('x6', Λ(lambda π: π.x2() * π.x3())),
           ('x5', Λ(lambda π: π.x2() + π.x3())),
           ('x4', Λ(lambda π: π.square(π.x2()))),
           ('x3', Λ(lambda π: π.x1() * (π.x2() + π.x1()))),
           ('x2', Λ(lambda π: π.x1() + π.x1())),
           ('x1', Λ(lambda π: 1)), ]


class TestLetRec:

    def test_fact_81_bound_var(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.z1(π.m - 1, π.m * π.a, π.z1)),
                          ['m', 'a', 'z1']))],
                      Ξ(Λ(lambda π: π.fact(6, 1, π.fact))))

    def test_fact_82_same_name(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a, π.fact)),
                          ['m', 'a', 'fact']))],
                      Ξ(Λ(lambda π: π.fact(6, 1, π.fact))))

    def test_fact_83_no_application(self):
        assert isinstance(
            LETREC([('fact',
                     Λ(lambda π:
                       (π.a
                        if π.m <= 0
                        else π.fact(π.m - 1, π.m * π.a, π.fact)),
                       ['m', 'a', 'fact']))],
                   Λ(lambda π: π.fact(6, 1, π.fact))),
            Procedure)

    def test_fact_84_unrolled(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a, π.fact)),
                          ['m', 'a', 'fact']))],
                      Ξ(Λ(lambda π: π.fact(π.n, π.b, π.f), ['n', 'b', 'f']),
                        [6, 1, Var('fact')]))

    def test_85_codependence(self):
        assert 1806 == \
               LETREC([('z0', Λ(lambda π: 1 + π.y0(), ['y0'])),
                       ('y0', Λ(lambda π: 42))],
                      Ξ(Λ(lambda π: π.y0() * π.z0(π.y0))))

    def test_86_binding_levels(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π1:
                          (π1.a
                           if π1.m <= 0
                           else π1.f(π1.m - 1, π1.m * π1.a, π1.f)),
                          ['m', 'a', 'f']))],
                      Ξ(Λ(lambda π0: π0.fact(6, 1, π0.fact))))

    def test_87_no_leaks(self):
        with pytest.raises(NameError):
            _ = ΓΠ.y0

        with pytest.raises(NameError):
            _ = ΓΠ.z0

        with pytest.raises(NameError):
            _ = ΓΠ.fact

    def test_88_letrec_fact_with_freevars(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           # 'fact' is free here:
                           else π.fact(π.m - 1, π.m * π.a)),
                          ['m', 'a']))],
                      Ξ(Λ(lambda π: π.fact(6, 1))))

    def test_89_letrec_fact_procedure(self):
        assert isinstance(
            LETREC([('fact',
                     Λ(lambda π:
                       (π.a
                        if π.m <= 0
                        else π.fact(π.m - 1, π.m * π.a)),
                       ['m', 'a']))],
                   Λ(lambda π: π.fact(6, 1))),
            Procedure)

    def test_90_letrec_fact_proc_eval(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a)),
                          ['m', 'a']))],
                      # procedure is returned: call it
                      Λ(lambda π: π.fact(6, 1)))()

    def test_91_letrec_unrolled_freevars(self):
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a)),
                          ['m', 'a']))],
                      Ξ(Λ(lambda π: π.fact(π.n, π.b),
                          ['n', 'b']),  # <~~~ formal parameters
                        [6, 1]))  # <~~~ actual arguments

    def test_92_still_no_leaks(self):
        with pytest.raises(NameError):
            _ = ΓΠ.y0

        with pytest.raises(NameError):
            _ = ΓΠ.z0

        with pytest.raises(NameError):
            _ = ΓΠ.fact

    def test_93_racket_example(self, racket_bindings):
        assert (True, False, False, True) == \
               LETREC(racket_bindings,
                      Ξ(Λ(lambda π: (
                          π.is_even(42),
                          π.is_even(-43),
                          π.is_odd(-42),
                          π.is_odd(43),
                      ))))

    def test_letrec_no_procs(self):
        """LETREC acts like LET in many cases."""
        r = LETREC([('x', 6),
                    ('y', 7)],
                   Ξ(Λ(lambda π: π.x * π.y)))
        assert r == 42

    def test_letrec_scramble(self,
                             square,
                             fact_recursive,
                             prime_bindings):
        r = LETREC(prime_bindings,
                   Ξ(Λ(lambda π: π.x6() * π.x7())))
        assert r == 42

    def test_wilsons_theorem(self,
                             square,
                             wilsons,
                             prime_bindings):
        r = LETREC(prime_bindings,
                   Ξ(Λ(lambda π:
                       π.wilsons(π.x7())
                       and not π.wilsons(π.x6())
                       and π.wilsons(π.x5())
                       and not π.wilsons(π.x4())
                       and π.wilsons(π.x3())
                       and π.wilsons(π.x2())
                       )))
        assert r

    def test_wilsons_let_theorem(self,
                                 square,
                                 wilsons_let,
                                 prime_bindings):
        r = LETREC(prime_bindings,
                   Ξ(Λ(lambda π:
                       π.wilsons_let(π.x7())
                       and not π.wilsons_let(π.x6())
                       and π.wilsons_let(π.x5())
                       and not π.wilsons_let(π.x4())
                       and π.wilsons_let(π.x3())
                       and π.wilsons_let(π.x2())
                       )))
        assert r


@pytest.fixture
def wilsons(fact_tc):
    DEFINE('wilsons',
           Λ(lambda π:
             (LOOP2(π.fact_tc)(π.p - 1, 1) % π.p)
             == ((π.p - 1) % π.p),
             ['p']))
    yield None
    del ΓΠ.ϕ.wilsons


@pytest.fixture
def wilsons_let(fact_tc):
    """let-under-lambda idiom"""
    DEFINE(
        'wilsons_let',
        Λ(lambda π0:
          LET([('p_1', π0.p - 1)],
              Ξ(Λ(lambda π:
                  (LOOP2(π.fact_tc)(π.p_1, 1) % π.p)
                  == (π.p_1 % π.p))),
              π0),  # <~~~ don't forget to chain
          ['p']))
    yield None
    del ΓΠ.ϕ.wilsons_let


class TestLetLambda:
    def test_let_under_lambda(self):
        q = Ξ(Λ(lambda π0:  # for this λ ...
                LET([('p', False)],
                    Ξ(Λ(lambda π: π.p0 or π.p))),
                ['p0']),  # <~~~ formal parameter
              [True])  # <~~~ actual argument
        assert q

    def test_let_over_lambda(self):
        q = LET([('p',
                  Ξ(Λ(lambda π: True)))],
                Ξ(Λ(lambda π: π.p)))
        assert q


@pytest.fixture
def fact_iter_fixt():
    yield Λ(lambda π:
            (π.a
             if π.m <= 0
             else π.fact_iter_nom(π.m - 1, π.a * π.m)),
            ['m', 'a'])


class TestLabels:

    def test_94_test_fact(self, fact_iter_fixt):
        r = LABELS([('fact_iter_nom', fact_iter_fixt)],
                   Ξ(Λ(lambda π:
                       π.fact_iter_nom(6, 1))))
        assert 720 == r

    def test_94_2_test_fact(self, fact_iter_fixt):
        r = LABELS([('fact_iter_nom', fact_iter_fixt)],
                   Ξ('fact_iter_nom',
                     [6, 1]))
        assert 720 == r

    def test_94_3_test_fact(self, fact_iter_fixt):
        r = LABELS([('fact_iter_nom', fact_iter_fixt)],
                   Ξ(Λ(lambda π:
                       π.fact_iter_nom(π.m, π.a),
                       ['m', 'a']),
                     [6, 1]))
        assert 720 == r

    def test_94_4_test_fact(self, fact_iter_fixt):
        r = LABELS([('fact_iter_nom', fact_iter_fixt)],
                   Ξ(Λ(lambda π:
                       π.fact_iter_nom)))(
            6, 1)
        assert 720 == r

    def test_95_test_racket(self, racket_bindings):
        r = LABELS(racket_bindings,
                   Ξ(Λ(lambda π: (
                       π.is_even(42),
                       π.is_even(-43),
                       π.is_odd(-42),
                       π.is_odd(43),
                   ))))
        assert (True, False, False, True) == r


@pytest.fixture
def fact_iter_patched():
    yield Λ(lambda π:
            (π.a
             if π.m <= π.m0  # <~~~ Watch out!
             else π.fact(π.m - 1, π.m * π.a)),
            ['m', 'a'])


class TestPatching:

    def test_96_patched_environments(self, fact_iter_patched):
        assert 720 == \
               Λ(lambda π:
                 LETREC([('fact', fact_iter_patched)],
                        Ξ(Λ(lambda π: π.fact(6, 1))),
                        π),  # <~~~ Access m0.
                 ['m0'])(0)  # <~~~ works with 0

    def test_patching_again(self, fact_iter_patched):
        assert 720 == \
               Λ(lambda πo:  # <~~~ name change
                 LABELS([('fact', fact_iter_patched)],
                        Ξ(Λ(lambda π: π.fact(6, 1))),
                        πo),  # <~~~ name change
                 ['m0'])(1)  # <~~~ Works with 1, also.


class TestDo:

    def test_do(self):
        r = Λ(lambda πo:
              DO([('m', 6, Λ(lambda π: π.m - 1)),
                  ('a', 1, Λ(lambda π: π.a * π.m))],
                 pred=Λ(lambda π: π.m <= π.m0),
                 value=Λ(lambda π: π.a),
                 body=Λ(lambda π: None),
                 π=πo),
              ['m0'])(1)
        assert r == 720
