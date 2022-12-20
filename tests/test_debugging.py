import numpy
import pytest

from pprint import pprint

from debugging import (
    ΓΠ,
    Procedure,
    Λ,
    EVAL,
    Var,
    APPLY,
    IllegalArgumentsError,
    ECHO,
    Application,
    Ξ,
    DEFINE,
    LOOP3,
    LOOP5,
    BLOCK,
    SET_BANG,
    LET_STAR,
    LET,
    LETREC,
    LABELS)


class TestEnvironment:

    def test_ΓΠ(self):
        print("\n1...")
        pprint({'ΓΠ': ΓΠ, 'ΓΠ.ϕ': ΓΠ.ϕ.__dict__})
        assert ΓΠ.π is None

    def test_setattr(self):
        print("\n2...")
        setattr(ΓΠ.ϕ, 'γόὂ', 43)
        pprint({'ΓΠ': ΓΠ, 'ΓΠ.ϕ': ΓΠ.ϕ.__dict__})
        assert ΓΠ.γόὂ == 43

    def test_setattr_sticky(self):
        print("\n3...")
        pprint({'ΓΠ': ΓΠ, 'ΓΠ.ϕ': ΓΠ.ϕ.__dict__})
        assert ΓΠ.γόὂ == 43


class TestProcedure:

    def test_set_square(self):
        print("\n4...")
        setattr(  # Bind a variable ...
            ΓΠ.ϕ,  # ... in the frame of the global environment ...
            "square",  # ... a variable named "square" ...
            Procedure(  # ... to this Schemulator procedure.
                {"body": lambda π: π.x * π.x,
                 "parameters": ['x']}))  # Don't forget the parameter list!
        assert ΓΠ.square is not None

    def test_square(self):
        print("\n5...")
        # during TDD only, until EVAL and APPLY are defined
        # assert ΓΠ.square(5) == (5,)
        # assert ΓΠ.square(5, 6) == (5, 6)
        assert ΓΠ.square(5) == 25
        with pytest.raises(IllegalArgumentsError):
            assert ΓΠ.square(5, 6) == (5, 6)

    def test_dupes(self):
        print("\n6...")
        with pytest.raises(ValueError):
            setattr(
                ΓΠ.ϕ,
                "square",
                Procedure(
                    {"body": lambda π: π.x * π.y,
                     "parameters": ['x', 'x']}))

    def test_Λ(self):
        print("\n7...")
        setattr(  # Give a name ...
            ΓΠ.ϕ,  # ... in the frame of the global environment ...
            "square_Λ",
            Λ(lambda π: π.x * π.x, ['x']))  # ... to this anonymous procedure
        # during TDD only, until EVAL and APPLY are defined
        # assert ΓΠ.square_Λ(5) == (5,)
        # assert ΓΠ.square_Λ(5, 6) == (5, 6)
        assert ΓΠ.square_Λ(5) == 25
        with pytest.raises(IllegalArgumentsError):
            assert ΓΠ.square_Λ(5, 6) == (5, 6)


class TestEval:

    def test_var_lookup(self):
        print("\n8...")
        assert 43 == EVAL(Var('γόὂ'))

    def test_string_literal(self):
        print("\n9...")
        assert 'γόὂ' == EVAL('γόὂ')


class TestApply:

    def test_squares(self):
        print("\n10...")
        assert 1764 == APPLY(ΓΠ.square, [42])

    def test_call_syntax(self):
        print("\n11...")
        assert 1764 == ΓΠ.square(42)

    def test_anonym(self):
        print("\n12...")
        assert 1764 == Λ(lambda π: π.x * π.x, ['x'])(42)

    def test_multi(self):
        print("\n13...")
        assert 56 == Λ(lambda π: π.x * π.y, ['x', 'y'])(8, 7)

    def test_leakage_x(self):
        print("\n14...")
        with pytest.raises(NameError):
            ECHO('x', ΓΠ.x)


class TestApplication:

    def test_definition(self):
        print("\n15...")
        ωfoo = Application(ΓΠ.square, [42])
        assert ωfoo is not None

    def test_evaluation(self):
        print("\n16...")
        ωfoo = Application(ΓΠ.square, [42])
        assert 1764 == EVAL(ωfoo)

    def test_Ξ(self):
        print("\n17...")
        assert 1764 == EVAL(Ξ('square', [42]))

    def test_Var(self):
        print("\n18...")
        assert 1849 == \
               EVAL(
                   Ξ('square',  # find proc in global env
                     [Var('γόὂ')]))  # find binding in global env

    def test_in_body(self):
        print("\n19...")
        assert 1849 == ΓΠ.square(43)
        assert 1849 == \
               Λ(lambda π:
                 ΓΠ.square(π.foobar),
                 ['foobar'])(43)
        assert 1849 == \
               Λ(lambda π:
                 EVAL(Ξ(ΓΠ.square, [Var('ϕοοβαρ')]), π),
                 ['ϕοοβαρ'])(43)

    def test_forgetting_π(self):
        print("\n20...")
        with pytest.raises(NameError):
            assert 1849 == \
                   Λ(lambda π:
                     EVAL(Ξ(ΓΠ.square, [Var('ϕοοβαρ')])),
                     ['ϕοοβαρ'])(43)

    def test_forgetting_Var(self):
        print("\n21...")
        with pytest.raises(TypeError):
            assert 1849 == \
                   Λ(lambda π:
                     EVAL(Ξ(ΓΠ.square, ['ϕοοβαρ']), π),
                     ['ϕοοβαρ'])(43)

    def test_forgetting_EVAL(self):
        print("\n22...")
        with pytest.raises(AssertionError):
            assert 1849 == \
                   Λ(lambda π:
                     Ξ(ΓΠ.square, [Var('ϕοοβαρ')], π),
                     ['ϕοοβαρ'])(43)

    def test_explicit_Λ(self):
        print("\n23...")
        assert 1849 == \
               EVAL(Ξ(Λ(lambda π: π.x * π.x, ['x']),
                      [Var('γόὂ')]))  # find binding in global env

    def test_sub_environments(self):
        print("\n24...")
        assert 1764 == \
               Λ(lambda π:
                 EVAL(Ξ('square', [Var('ϕοοβαρ')]), π),
                 ['ϕοοβαρ'])(42)

    def test_leakage(self):
        print("\n25...")
        with pytest.raises(NameError):
            assert 1849 == \
                   Λ(lambda π:
                     EVAL(Ξ('square', [Var('ϕοοβαρ')]), ΓΠ),  # wong en
                     ['ϕοοβαρ'])(42)


class TestDefine:

    def test_fancy(self):
        print("\n26...")
        import numpy
        assert numpy.all(
            numpy.array([[9, 16], [1, 4]]) -
            ECHO("fancy",
                 ΓΠ.square(numpy.array([[3, 4], [1, 2]])))
            == 0)

    def test_saxpy(self):
        print("\n27...")
        DEFINE(
            'saxpy',
            Λ(lambda π:
              numpy.dot(π.a, π.x) \
                  if isinstance(π.a, numpy.ndarray)
                     and isinstance(π.x, numpy.ndarray) \
                  else π.a * π.x + π.y,
              ['a', 'x', 'y']))
        assert numpy.all(
            numpy.array([[68], [161]]) -
            ECHO("saxpy",
                 ΓΠ.saxpy(
                     numpy.array([[1, 2, 3], [4, 5, 6]]),
                     numpy.array([[7], [11], [13]]),
                     numpy.array([[42], [43]])))
            == 0)

    def test_recursion(self):
        print("\n28...")
        DEFINE('fact_iter_0',
               Λ(lambda π: π.product if π.counter > π.max_count else \
                   π.fact_iter_0(
                       π.counter * π.product,
                       π.counter + 1,
                       π.max_count
                   ), ['product', 'counter', 'max_count']));

        assert 720 == ΓΠ.fact_iter_0(1, 1, 6)


class TestFirstClassness:

    def test_procs_as_args(self):
        print("\n29...")
        # Outer parens necessary to break lines for comments (Python syntax booger).
        assert 1764 == \
               (Λ(lambda π:  # Create environment E1 in ΓΠ
                  π.f(π.x),  # Apply E1.f to E1.x.
                  ['f', 'x'])  # parameters
                (ΓΠ.square, 42))  # <~~~ Bind f to square, x to 42.

    def test_anon_sibling(self):
        print("\n30...")
        assert 1764 == \
               Λ(lambda π:  # Create environment E1 in ΓΠ.
                 Λ(lambda π:  # Create environment E2 in ΓΠ.
                   π.n * π.n,  # <~~~ n is bound in E2.
                   ['n']  # (E2 is sibling to E1)
                   )  # Parent environment implicitly ΓΠ.
                 (π.m),  # <~~~ Look up m in E1, bind to n in E2.
                 ['m'])(42)  # <~~~ Bind m to 42 in E1.

    def test_non_shadowing(self):
        print("\n31...")
        assert 1764 == \
               Λ(lambda π:  # Create environment E1 in ΓΠ.
                 Λ(lambda π:  # Create environment E2 in ΓΠ.
                   π.n * π.n,  # <~~~ n is bound in E2.
                   ['n']  # (E2 is sibling to E1)
                   )  # Parent environment implicitly ΓΠ.
                 # DIFFERENT ............................
                 (π.n),  # <~~~ Look up n in E1, bind to n in E2.
                 ['n'])(42)  # <~~~ a different n bound to 42 in E1.

    def test_anon_child(self):
        print("\n32...")
        assert 1764 == \
               Λ(lambda π:  # Create environment E1 in ΓΠ.
                 Λ(lambda π:  # Create environment E2 in E1 !!!!
                   π.x * π.n,  # <~~~ n in E1, x in E2.
                   ['x'],  # (E2 is child of E1, written E1<--E2)
                   π)  # Parent environment *explicitly* E1.
                 (π.n),  # <~~~ Look up n in E1, bind x in E1<--E2
                 ['n'])(42)  # <~~~ Bind n to 42 in E1

    def test_returned_proc(self):
        """Calling parens must start on same line as callee!"""
        print("\n33...")
        assert 1764 == \
               Λ(lambda π:  # Create environment E1 in ΓΠ.
                 π.f,  # Just return the value of parameter f.
                 ['f'])(
                   ΓΠ.square)(
                   42)  # Apply the returned procedure.


class TestAnonRecursion:

    def test_square_of_function(self):
        print("\n34...")
        assert 720 == \
               Λ(lambda π:
                 Λ(lambda π:
                   # Observe the "multiplication" sf(sf):
                   1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1),
                   ['n'], π), ['sf'])(  # <~~~ apply to copy of itself
                   Λ(lambda π:  # <~~~ this gets bound to 'sf'
                     Λ(lambda π:
                       1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1),
                       ['n'], π), ['sf']))(6)

    def test_delayed_square(self):
        print("\n35...")
        assert 720 == \
               Λ(lambda π:  # sf
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

    def test_abstracted_domain_code(self):
        print("\n36...")
        assert 720 == \
               Λ(lambda π:  # d
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

    def test_second_square(self):
        print("\n37...")
        assert 720 == \
               Λ(lambda π:  # function of domain code, d
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


class TestΥ1:

    def test_recursive_factorial(self):
        print("\n38...")
        assert 720 == \
               ΓΠ.Υ1(ΓΠ.fact_recursive)(6)


class TestΥ3:

    def test_iterative_factorial(self):
        print("\n39...")
        assert 720 == \
               ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 6)


class TestTailRecursion:

    def test_tail_fact_iter(self):
        print("\n40...")
        assert 720 == \
               LOOP3(ΓΠ.fact_iter)(1, 1, 6)

    def test_blown_recursion(self):
        print("\n41...")
        with pytest.raises(RecursionError):
            ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 400)

        with pytest.raises(RecursionError):
            ΓΠ.fact_iter_0(1, 1, 400)

    def test_safe_recursion(self):
        print("\n42...")
        assert \
            LOOP3(ΓΠ.fact_iter)(1, 1, 400) \
            == 64034522846623895262347970319503005850702583026002959458684445942802397169186831436278478647463264676294350575035856810848298162883517435228961988646802997937341654150838162426461942352307046244325015114448670890662773914918117331955996440709549671345290477020322434911210797593280795101545372667251627877890009349763765710326350331533965349868386831339352024373788157786791506311858702618270169819740062983025308591298346162272304558339520759611505302236086810433297255194852674432232438669948422404232599805551610635942376961399231917134063858996537970147827206606320217379472010321356624613809077942304597360699567595836096158715129913822286578579549361617654480453222007825818400848436415591229454275384803558374518022675900061399560145595206127211192918105032491008000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

    def test_fib_slow(self):
        print("\n43...")
        assert 46_368 == \
               ΓΠ.Υ1(ΓΠ.fib_slow)(23)
        assert 13 == \
               ΓΠ.Υ1(ΓΠ.fib_slow)(6)

    def test_fib_iter(self):
        print("\n44...")
        assert 46_368 == \
               LOOP3(ΓΠ.fib_iter)(0, 1, 23)

    def test_Υ2C(self):
        print("\n45...")
        assert 46_368 == \
               ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1]

    def test_blown_recursion_2(self):
        print("\n46...")
        with pytest.raises(RecursionError):
            ΓΠ.Υ2C(ΓΠ.fib_fast)({})(200)[1]

    def test_blown_fib_fast_uncurried(self):
        print("\n47...")
        with pytest.raises(RecursionError):
            assert ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 400)[1] == \
                   453973694165307953197296969697410619233826

    def test_blown_fib_memo(self):
        print("\n48...")
        with pytest.raises(RecursionError):
            ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)

    def test_fib_memo(self):
        print("\n49...")
        assert 225591516161936330872512695036072072046011324913758190588638866418474627738686883405015987052796968498626 == \
               LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1]


class TestBlock:

    def test_no_x(self):
        print("\n50...")
        DEFINE('x', 0)  # <~~~ in ΓΠ
        assert 42 == \
               BLOCK(
                   Λ(lambda π: SET_BANG('x', 6, π)),
                   Λ(lambda π: π.x * 7))

    def test_yes_y(self):
        print("\n51...")
        DEFINE('y', 42)  # <~~~ in ΓΠ
        assert 1764 == \
               BLOCK(
                   Λ(lambda π: SET_BANG('x', 6, π)),
                   Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                   Λ(lambda π: π.x * π.y))

    def test_undefined_z(self):
        print("\n52...")
        with pytest.raises(NameError):
            BLOCK(
                Λ(lambda π: SET_BANG('x', 6, π)),
                Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                Λ(lambda π: print({'expect 0': π.x * π.y})),
                Λ(lambda π: π.z))  # <~~~ no binding

    def test_non_global_environment(self):
        print("\n53...")
        assert 42 == \
               Λ(lambda π:  # <~~~ make a non-globl π by side-effect in Λ
                 BLOCK(
                     Λ(lambda π: SET_BANG('x1', 7, π), π=π),
                     Λ(lambda π: SET_BANG('y1', 6, π), π=π),
                     Λ(lambda π: π.x1 * π.y1, π=π),
                     π=π
                 ),
                 ['x1', 'y1'])(0, 0)

    def test_no_leak(self):
        print("\n54...")
        with pytest.raises(NameError):
            ΓΠ.x1
        with pytest.raises(NameError):
            ΓΠ.y1

    def test_not_none_returns(self):
        print("\n55...")
        assert 1764 == \
               BLOCK(
                   Λ(lambda π: SET_BANG('x', 6, π)),
                   Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                   Λ(lambda π: π.x * π.y),
                   Λ(lambda π: π.x * π.y))

    def test_nested_envs(self):
        print("\n56...")
        assert 1764 == \
               BLOCK(
                   Λ(lambda π: SET_BANG('x', 6, π)),
                   Λ(lambda π:  # <~~~ Don't forget to wrap it!
                     BLOCK(Λ(lambda π: SET_BANG('x', π.x * 7, π)),
                           Λ(lambda π: π.x * π.y))))


class TestLetStar:

    def test_depth_0(self):
        print("\n57...")
        assert 1806 == \
               LET_STAR([],
                        Ξ(Λ(lambda π: 43 * 42)))

    def test_depth_1(self):
        print("\n58...")
        assert 1764 == \
               LET_STAR([('z', 42)],
                        Ξ(ΓΠ.square, [Var('z')]))

    def test_depth_2_with_free_vars(self):
        print("\n59...")
        assert 1806 == \
               LET_STAR([('z', 42),
                         ('y', 43)],
                        Ξ(Λ(lambda π: π.z * π.y)))

    def test_depth_3_with_free_vars(self):
        print("\n60...")
        assert 1806 == \
               LET_STAR([('z', 42),
                         ('y', Ξ(Λ(lambda π: π.z + 1))),
                         ('w', Ξ(Λ(lambda π: π.z * π.y)))],
                        body=Ξ(Λ(lambda π: π.w)))

    def test_depth_3_with_bound_vars(self):
        print("\n61...")
        assert 1806 == \
               LET_STAR([('z', 42),
                         ('y', Ξ(Λ(lambda π: π.z + 1, ['z']),
                                 [Var('z')])),
                         ('w', Ξ(Λ(lambda π: π.z * π.y, ['z', 'y']),
                                 [Var('z'), Var('y')])
                          )],
                        body=Ξ(Λ(lambda π: π.w)))

    def test_procs_as_vars(self):
        print("\n62...")
        assert 1764 == \
               LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
                        body=Ξ(Λ(lambda π: π.g(42))))

    def test_no_leak(self):
        print("\n63...")
        with pytest.raises(NameError):
            print(ΓΠ.g)

    def test_bound_procs(self):
        print("\n64...")
        assert 1764 == \
               LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
                        body=Ξ(Λ(lambda π: π.gg(42), ['gg']),
                               [Var('g')]))


class TestLet:

    def test_depth_0(self):
        print("\n65...")
        assert 1806 == \
               LET([],
                   Ξ(Λ(lambda π: 43 * 42)))

    def test_depth_1(self):
        print("\n66...")
        assert 1764 == \
               LET([('z', 42)],
                   Ξ(ΓΠ.square, [Var('z')]))

    def test_depth_2(self):
        print("\n67...")
        assert 1806 == \
               LET([('z', 42),
                    ('y', 43)],
                   Ξ(Λ(lambda π: π.z * π.y)))

    def test_depth_2_reversed(self):
        print("\n68...")
        assert 1806 == \
               LET([('y', 42),
                    ('z', 43)],
                   Ξ(Λ(lambda π: π.z * π.y)))

    def test_no_downward_leakage(self):
        print("\n69...")
        DEFINE('y', 0)
        assert 42 == \
               LET([('y', 42),
                    ('z', Ξ(Λ(lambda π: π.y + 1)))],  # Outer y = 0, not inner y = 42
                   Ξ(Λ(lambda π: π.z * π.y)))  # Inner y = 42 * inner z = 1

    def test_no_downward_leakage_reversed(self):
        print("\n70...")
        assert 42 == \
               LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
                    ('y', 42)],
                   Ξ(Λ(lambda π: π.z * π.y)))  # Inner y = 42 * inner z = 1

    def test_bindings(self):
        print("\n71...")
        assert 42 == ΓΠ.x
        assert 0 == ΓΠ.y
        with pytest.raises(NameError):
            print(ΓΠ.z)

    def test_nested(self):
        print("\n72...")
        assert 1806 == \
               LET([('z0', 42)],
                   Ξ(Λ(lambda π:
                       LET([('y0', Ξ(Λ(lambda π: π.z0 + 1)))],
                           Ξ(Λ(lambda π: π.z0 * π.y0)),
                           π=π))))  # <~~~ Don't forget to chain!

    def test_del(self):
        print("\n73...")
        try:
            del ΓΠ.ϕ.x
        except:
            pass
        assert 1764 == \
               LET([('x', 42)],
                   Ξ(Λ(lambda π: π.x * π.x)))

    def test_no_x_leakage(self):
        print("\n74...")
        with pytest.raises(NameError):
            _ = ΓΠ.x

    def test_local_to_bound(self):
        print("\n75...")
        assert 1764 == \
               LET([('x', 42)],
                   Ξ(Λ(lambda π: π.y * π.y, ['y']),
                     [Var('x')]))


class TestCollections:
    bogue = [42, 43, 42, 43, 42, 43]

    def test_lists_with_free_vars(self):
        print("\n76...")
        assert self.bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.x * π.y)))

    def test_lists_with_bound_vars(self):
        print("\n77...")
        assert self.bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.xx * π.yy,
                       ['xx', 'yy']),
                     [Var('x'), Var('y')]))

    def test_tuples(self):
        print("\n78...")
        assert tuple(self.bogue) == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: (π.forty_two, π.forty_two + 1)))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: π.x * π.y)))

    def test_dictionaries(self):
        print("\n79...")
        assert self.bogue == \
               LET_STAR([
                   ('forty_two', 42),
                   ('x', Ξ(Λ(lambda π: {'π.forty_two': π.forty_two,
                                        'forty-three': π.forty_two + 1}))),
                   ('y', 3)],
                   Ξ(Λ(lambda π: [π.x['π.forty_two'],
                                  π.x['forty-three']] * π.y)))

    def test_numpy_array(self):
        print("\n80...")
        assert numpy.all(
            numpy.array([126, 129]) ==
            LET_STAR([
                ('forty_two', 42),
                ('x', Ξ(Λ(lambda π: numpy.array(
                    [π.forty_two,
                     π.forty_two + 1])))),
                ('y', 3)],
                Ξ(Λ(lambda π: π.x * π.y))))


class TestLetRec:

    def test_fact_81_bound_var(self):
        print("\n81...")
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.z1(π.m - 1, π.m * π.a, π.z1)),
                          ['m', 'a', 'z1']))],
                      Ξ(Λ(lambda π: π.fact(6, 1, π.fact))))

    def test_fact_82_same_name(self):
        print("\n82...")
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a, π.fact)),
                          ['m', 'a', 'fact']))],
                      Ξ(Λ(lambda π: π.fact(6, 1, π.fact))))

    def test_fact_83_no_application(self):
        print("\n83...")
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
        print("\n84...")
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
        print("\n85...")
        assert 1806 == \
               LETREC([('z0', Λ(lambda π: 1 + π.y0(), ['y0'])),
                       ('y0', Λ(lambda π: 42))],
                      Ξ(Λ(lambda π: π.y0() * π.z0(π.y0))))

    def test_86_binding_levels(self):
        print("\n86...")
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π1:
                          (ECHO('π1', π1).a
                           if π1.m <= 0
                           else ECHO('f', π1.f)(π1.m - 1, π1.m * π1.a, π1.f)),
                          ['m', 'a', 'f']))],
                      Ξ(Λ(lambda π0: ECHO('π0', π0).fact(6, 1, π0.fact))))

    def test_87_no_leaks(self):
        print("\n87...")
        with pytest.raises(NameError):
            print(ΓΠ.y0)

        with pytest.raises(NameError):
            print(ΓΠ.z0)

        with pytest.raises(NameError):
            print(ΓΠ.fact)

    def test_88_letrec_fact_with_freevars(self):
        print("\n88...")
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a)),
                          ['m', 'a']))],
                      Ξ(Λ(lambda π: π.fact(6, 1))))

    def test_89_letrec_fact_procedure(self):
        print("\n89...")
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
        print("\n90...")
        assert 720 == \
               LETREC([('fact',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact(π.m - 1, π.m * π.a)),
                          ['m', 'a']))],
                      Λ(lambda π: π.fact(6, 1)))()

    def test_91_letrec_unrolled_freevars(self):
        print("\n91...")
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
        print("\n92...")
        with pytest.raises(NameError):
            print(ΓΠ.y0)

        with pytest.raises(NameError):
            print(ΓΠ.z0)

        with pytest.raises(NameError):
            print(ΓΠ.fact)

    def test_93_racket_example(self):
        print("\n93...")
        assert (True, False, False, True) == \
               LETREC([('is_even',
                        Λ(lambda π: True if π.n == 0 else (not π.is_odd(π.n)),
                          ['n'])),
                       ('is_odd',
                        Λ(lambda π: π.n != 0 and π.is_even(abs(π.n) - 1),
                          ['n']))],
                      Ξ(Λ(lambda π: (
                          π.is_even(42),
                          π.is_even(-43),
                          π.is_odd(-42),
                          π.is_odd(43),
                      ))))


class TestLabels:

    def test_94_test_fact(self):
        print("\n94...")
        assert 720 == \
               LABELS([('fact_iter_nom',
                        Λ(lambda π:
                          (π.a
                           if π.m <= 0
                           else π.fact_iter_nom(π.m - 1, π.a * π.m)),
                          ['m', 'a']))],
                      Ξ(Λ(lambda π: π.fact_iter_nom(6, 1))))

    def test_95_test_racket(self):
        print("\n95...")
        assert (True, False, False, True) == \
               LABELS([('is_even',
                        Λ(lambda π: True if π.n == 0 else (not π.is_odd(π.n)),
                          ['n'])),
                       ('is_odd',
                        Λ(lambda π: π.n != 0 and π.is_even(abs(π.n) - 1),
                          ['n']))],
                      Ξ(Λ(lambda π: (
                          π.is_even(42),
                          π.is_even(-43),
                          π.is_odd(-42),
                          π.is_odd(43),
                      ))))
