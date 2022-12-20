from pprint import pprint, pformat
from typing import Any


def ECHO(key: str, x: Any) -> Any:
    """In any Lisp, this would be a macro!"""
    pprint({key: x})
    return x


from dataclasses import dataclass, field
from typing import Any

from dataclasses import dataclass, field
from types import FunctionType
from typing import Any


@dataclass
class Environment:
    """An Environment has a frame ϕ and a pointer π to an enclosing
    Environment. Bindings in the frame are attributes on the
    object ϕ. Choose an empty function as the carrier object for
    such bindings / attributes. Bindings / attributes have a name and
    a value. Retrieve the value of binding ξ in fraome ϕ of
    Environment E via dot notation as in 'E.ϕ.ξ'. Set the value v
    of binding ξ via 'settattr(E.ϕ, ξ, v)'. When getting values of
    bindings, it's OK to omit the ϕ, writing 'E.ξ', because of the
    overloaded __getattr__ of this class, Environment. Bracket
    notation is also OK, as in 'E["ξ"]', because of Environment's
    overloaded __getitem__."""
    ϕ: "() -> None"  # "frame," a nice place to hang attributes
    π: "Environment"  # via Greek πηρι, short name for 'enclosing'

    def _get_it(self, key: str) -> Any:
        """Walk the sequence of Environments upward."""
        try:
            ρ = getattr(self.ϕ, key)
        except AttributeError as _:
            if self.π is None:
                raise NameError(f'Environment: Name {key} is unbound.')
            else:  # Recurse: walk upwards.
                ρ = self.π.__getattr__(key)
        return ρ

    def __getattr__(self, key: str) -> Any:
        """recursive lookup by dot notation"""
        return self._get_it(key)

    def __getitem__(self, key: str) -> Any:
        """recursive lookup by bracket notation"""
        return self._get_it(key)

    def __repr__(self):
        """for the debugger"""
        is_global = (self.π is None)
        result = ("(" + hex(id(self.ϕ))[-4:] +
                  (",ΓΠ" if is_global else "") +
                  ") ") + \
                 pformat(str(list(self.ϕ.__dict__.keys()))) + \
                 (">" + self.π.__repr__()
                  if not is_global
                  else "")

        return result


#    def __setattr__(self, key, val):
#        """Diverges because it calls __getattr__ for 'self.ϕ'."""
#        setattr(getattr(self, 'ϕ'), key, val)
#        setattr(self.ϕ, key, val)


# diverges because it calls __getattr__ for 'self.ϕ'
#    def __setattr__(self, key, val):
#        setattr(getattr(self, 'ϕ'), key, val)
#        setattr(self.ϕ, key, val)
# The ugliness of 'setattr' is hidden in DEFINE.


ΓΠ = Environment(lambda: None, None)

from typing import Dict, List, Tuple, Any

Parameters = List[str]  # positional, ordered arguments only

from typing import Dict, List, Tuple, Any

Parameters = List[str]  # type synonym; positional, ordered arguments only


def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
          args: List[Any],
          π: Environment = ΓΠ) -> Any:  # defaults to global
    """forward reference; will be corrected. Needed to
    spec Procedure."""
    ECHO("APPLY.args", args)  # Just print, for now.


@dataclass
class Procedure:
    """Include __call__ override for convenient syntax."""
    code: Dict
    π: Environment = ΓΠ  # bound in global environment by default

    def __init__(self, code, π: Environment = ΓΠ):
        if len(set(code["parameters"])) != len(code["parameters"]):
            raise ValueError(
                f'Procedure: parameters {code["parameters"]}'
                ' must not contain duplicate symbols.')
        self.code = code
        self.π = π

    def __call__(self, *args):
        result = APPLY(self, args, self.π)
        return result

    def __repr__(self):
        """for the debugger"""
        result = pformat({
            'Λ': hex(id(self.code['body']))[-4:],
            'parms': str(self.code['parameters']),
            'env': self.π if self.π.π else 'ΓΠ'
        })
        return result


def Λ(body: "(π: Any) -> Any",
      parameters=None,  # default empty
      π=ΓΠ  # default global
      ) -> Procedure:
    ρ = Procedure(
        code={"body": body,
              "parameters": parameters or []},
        π=π)
    return ρ


from typing import Union, Any

from typing import Union, Any


def EVAL_APPLICATION(
        expr: "Application",
        π: Environment = ΓΠ
) -> Any:
    """forward reference; corrected below"""
    pass


from dataclasses import (dataclass, field)


@dataclass
class Application:
    head: Union[str, Procedure]
    args: List[Any] = field(default_factory=list)  # args, not params!
    π: Environment = ΓΠ

    def __call__(self):
        EVAL_APPLICATION(self, self.π)

    def __repr__(self):
        """for the debugger"""
        result = str({
            'Ξ': hex(id(self))[-4:],
            'head': self.head,
            'args': self.args,
            'π': self.π if self.π.π else "ΓΠ"
        })
        return result


Ξ = Application


@dataclass
class Var:
    sym: str


def EVAL(expr: Any, π: Environment = ΓΠ, tag: str = None) -> Any:
    """forward reference, corrected below"""
    pass


def EVAL_APPLICATION(expr: Application, π: Environment = ΓΠ) -> Any:
    """corrected definition"""
    if isinstance(expr.head, str):
        head = EVAL(expr.π[expr.head], expr.π)  # 1/3. Evaluate first slot ...
        # ... yielding a procedure, perhaps through a Var:
        assert isinstance(head, Procedure), \
            f'The head of {expr} must be a string or a Procedure, ' \
            f'not a {expr.head}'
    elif isinstance(expr.head, Procedure):
        head = expr.head
    else:
        raise ValueError(
            f'The head of {expr} must be a string or a Procedure, '
            f'not a {expr.head}')
    eargs = [EVAL(arg, π) for arg in expr.args]  # 2/3. Evaluate all args.
    ρ = APPLY(head, eargs, π)  # 3.3. Apply the procedure.
    return ρ


from typing import Any, Dict, Tuple, List
import numpy


def EVAL(expr: Any, π: Environment = ΓΠ, tag: str = None) -> Any:
    """forward reference, corrected below"""
    pass


def EVAL_APPLICATION(expr: Application, π: Environment = ΓΠ) -> Any:
    """corrected definition"""
    if isinstance(expr.head, str):
        head = EVAL(expr.π[expr.head], expr.π)  # 1/3. Evaluate first slot ...
        # ... yielding a procedure, perhaps through a Var:
        assert isinstance(head, Procedure), \
            f'The head of {expr} must be a string or a Procedure, ' \
            f'not a {expr.head}'
    elif isinstance(expr.head, Procedure):
        head = expr.head
    else:
        raise ValueError(
            f'The head of {expr} must be a string or a Procedure, '
            f'not a {expr.head}')
    eargs = [EVAL(arg, π) for arg in expr.args]  # 2/3. Evaluate all args.
    ρ = APPLY(head, eargs, π)  # 3.3. Apply the procedure.
    return ρ


from typing import Any, Dict, Tuple, List


def EVAL(
        expr: Any,
        π: Environment = ΓΠ,
        tag=None
) -> Any:
    """Python does a lot of this for us.
    'Tag' is included to aid debugging, especially
    outside this notebook."""
    if tag == 'debug':
        pprint({"EVAL": "",
                "expr": expr,
                "type": type(expr),
                "tag": tag,
                "env": π})
    if isinstance(expr, Dict):
        ρ = {k: EVAL(v, π) for k, v in expr.items()}
    elif isinstance(expr, Tuple):
        ρ = tuple((EVAL(v, π) for v in expr))
    elif isinstance(expr, List):
        ρ = [EVAL(v, π) for v in expr]
    elif isinstance(expr, numpy.ndarray):
        ρ = numpy.vectorize(lambda v: EVAL(v, π))(expr)
    elif isinstance(expr, Var):
        ρ = π[expr.sym]  # recursive lookup in Environment
    elif isinstance(expr, Application):
        ρ = EVAL_APPLICATION(expr, π)
    else:
        ρ = expr
    return ρ  # hang a breakpoint here


class IllegalArgumentsError(ValueError):
    pass


def APPLY(
        proc: Procedure,
        args=None,  # Python doesn't like mutable [] here, ...
        π: Environment = ΓΠ
) -> Any:
    if args is None:
        args = []  # ... but here it's OK.
    if len(proc.code['parameters']) != len(args):
        raise IllegalArgumentsError(
            f"Wrong number of arguments, "
            f"{len(args)} = len({args}), "
            f"passed to procedure {proc}, "
            f"which expects {len(proc.code['parameters'])} = "
            f"len({proc.code['parameters']})."
        )
    # 1/4. Eval args in old env.
    evaled_args = [EVAL(arg, π) for arg in args]
    # 2/4. Make a new environment, E1.
    E1 = Environment(lambda: None, π)
    # 3/4. Bind parameters in new env E1 to actual args.
    for k, v in zip(proc.code['parameters'], evaled_args):
        setattr(E1.ϕ, k, v)
    # 4/4. Invoke the code body, ...
    ρ = proc.code['body'](E1)
    # ... always a lambda of an environment π.
    return ρ


def DEFINE(
        sym: str,
        val: Any,
        π: Environment = ΓΠ  # default
) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
    return val


DEFINE('Υ1',
       Λ(lambda π:  # function of domain code, d
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of one parameter
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m),
                   ['m'], π)),
               ['sf'], π)),
         ['d']))

DEFINE('fact_recursive',
       Λ(lambda π:  # domain code; function of business code, f
         Λ(lambda π:
           1 if π.n < 1 else π.n * π.f(π.n - 1),  # business code
           ['n'], π),  # 1 business parameter, n
         ['f']))  # recursive function

# λ d: (λ g: g[g])(λ sf: d[λ m, c, x: sf[sf][m, c, x]])
DEFINE('Υ3',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # ... of business code of three parameters
             Λ(lambda π: π.d(  # domain code
                 Λ(lambda π:
                   π.sf(π.sf)(π.m, π.c, π.x),  # business code
                   ['m', 'c', 'x'], π)),  # business parameters
               ['sf'], π)),
         ['d']))

# λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter',  # domain code is a function of f ...
       Λ(lambda π:  # ... which is business code.
         Λ(lambda π:
           π.m
           if π.c > π.x
           else π.f(π.m * π.c, π.c + 1, π.x),  # business code
           ['m', 'c', 'x'], π),  # business parameters
         ['f']))


class TailCall(Exception):
    """αναδρομική κλήση"""

    def __init__(self, *args):
        """Overwrite old args with new."""
        self.args = args


def RECUR(*args):
    """υψώνω: in sincere flattery of Clojure"""
    raise TailCall(*args)


def LOOP3(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    # in the global environment, ΓΠ,
    DEFINE('Ρ3', Λ(lambda π: RECUR(π.m, π.c, π.x), ['m', 'c', 'x']));

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ3)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π: looper(π.m, π.c, π.x), ['m', 'c', 'x'], π=d.π)
    return ρ


DEFINE('fib_slow',
       Λ(lambda π:
         Λ(lambda π: 1 if π.n < 2 else
         π.f(π.n - 1) + π.f(π.n - 2), ['n'], π),
         ['f']))

DEFINE('fib_iter',
       Λ(lambda π:
         Λ(lambda π: π.b if π.n < 1 else
         π.f(π.b, π.a + π.b, π.n - 1),
           ['a', 'b', 'n'], π),
         ['f']))

DEFINE('Υ2C',
       Λ(lambda π:  # function of domain code, d ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # with business code of 2 parameters, curried
             Λ(lambda π:
               π.d(Λ(lambda π:
                     Λ(lambda π: π.sf(π.sf)(π.m)(π.n),
                       ['n'], π), ['m'], π)),
               ['sf'], π)),
         ['d']))

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

DEFINE('Υ2',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of two parameters
             Λ(lambda π:
               π.d(Λ(lambda π: π.sf(π.sf)(π.m, π.c),
                     ['m', 'c'], π)),
               ['sf'], π)),
         ['d']))

DEFINE('Υ5',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of five parameters
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m, π.c, π.x, π.a, π.b),
                   ['m', 'c', 'x', 'a', 'b'], π)),
               ['sf'], π)),
         ['d']))

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


def LOOP5(d: Procedure) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    DEFINE('Ρ5', Λ(lambda π:
                   RECUR(π.m, π.c, π.x, π.a, π.b),
                   ['m', 'c', 'x', 'a', 'b']));

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ5)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(π.m, π.c, π.x, π.a, π.b),
          ['m', 'c', 'x', 'a', 'b'], π=d.π)
    return ρ


def SET_BANG(
        sym: str,
        val: Any,
        π: Environment = ΓΠ
) -> None:
    ee = EVAL(val, π)
    """recursive lookup"""
    while π is not None:
        # Find the right π; ...
        try:
            getattr(π.ϕ, sym)  # ... don't recurse via π[sym]
            break
        except AttributeError as _:
            if π.π is None:
                raise NameError(f'Set!: Name {sym} is unbound.')
            else:  # recurse
                π = π.π
    setattr(π.ϕ, sym, ee)
    return None  # following Gambit Scheme


def BLOCK(
        *ss: "Procedure | Procedure",  # <~~~ PEP 438 type notation
        π: Environment = ΓΠ
) -> Any:
    ρ = None
    for s in ss:
        ρ = APPLY(s, [], π=π)  # <~~~ thunks take no args
    return ρ


BEGIN = BLOCK


def LET_STAR(
        binding_pairs: List[Tuple[str, Application]],
        body: Application,
        π: Environment = ΓΠ
) -> Any:
    if len(binding_pairs) == 0:  # <~~~ Empty bindings are allowed.
        ρ = EVAL(body, π)
        return ρ
    key, val = binding_pairs[0]
    if len(binding_pairs) == 1:
        νλ = Λ(lambda π:
               EVAL(body, π),
               [key], π=π)
    else:
        νλ = Λ(lambda π:  # <~~~ Sequence is realized by recursion.
               LET_STAR(binding_pairs[1:], body, π),
               [key], π=π)  # <~~~ Automatically chains envs.
    ρ = νλ(EVAL(val, π))
    return ρ


def LET(
        binding_pairs: List[Tuple[str, Any]],
        body: Application,
        π: Environment = ΓΠ
) -> Any:
    if len(binding_pairs) == 0:
        ρ = EVAL(body, π)
        return ρ
    keys = [pair[0] for pair in binding_pairs]
    vals = [pair[1] for pair in binding_pairs]
    νλ = Λ(lambda π:
           EVAL(body, π),
           keys,
           π=π)
    ρ = APPLY(νλ, vals, π=π)  # <~~~ Makes a new env for νλ.
    return ρ


def LETREC(
        binding_pairs: List[Tuple[str, Any]],
        body: Application,
        π: Environment = ΓΠ
) -> Any:
    if len(binding_pairs) == 0:
        ρ = EVAL(body, π)
        return ρ
    E1 = Environment(lambda: None, π)
    _ = [setattr(E1.ϕ, pair[0], pair[1])
         for pair in binding_pairs]
    # Monkey-patch environments for vals that are Procedures.
    for pair in binding_pairs:
        if isinstance(pair[1], Procedure):
            pair[1].π = E1
    # Monkey patch the body, if it's a Procedure.
    if isinstance(body, Procedure):
        body.π = E1
    ρ = EVAL(body, E1)
    return ρ
