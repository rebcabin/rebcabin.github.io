from pprint import pprint, pformat
from typing import Any


def ECHO(key: str, x: Any) -> Any:
    """In any Lisp, this would be a macro!"""
    print()
    pprint({key: x})
    return x


from dataclasses import dataclass
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

    def _is_global(self):
        return self.π is None

    def _is_empty(self):
        return not vars(self.ϕ)

    def _copy(self) -> "Environment":
        e = Environment(lambda: None, self.π)
        for k, v in vars(self.ϕ).items():
            setattr(e.ϕ, k, v)
        return e

    def _copy_trunk(self) -> "Environment":
        """Don't copy the global at the end, but do leave it
        attached."""
        if self._is_global():
            return self
        r = self._copy()
        r.π = r.π._copy_trunk()  # recurse
        return r

    def _splice_onto_lower(self, lower: "Environment"):
        """Splice self onto the end of lower._trunk(), which
        does not have the global at its end. Self should be
        either global or have global at its end.

        For monkey-patching proc envs, where free vars and
        parameters are looked up. 'Lower' comes from the
        prevailing env chain when a procedure is evaluated, i.e.,
        defined in some env like that established by LET_STAR.
        'Lower' contains bindings for free variables in the body
        of the procedure.

        _splice_onto_lower is called iff 'self' comes from the
        explicit π parameter of a procedure constructor. 'Self'
        is global if the procedure has no parameters (i.e., no
        non-free variables). An exception to this rule pertains
        during testing, so the rule is not machine-checked.

        Aside:
        When a procedure with parameters is APPLY'd, a fresh
        env is consed on to 'self.' The fresh env contains
        parameter bindings. When a procedure with no parameters
        is APPLY'd, no such env is consed on.

        Definitions:
        - A chain ends in global with no refs to globals inside.
        - The tip of a chain is the one env furthest from global.
        - The trunk of a chain is all but the final global.
        - The root of a chain is the final global.

        lemmas:
        - A chain cannot be None.
        - A chain could be simply a ref to global. That's a
          chain with no tip or trunk, only a root.
        - The tip of a chain is None if the chain is just the
          final global.
        - The trunk of a chain is None if the chain is just the
          final global.

        @precondition: self and lower are non-None chains. There
        are two references to the global in the two chains.

        @postcondition: a chain whose tip is a mutable copy of
        the tip of lower if lower is not empty. The final global
        of lower is replaced by self. The resulting chain has
        one ref to global at the end (the root).

        scenarios:
        _splice_onto_lower is called iff self is an explicit
        procedure env.

        proc env is global      lower is global     return lower
        proc env is global      lower is not empty  return lower
        proc env is not empty   lower is global     return self
        proc env is not empty   lower is not empty  splice
        """
        assert lower is not None
        # TODO: assert lower._is_global() or not lower._is_empty()
        if lower._is_global() or lower._is_empty():
            return self
        if lower is self:
            return self
        if self._is_global():
            return lower

        temp = self._trunk()
        branch = temp
        while branch.π:
            branch = branch.π
        branch.π = lower  # MONKEY PATCH!
        return temp

    def _trunk(self) -> "Environment":
        """all but the last, global environment in a chain;
        Remove the pointer to global env in a copy of the
        penultimate. Called ONLY by _splice_onto_lower!
        """
        if self._is_global():
            return None
        else:  # Monkey-patch the last env
            result = self._copy_trunk()
            branch = result
            while not branch.π._is_global():
                branch = branch.π
            branch.π = None  # danger! Looks global!
            # Fix this immediately in _splice_onto_lower
        return result

    def _get_binding_val(self, var: str) -> Any:
        """Walk the sequence of Environments upward."""
        try:
            ρ = getattr(self.ϕ, var)
        except AttributeError as _:
            if self.π is None:
                raise NameError(
                    f'Environment: Name {var} is unbound.')
            else:  # Recurse: walk upwards.
                ρ = self.π.__getattr__(var)
        return ρ

    def __getattr__(self, key: str) -> Any:
        """recursive lookup by dot notation"""
        return self._get_binding_val(key)

    def __getitem__(self, key: str) -> Any:
        """recursive lookup by bracket notation"""
        return self._get_binding_val(key)

    def __repr__(self):
        """for the debugger"""
        is_global = (self.π is None)
        result = ("(" + hex(id(self.ϕ))[-4:] +
                  (",ΓΠ" if is_global else "") +
                  ") ") + \
                 pformat(str(list(vars(self.ϕ).keys()))) + \
                 (">" + self.π.__repr__()
                  if not is_global
                  else "")

        return result


#    def __setattr__(self, var, val):
#        """Diverges because it calls __getattr__ for 'self.ϕ'."""
#        setattr(getattr(self, 'ϕ'), var, val)
#        setattr(self.ϕ, var, val)


ΓΠ = Environment(lambda: None, None)

from typing import Dict, List, Any

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


from typing import Any
import numpy


def EVAL(
        expr: Any,
        π: Environment = ΓΠ,
        tag: str = None
) -> Any:
    """forward reference, corrected below"""
    pass


def EVAL_APPLICATION(
        expr: Application,
        π: Environment = ΓΠ
) -> Any:
    """corrected definition"""
    if isinstance(expr.head, str):
        # 1/4. Evaluate first slot to find proc from string ...
        proc = π[expr.head]
        # ... yielding a procedure, perhaps through a Var:
        assert isinstance(proc, Procedure), \
            f'The head of {expr} must be a string or a Procedure, ' \
            f'not a {expr.head}'
    elif isinstance(expr.head, Procedure):
        # 1/4. Evaluate first slot in an env with free vars ...
        proc = expr.head
    else:
        raise ValueError(
            f'The head of {expr} must be a string or a Procedure, '
            f'not a {expr.head}')
    # 2/4. Evaluate the proc to access free vars ...
    proc = EVAL(proc, π)
    # 3/4. Evaluate all args ...
    eargs = [EVAL(arg, π) for arg in expr.args]
    # 4/4. Apply the procedure.
    ρ = APPLY(proc, eargs, π)  # 3.3. Apply the procedure.
    return ρ


def EVAL_PROCEDURE(
        λ: Procedure,
        π: Environment = ΓΠ
) -> Procedure:
    λ.π = λ.π._splice_onto_lower(π)
    return λ


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
    elif isinstance(expr, Procedure):
        ρ = EVAL_PROCEDURE(expr, π)
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
    # 1/3. Make a new environment, E1, if needed.
    if proc.code['parameters']:
        E1 = Environment(lambda: None, π)
    else:
        E1 = π
    # 2/3. Bind parameters in new env E1 to actual args, evaled
    #      in the old environment ...
    for k, v in zip(proc.code['parameters'], args):
        setattr(E1.ϕ, k, EVAL(v, π))
    # 3/3. Invoke the code body, ...
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
       Λ(lambda πd:  # function of domain code, d
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of one parameter
             Λ(lambda πsf:
               πd.d(Λ(lambda π: π.sf(π.sf)(π.m),
                      ['m'], πsf)),
               ['sf'], πd)),
         ['d']))

# λ d: (λ g: g[g])(λ sf: d[λ m, c, x: sf[sf][m, c, x]])
DEFINE('Υ3',
       Λ(lambda πd:  # of d, the domain code ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # ... of business code of three parameters
             Λ(lambda πsf: πd.d(  # domain code
                 Λ(lambda π:
                   π.sf(π.sf)(π.μ, π.γ, π.ξ),  # business code
                   ['μ', 'γ', 'ξ'], πsf)),  # business parameters
               ['sf'], πd)),
         ['d']))


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
    nyms = ['α', 'β', 'γ']
    DEFINE('Ρ3',
           Λ(lambda π:
             RECUR(*[π[nym] for nym in nyms]),
             nyms))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ3)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(*[π[nym] for nym in nyms]),
          nyms,
          π=d.π)

    return ρ


def LOOP1(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    # in the global environment, ΓΠ,
    DEFINE('Ρ1',
           Λ(lambda π:
             RECUR(π.α),
             ['α']))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ1)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(π.α),
          ['α'],
          π=d.π)

    return ρ


def LOOP2(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    nyms = ['α', 'β']
    # in the global environment, ΓΠ,
    DEFINE('Ρ2',
           Λ(lambda π:
             RECUR(*[π[nym] for nym in nyms]),
             nyms))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ2)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(*[π[nym] for nym in nyms]),
          nyms,
          π=d.π)

    return ρ


# Don't bother generalizing Υ2C now:

DEFINE('Υ2C',
       Λ(lambda πd:  # function of domain code, d ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # with business code of 2 parameters, curried
             Λ(lambda πsf:
               πd.d(Λ(lambda π:
                      Λ(lambda πn:
                        # Notice double application because of currying.
                        πn.sf(πn.sf)(πn.m)(πn.n),
                        ['n'], π),
                      ['m'], πsf)),
               ['sf'], πd)),
         ['d']))

DEFINE('Υ2',
       Λ(lambda πd:  # of d, the domain code ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of two parameters
             Λ(lambda πsf:
               # single application to two arguments; no currying
               πd.d(Λ(lambda π: π.sf(π.sf)(π.m, π.c),
                      ['m', 'c'], πsf)),
               ['sf'], πd)),
         ['d']))

DEFINE('ΥN',
       Λ(lambda πd:  # of d, the domain code and vars ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of N parameters
             Λ(lambda πsf:
               πd.d(Λ(lambda πvs:
                      πvs.sf(πvs.sf)(
                          *[πvs[var] for var in πvs.vars_]),
                      πsf.vars_,
                      πsf)),
               ['sf'], πd)),
         ['d', 'vars_']))

DEFINE('Υ5',
       Λ(lambda πd:  # of d, the domain code ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of five parameters
             Λ(lambda πsf:
               πd.d(
                   Λ(lambda π: π.sf(π.sf)(π.α, π.β, π.γ, π.δ, π.ζ),
                     ['α', 'β', 'γ', 'δ', 'ζ'], πsf)),
               ['sf'], πd)),
         ['d']))


def LOOP5(d: Procedure) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    nyms = ['α', 'β', 'γ', 'δ', 'ζ']
    DEFINE('Ρ5',
           Λ(lambda π:
             RECUR(*[π[nym] for nym in nyms]),
             nyms))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.Ρ5)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(*[π[nym] for nym in nyms]),
          nyms,
          π=d.π)

    return ρ


def LOOPN(d: Procedure, vars_: List[str]) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    DEFINE('ΡN',
           Λ(lambda π:
             RECUR(*[π[var] for var in vars_]),
             vars_))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(ΓΠ.ΡN)(*args)
            except TailCall as e:
                args = e.args

    ρ = Λ(lambda π:
          looper(*[π[var] for var in vars_]),
          vars_,
          π=d.π)

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
        binding_pairs: List[Tuple[str, Any]],
        body: Union[Application, Procedure],
        πl: Environment = ΓΠ
) -> Any:
    if len(binding_pairs) == 0:  # <~~~ Empty bindings are allowed.
        ρ = EVAL(body, πl)
        return ρ
    key, val = binding_pairs[0]
    E1 = Environment(lambda: None, πl)
    setattr(E1.ϕ, key, EVAL(val, πl))
    if len(binding_pairs) == 1:
        return EVAL(body, E1)
    else:
        return LET_STAR(binding_pairs[1:], body, E1)


def LET(
        pairs: List[Tuple[str, Any]],
        body: Application,
        π: Environment = ΓΠ
) -> Any:
    if len(pairs) == 0:
        ρ = EVAL(body, π)
        return ρ
    E1 = Environment(lambda: None, π)
    for p in pairs:
        if isinstance(p[1], Procedure):
            p[1].π = E1
    _ = [setattr(E1.ϕ, p[0], EVAL(p[1], π))
         for p in pairs]
    ρ = EVAL(body, E1)
    return ρ


def LETREC(
        pairs: List[Tuple[str, Any]],
        body: Union[Application, Procedure],
        π: Environment = ΓΠ
) -> Any:
    if len(pairs) == 0:
        ρ = EVAL(body, π)
        return ρ
    E1 = Environment(lambda: None, π)
    for p in pairs:
        if isinstance(p[1], Procedure):
            p[1].π = E1
    _ = [setattr(E1.ϕ, p[0], p[1]) for p in pairs]
    ρ = EVAL(body, E1)
    return ρ


def LABELS(
        binding_pairs: List[Tuple[str, Any]],
        body: Application,
        π: Environment = ΓΠ
) -> Any:
    for pair in binding_pairs:
        if not isinstance(pair[1], Procedure):
            raise IllegalArgumentsError(
                f'all values in labels must be Procedures; '
                f'this value {pair[1]} is not')
    result = LETREC(binding_pairs, body, π)
    return result  # <~~~ Hang breakpoint here.


def CHECK_TYPE(x: Any, t: Any) -> Any:
    assert isinstance(x, t)
    return x


def DO_NTC(
        triples: List[Tuple[str, Any, Procedure]],
        pred: Procedure,
        value: Any,
        body: Procedure,
        π: Environment = ΓΠ
) -> Any:
    """(DO ((<var1> <init1> <λstep1>)
            (<var2> <init2> <λstep2>
            . . .
            (<varñ> <initñ> <λstepñ))
            (<λpred> <λvalue>)
            <λbody>
            <env=None>).
    Steps are evaluated sequentially.
    Tail-recursive version requires a LOOPN.
    """
    vars = [CHECK_TYPE(t[0], str) for t in triples]
    inits = [t[1] for t in triples]
    steps = [CHECK_TYPE(t[2], Procedure) for t in triples]
    E1 = Environment(lambda: None, π)
    _ = [setattr(E1.ϕ, f'σteps_{i}', step)
         for i, step in enumerate(steps)]
    setattr(E1.ϕ, 'πred', CHECK_TYPE(pred, Procedure))
    setattr(E1.ϕ, 'vaλue', CHECK_TYPE(value, Procedure))
    setattr(E1.ϕ, 'βody', CHECK_TYPE(body, Procedure))
    r = LABELS([(
        'λoop',
        Λ(lambda πb:
          (EVAL(Ξ('vaλue'), πb)
           if EVAL(Ξ('πred'), πb)
           else πb.λoop(
              EVAL(Ξ('βody'), πb),
              *[EVAL(Ξ(f'σteps_{i}'), πb)
                for i in range(len(steps))])),
          ['βody_result', *vars]))],
        Ξ('λoop',
          [None, *[EVAL(init, E1) for init in inits]]),
        E1)
    return r


def DO(
        triples: List[Tuple[str, Any, Procedure]],
        pred: Procedure,
        value: Any,
        body: Procedure,
        π: Environment = ΓΠ):
    """(DO ((<var1> <init1> <λstep1>)
            (<var2> <init2> <λstep2>
            . . .
            (<varñ> <initñ> <λstepñ))
            (<λpred> <λvalue>)
            <λbody>
            <env=None>).
    Steps are evaluated sequentially.
    Tail-recursive version requires a LOOPN.
    """
    vars = [CHECK_TYPE(t[0], str) for t in triples]
    inits = [t[1] for t in triples]
    steps = [CHECK_TYPE(t[2], Procedure) for t in triples]
    E1 = Environment(lambda: None, π)
    _ = [setattr(E1.ϕ, f'σteps_{i}', step)
         for i, step in enumerate(steps)]
    setattr(E1.ϕ, 'πred', CHECK_TYPE(pred, Procedure))
    setattr(E1.ϕ, 'vaλue', CHECK_TYPE(value, Procedure))
    setattr(E1.ϕ, 'βody', CHECK_TYPE(body, Procedure))
    r = LABELS([(
        'λoop',
        Λ(lambda πd:  # Domain code is a functino of 'λf', ...
          Λ(lambda πb:  # ... which is busines code of N params.
            (EVAL(Ξ('vaλue'), πb)
             if EVAL(Ξ('πred'), πb)
             else πb.λf(  # <~~~ tail recursion
                EVAL(Ξ('βody'), πb),
                *[EVAL(Ξ(f'σteps_{i}'), πb)
                  for i in range(len(steps))])),
            ['βody_result', *vars], π=πd),
          ['λf'], π=E1))],
        Ξ(Λ(lambda π:
            LOOPN(π.λoop, ['βody_result', *vars])
            (None, *[EVAL(init, E1) for init in inits]))),
        E1)
    return r
