print(42)


from dataclasses import dataclass
from pprint import pprint, pformat
from typing import Any


def ECHO(key: str, x: Any) -> Any:
    """In any Lisp, this would be a macro!"""
    print()
    pprint({key: x})
    return x


@dataclass
class Environment:
    """An Environment has a frame ϕ and a pointer π to an enclosing
    Environment. Bindings in the frame are _attributes_ on the
    object ϕ. Choose an empty function as the carrier object for
    such bindings / attributes. Bindings / attributes have a name and
    a value. Retrieve the value of binding ξ in frame ϕ of
    Environment E via dot notation as in 'E.ϕ.ξ'. Set the value v
    of binding ξ via 'setattr(E.ϕ, ξ, v)'. When getting values of
    bindings, it's OK to omit the ϕ, writing 'E.ξ', because of the
    overloaded __getattr__ of this class, Environment. Bracket
    notation is also OK, as in 'E["ξ"]', because of Environment's
    overloaded __getitem__."""
    ϕ: "() -> None"  # "frame," a nice place to hang attributes
    π: "Environment | None"  # via Greek πηρι, short name for 'enclosing'

    def _is_global(self):
        return self.π is None

    def _is_empty(self):
        return not vars(self.ϕ)

    def _copy(self) -> "Environment":
        """same parent π"""
        e = Environment(lambda: None, self.π)
        for k, v in vars(self.ϕ).items():
            setattr(e.ϕ, k, v)
        return e

    def _copy_trunk(self) -> "Environment":
        """Don't copy the global at the end, but do leave it
        attached. WARNING: this does not return a _trunk_
        because the global environment remains at the end.
        This avoids two traversals of the chain, once to
        remove the global and again to splice something in
        its place."""
        if self._is_global():
            return self
        r = self._copy()
        r.π = r.π._copy_trunk()  # recurse
        return r

    def _splice_onto_lower(self, lower: "Environment"):
        """Splice self onto the end of lower._trunk(), which
        does not have the global at its end. Self should either
        be the global or have the global at its end.

        Purpose:
        For monkey-patching proc envs, where free vars and
        parameters are looked up. 'Lower' comes from the
        prevailing env chain when a procedure is evaluated, i.e.,
        defined in some env like that established by LET_STAR.
        'Lower' may contain bindings for free variables in the
        body of the procedure.

        _splice_onto_lower is called iff 'self' comes from the
        explicit π parameter of a procedure constructor. 'Self'
        is global if the procedure has no parameters (i.e., no
        non-free variables). This rule is not checked because
        it's ignored during testing.

        Aside:
        When a procedure with parameters is APPLY'd, a fresh
        singleton env is consed on to 'self.' The fresh env
        contains parameter bindings. When a procedure with no
        parameters is APPLY'd, no such env is consed on.

        Definitions:
        - A chain ends in global with no refs to global inside.
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

        self is global      lower is global     return lower
        self is global      lower is not empty  return lower
        self is not empty   lower is global     return self
        self is not empty   lower is not empty  splice
        """
        assert lower is not None
        if lower._is_global() or lower._is_empty():
            return self
        if lower is self:
            return self
        if self._is_global():
            return lower

        temp = lower._trunk()
        branch = temp
        while branch.π:
            branch = branch.π
        branch.π = self  # MONKEY PATCH!
        return temp

    def _trunk(self) -> "Environment | None":
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


ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"


from typing import Dict, List, Any
Parameters = List[str]  # type synonym; positional, ordered arguments only


def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
          args: List["Expression"] | None,
          π: Environment = ΓΠ) -> Any:  # defaults to global
    """forward reference; will be corrected. Needed to
    spec Procedure."""
    ECHO("OLD APPLY.args", args)  # Just print, for now.


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


setattr(  # Bind a variable ...
    ΓΠ.ϕ,  # ... in the frame of the global environment ...
    "square",  # ... a variable named "square" ...
    Procedure(  # ... to this Schemulator procedure.
        {"body": lambda π: π.x * π.x,
         "parameters": ['x']}))  # Don't forget the parameter list!


ΓΠ.square(5)
ΓΠ.square(5, 6)


def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
          args: List["Expression"] | None,
          π: Environment = ΓΠ) -> Any:  # defaults to global
    ECHO("NEW APPLY.args", args)


ΓΠ.square(5)
ΓΠ.square(5, 6)


def AOT_DEMO():
    ΓΠ.square(5)
    ΓΠ.square(5, 6)

    def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
              args: List["Expression"] | None,
              π: Environment = ΓΠ) -> Any:  # defaults to global
        ECHO("LOCAL APPLY.args", args)

    ΓΠ.square(5)
    ΓΠ.square(5, 6)


print("++++++++ AOT ++++++++")
AOT_DEMO()
