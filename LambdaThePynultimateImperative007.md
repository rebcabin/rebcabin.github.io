---
jupytext:
  formats: ipynb,md:myst
  text_representation:
    extension: .md
    format_name: myst
    format_version: 0.13
    jupytext_version: 1.14.1
kernelspec:
  display_name: Python 3 (ipykernel)
  language: python
  name: python3
---

### Lambda, the Pynultimate Imperative

+++

#### Version 7
#### Brian Beckman
#### 25 Dec 2022
#### [Creative Commons Attribution 4.0 International Public License](https://creativecommons.org/licenses/by/4.0/legalcode)

+++

# Introduction

+++

In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative constructs with just _lambda:_

+++

> We demonstrate how to model the following common [imperative] programming constructs in ... an applicative-order language similar to LISP: Simple Recursion, Iteration, Compound Statements and Expressions, GO TO and Assignment, Continuation-Passing, Escape Expressions, Fluid Variables, Call by Name, Call by Need, and Call by Reference. The models require only (possibly self-referent) lambda application, conditionals, and (rarely) assignment.

+++

It's useful to recap this paper in Python, which has most of the listed imperative constructs. Imagine compiling Python into an intermediate language in which the semantics, even those with side-effects, are laid bare as trees of $\lambda$ expressions. In such a representation, optimizations are 
1. easy to write as tree-to-tree transforms
1. easy to extend via just function composition (even Kleisli-monadic)
2. independent of surface syntax, thus easy to share with other imperative languages like Fortran, C, Java
3. independent of back ends, thus easy to run interactively; or to translate into LLVM, x86, ARM64, C, for execution; or to transpile into other surface languages

+++

The use-cases above are similar to those for a SQL algebraizer. Many SQL implementations 
1. translate the surface language into bare-bones expressions in a closed relational algebra, free of original syntax
2. run the algebraic expressions through symbolic optimizers, which often rearrange the expressions completely
2. incrementally extend the system by composing new optimizations
3. translate optimized expressions into commands for local and distributed servers

+++

We follow the paper more-or-less directly, with refernce to [SICP](https://sarabander.github.io/sicp/).

+++

## _Schemulation:_ Python Semantics in Python<a id="semantics"></a>

+++

Ideally, we'd compile Python into Scheme or Clojure or Common Lisp, then write transformations, translations, interpreters, debuggers, etc. in Scheme or Clojure or Common Lisp. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in a Scheme-like applicative-order $\lambda$-calculus embedded in basic Python.

+++

Some people may find this weird. Why not just implement Scheme in Python instead of emulating Scheme in Python? It's an engineering judgment call. Most authors of compilers and interpreters spend undue time on syntax before even getting to semantics. Compiler textbooks tell you to do this! Semantics becomes a hidden "implementation detail," but then developers forgive themselves for making an ungodly mess of it. We prefer the other way around. Get the semantics clean, composable, extendable, maintainable, and correct, _first_, then spend time on syntax, if you even care any more. Or just let someone else do it. From the development point of view, syntax is a distraction. It's been solved for decades, but it makes young developers and professors feel powerful. The decision to spend time on syntax before semantics is not engineering, it's self-indulgence.

+++

## Orthogonality as a Design Principle

+++

We prefer designs that minimize cross-talk. Each facility -- transformation layer or module -- should have the least possible dependency on other facilities. For example, tranformations that affect control flow need not necessarily depend on transformations that affect numerics.

+++

Contrast to a braided design, where each facility explicitly accounts for every other. Such a design is not usually created on purpose: it accretes as semantics is bodged on to syntax trees. Correcting such a design requires total refactoring after-the-fact. It's much easier to get it right before-the-fact.

+++

## How to Use this Notebook

+++

The goal of this notebook is to transmit ideas from my mind into yours. I want you to read the code. Most of the examples return easy numerical results like `42 * 43 == 1806`. I want you to do the arithmetic and check the result by looking at them. That's why I don't use Python `assert`, here.

+++

Run the cells sequentially, in order. The reason is that some of the cells modify the unique global environment, so that later cells depend on earlier cells. Some cells, such as cells that delete bindings from the global environment, can't be repeated (they're not idempotent).

+++

There is a companion Python project that `asserts` the results in pytest, that is, proves the results in the computer. Unlike this notebook, that project is written in non-narrative style, but the tests can be run in any order.

+++

# PART I: BASICS<a id="basics"></a>

+++

# `ECHO` for Debugging

+++

`ECHO` prints and then returns its argument. Semantically, it's the identity function.

+++

Though the print statement in `ECHO` is technically a side-effect, it's not a side-effect pertinent to Schemulator semantics. It's a side-effect at the interpreter level for user convenience.

```{code-cell} ipython3
from pprint import pprint, pformat
from typing import Any
def ECHO(key: str, x: Any) -> Any:
    """In any Lisp, this would be a macro!"""
    print()
    pprint({key: x})
    return x
```

# Environment and Frame<a id="environment"></a>

+++

We find it necessary to model Scheme's environments and frames explicitly. We tried multiple Pythonic short-cut alternatives and found that none of them compose well.

+++

[SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) has some apparent contradictions in the definition of environment and frame. It says that "an environment is a sequence of frames," but the rest of the text and figures clearly imply that an environment has just one frame.

+++

The best resolution appears to be:

+++

> __DEFINITION__: An ___environment___ is a frame $\phi$ and a pointer $\pi$ to an enclosing environment. A ___frame___ is a mathematical function from variable names to values; no variable name may appear more than once in a frame.

+++

We note in passing that this works only for a single thread. [Clojure, for instance, solves that problem with _Vars_](https://clojure.org/reference/vars).

+++

## Greek and ALL CAPS<a id="greek"></a>

+++

$\pi$ for an enclosing environment is a nice pun: it evokes $\pi\eta\rho\iota$, a Greek prefix meaning "surrounding," as in "perimeter" (_sic_, not "parameter").

+++

We use Greek letters for system attributes and variables that will be impossible for users to write if the follow the rule below. So, for example, we don't need `gensym`, a thorn in every Lisp programmer's side.

+++

> __RULE__: Avoid Greek in user-level code to avoid clobbering system-supplied names.

+++

We use ALL CAPS for system procedures that implement Schemulator.

+++

## Bindings<a id="bindings"></a>

+++

> __DEFINITION__: A ___binding___ is an association from a variable name to a value, that is, an entry in a frame.

+++

We might model a binding as a pair, or as a row in a table, an element of a relation (subset of a Cartesian product), an element of a Python dictionary, or as an attribute of a Python object. We prefer the attribute model because it affords _dot_ notation for lookup, that is, `o.foo` rather than the dictionary's syntax `o['foo']`. Thanks to [divs1210](https://gist.github.com/divs1210?page=3) for this idea.

+++

If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a virtual sequence of frames via the chain of $\pi\eta\rho\iota$ pointers to enclosing environments.

+++

> __INVARIANT__: The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`.

+++

> __OBSERVATION__: A frame $\phi$ belongs to a virtual sequence of environments implied by the unidirectional chain of enclosing environments ending at the unique global environment.

+++

> __DEFINITIONS__: The ___value of a variable in an environment___ is the value in the first binding in any frame of the sequence ending at _global_. Bindings lower in the chain may ___shadow___ bindings higher in the chain, rendering them inaccessible. If no frame in a chain specifies a binding for the variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another.

+++

In the Environment class, we override `__getattr__` to avoid a separate method for recursive lookup. We can't also override `__setattr__` to call `setattr(self.$\phi$ ...)` because `self.$\phi$` diverges on `getattr(self.$\phi$ ...)`.

+++

The `Environment` class is lengthy due to splicing code, necessary to give bindings to [___free variables___](#free-variables) in procedure bodies. Jupyter notebooks do not furnish a convenient way to break up the code for classes, so we have put essential information in comments in this code. Read the comments as if they were text in the document.

```{code-cell} ipython3
from dataclasses import dataclass, field
from types import FunctionType
from typing import Any
@dataclass
@dataclass
class Environment:
    """An Environment has a frame ϕ and a pointer π to an enclosing
    Environment. Bindings in the frame are _attributes_ on the
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
        singleton env is consed on to 'self.' The fresh env 
        contains parameter bindings. When a procedure with no
        parameters is APPLY'd, no such env is consed on.

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
```

# Unique Global Environment $\Gamma\Pi$<a id="global-environment"></a>

+++

The unique global environment is $\Gamma\Pi$, defined once for each session. The carrier frame $\phi$ is an empty function, namely `lambda: None`. The example shows setting and getting the binding for a made-up variable, `γόὂ`.

```{code-cell} ipython3
ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
```

## Example:

```{code-cell} ipython3
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ
```

# Procedure(code{body, params}, $\pi$)<a id="procedure"></a>

+++

From SICP again:

+++

> __DEFINITION__: A ___procedure___ is a pair of _code_ and environment.

+++

> __DEFINITION__: ___Code___ is a dictionary of two items: a list of parameter names and a `body`. _Body_ is a $\lambda$ expression taking a single parameter, $\pi$. $\pi$ is bound to the [`Environment`](#environment) in which the formal parameters of the procedure are in-turn [bound](#binding), later, to actual arguments at call time.

+++

> __DEFINITIONS__: ___Call___, ___invocation___, and ___application___ are synonyms for expressions in which a procedure's formal parameters are bound to actual arguments, which are values of `Any` type.

+++

> __DEFINITION__: ___Arguments___ or ___actual arguments___ are values of `Any` type that appear in 1-to-1 correspondence to parameters in a call expression, a.k.a, invocation expression or application expression.

+++

Example:

```{code-cell} ipython3
{"body": lambda π: π.x + (2 * π.y), 
 "parameters": ['x', 'y', 'z']};
```

## Confusing Terminology<a id="confusing"></a>

+++

When speaking of the body of a procedure, variables in the parameter list are called [___bound variables___](#bound-variables); confer `x` and `y` above.

+++

> __WARNING__: This terminology is confusing, but common.

+++

Parameters are not bound to values when the procedure is _defined_, only when the procedure is _called_ with actual arguments. Parameters are confusingly called _bound variables_ I suppose because it's just shorter than calling them "potentially bound variables" or "eventually bound variables."

+++

## Parameters, Arguments

+++

In ordinary Python, a call, invocation, or application looks like this:

```{code-cell} ipython3
(lambda x, y, z:  # <~~~ formal parameters
 x + 2*y)(
    40, 1, None)  # <~~~ actual arguments
```

The formal parameters are `x`, `y`, and `z`. The actual arguments are `40`, `1`, and `None`.

+++

> __DEFINITION__: ___Formal___ means "machine-checkable."

+++

Formal parameters are called "formal" because one can machine-check properties of the parameter list, such as absence of duplicated symbols.

+++

Not all parameters need be mentioned in the body. Vice versa, the body may mention variables that are not in the parameter list. Such variables are [___free variables___](#free-variables).

+++

In Schemulator, we write the code part of the example above as follows:

```{code-cell} ipython3
lambda π: π.x + (2 * π.y);
```

Notice that the unused parameter `z` is not mentioned. How do we know it exists? We might wait until the procedure is called with the wrong number of actual arguments, producing a run-time error. That's not great!

+++

To make all parameters explicit, we pair the body code with a list of formal parameters.

```{code-cell} ipython3
{"body": lambda π: π.x + (2 * π.y), 
 "parameters": ['x', 'y', 'z']};
```

When speaking of the parameters of a procedure or the arguments in an invocation of a procedure, we do not mean $\pi$. Rather, we mean the symbols bound in the environment $\pi$.

+++

### Ambiguous Language

+++

Sloppily, one says "a procedure of $n$ arguments" and really means "a procedure with $n$ parameters." Be aware of this ambiguity. Technically, we may say "a procedure _call_ with $n$ arguments," or "a procedure _invocation_ with $n$ arguments, or "a procedure _application_ with $n$ arguments."

+++

### Positional Arguments Only

+++

For now, unlike ordinary Python, Schemulator has only positional parameters, 1-to-1 with arguments. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the call has too many or too few arguments.

+++

### Anonymous versus Named

+++

Procedures need not have a name. In ordinary Python, contrast the named procedure `foo`

```{code-cell} ipython3
def foo(x):
    return x * x
foo(42)
```

against the anonymous procedure an identical body

```{code-cell} ipython3
(lambda x: x * x)(42)
```

By default, if Schemulator procedures have a name, that name is bound in the unique global environment, $\Gamma\Pi$. Nested definitions in non-global environments, as with `def` inside `def` in ordinary Python, are illustrated below.

+++

### Call Notation via `__call__`

+++

The `Procedure` class includes a `__call__` override for convenience. To test the call syntax, we need [`APPLY`](#apply). We'll prototype `APPLY` here, just printing actual arguments. [Later, in section `APPLY`](#apply), we implement the real work of binding parameters to arguments. That implementation requires [a codependent procedure, `EVAL`](#eval), which, in-turn, needs `APPLY`. We must get to mutual full definition of `APPLY` and `EVAL` in stages.

+++

The following definition of `Procedure` illustrates a general technique for defining codependent types. `Procedure` depends on `APPLY` and `APPLY` depends on `Procedure`. The technique is to write TBD types in string quotes in functions like `APPLY`.

```{code-cell} ipython3
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
```

## Examples:<a id="procedure-examples"></a>

+++

Following SICP 3.2.1, define `square` in the global environment and test `APPLY`.

```{code-cell} ipython3
setattr(  # Bind a variable ...
    ΓΠ.ϕ,  # ... in the frame of the global environment ...
    "square",  # ... a variable named "square" ...
    Procedure(  # ... to this Schemulator procedure.
        {"body": lambda π: π.x * π.x,
         "parameters": ['x']}))  # Don't forget the parameter list!
```

Test it! Remember, `APPLY`, for now, just prints the arguments of the applied procedure!

```{code-cell} ipython3
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

Test detection of duplicate parameters:

```{code-cell} ipython3
try: 
    setattr(
        ΓΠ.ϕ,
        "square",
        Procedure(
            {"body": lambda π: π.x * π.y,
             "parameters": ['x', 'x']}))
except ValueError as e:
    print(e.args)
```

## Function, Routine, Method

+++

> __DEFINITION__: A ___function___ is a mathematical object that associates input arguments to output values.

+++

The best way to think of a function is as a simple lookup table, where a key may appear no more than once.

+++

In Schemulator, a function is a special case of a procedure without side effects. Every invocation of a function with the same arguments produces the same result.

+++

> __DEFINITION__: ___Routine___ is a synonym for _Procedure_.

+++

> __DEFINITION__: A ___method___ is a procedure whose first argument is an object in ordinary Python.

+++

This definition hides the entire subject of object-oriented programming, only tangentially relevant here. One only needs to know that much of Schemulator is implemented in terms of Python's classes, objects, and methods.

+++

## Shortcut: $\Lambda$($\lambda$, params, $\pi$)

+++

The example procedure above has a name, "square", bound in the global environment. "Square" is not anonymous, but the `Procedure` value bound to the name "square" is anonymous.

+++

 $\Lambda$ is syntactical help to shorten definitions of anonymous procedures. Its default parameter list is empty and its default environment is the unique global environment $\Gamma{}\Pi$.

+++

Procedures need environments for looking up values of free and bound variables (in [the confusing sense highlighte above](#confusing)). To keep things easy, the next few examples have no free variables.

```{code-cell} ipython3
def Λ(body: "(π: Any) -> Any",
      parameters=None,  # default empty
      π = ΓΠ            # default global
) -> Procedure:
    ρ = Procedure(
        code={"body": body, 
              "parameters": parameters or []},
        π=π)
    return ρ
```

### Example:

+++

Test the $\Lambda$ syntax with the current `APPLY`, which just prints.

```{code-cell} ipython3
setattr(  # Give a name ...
    ΓΠ.ϕ,  # ... in the frame of the global environment ...
    "square",
    Λ(lambda π: π.x * π.x, ['x']))  # ... to this anonymous procedure
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

# Application(head, args, $\pi$)<a id="application"></a>

+++

> __DEFINITION__: An `Application` is an unevaluated object with a `Procedure` or symbol of type `str` and a list of unevaluated actual arguments.

+++

Do not confuse this _Application_, with a capital "A", with the word _application_ meaning a call or invocation of a procedure. An _Application_ with capital "A" is data representing a procedure call, invocation, or application. We test it later, after [`EVAL`](#eval) and [`APPLY`](#apply) are fully defined.

+++

`Application` is needed in [LET_STAR](#let-star) and related constructs to delay evaluation until the environment is established, where [EVAL](#eval) can evaluate them.

+++

`Application` is a placeholder for a more general [QUOTE](#quote) mechanism, which prevents evaluation in all cases (TODO). In real Scheme, `LET` and friends are syntactical forms that delay evaluation via Scheme macros, code rewriters. We prefer explicit delaying via `Application` objects at the semantical level, for clarity. An alternative is an embedded macro DSL (Domain-Specific Language) in Python, overkill here. If we go down that road, we might as well just implement Scheme altogether (TODO: reconsider).

+++

`Application` includes a `__call__` override for natural calling syntax in parentheses (round brackets).

```{code-cell} ipython3
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
```

## Shortcut: $\Xi$(head, args, $\pi$)

+++

Just as [Procedure](#procedure) has a [system-reserved Greek](#greek) shortcut, $\Lambda$, we make a Greek shortcut, $\Xi$, for `Application`.

```{code-cell} ipython3
Ξ = Application
```

Test later.

+++

# QUOTE, QUASIQUOTE, UNQUOTE, MACROS<a id="quote"></a>

+++

TODO

+++

# Var(sym)<a id="var"></a>

+++

In [applications](#application), we sometimes interpret strings as symbolic references to variables in an environment. The type that manages that need is `Var`. We test `Var` [after defining `EVAL`](#eval).

```{code-cell} ipython3
@dataclass
class Var:
    sym: str
```

# EVAL(expr, $\pi$)<a id="eval"></a>

+++

`EVAL` calls `APPLY` for applications of procedures. But `APPLY` calls `EVAL` on all arguments. To define `EVAL` before `APPLY` is fleshed out, we employ its earlier forward reference stub definition. We can't test `EVAL` until [`APPLY` is corrected, below](#apply).

+++

First, we correct `EVAL_APPLICATION`, [originally stubbed above](#application) to define the call syntax of `Application`. The first slot of an `Application` may contain

1. a string, [treated as a `Var` in the given environment](#var), that must evaluate to a procedure, or 

2. an explicit procedure

+++

To evaluate an `Application`, evaluate the procedure or string (implicitly a `Var`) in the first slot, then evaluate the arguments, then `APPLY` the procedure. 

```{code-cell} ipython3
def EVAL(expr: Any, π: Environment = ΓΠ, tag=None) -> Any:
    """forward reference, corrected below."""
    pass
def EVAL_APPLICATION(
        expr: Application,
        π: Environment = ΓΠ
) -> Any:
    """corrected definition; Treats 'head' a free variable in 
    an expression to recursively evaluate."""
    if isinstance(expr.head, str):  # 'head' is implicitly a Var.
        # 1/4. Evaluate first slot to find proc from string ...
        proc = π[expr.head]
        # ... yielding a procedure:
        assert isinstance(proc, Procedure), \
            f'The head of {expr} must be a string or a Procedure, ' \
            f'not a type({expr.head}) = {type(expr.head)}.'
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
```

`EVAL_PROCEDURE` splices environments for [free variables](#free-variables).

```{code-cell} ipython3
def EVAL_PROCEDURE(
        λ: Procedure,
        π: Environment = ΓΠ
) -> Procedure:
    λ.π = λ.π._splice_onto_lower(π)
    return λ
```

Many types other than `Application`, `Procedure`,  or `Var` evaluate to themselves. `EVAL` only needs cases for collections. [We test that below](#test-collections) after defining [`LET_STAR`](#let-star) and [`LET`](#let).

```{code-cell} ipython3
from typing import Any, Dict, Tuple, List
import numpy

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
```

## Test Var Lookup

+++

Earlier, we bound the little [Greek](#greek) system-test variable `γόὂ` in [section "Global Environment"](#global-environment).

```{code-cell} ipython3
EVAL(Var('γόὂ'))
```

Without `VAR`, strings are literal data.

```{code-cell} ipython3
EVAL('γόὂ')
```

# APPLY(proc, args, $\pi$)<a id="apply"></a>

+++

It's easier to mentally track environment chaining if one remembers that every procedure call creates a new environment for binding formal parameters to actual arguments.

+++

> __MEMORIZE__: `APPLY` makes a new environment parented in the given environment (default, $\Gamma\Pi$), then binds parameters in the new environment to actual arguments evaluated in the parent environment.

+++

Optimization: if a procedure has no parameters, `APPLY` doesn't make a new, empty environment.

+++

TODO: explore use cases where the environment given to `APPLY` is different from the environment of the procedure given to `APPLY`.

```{code-cell} ipython3
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
```

## Examples:

```{code-cell} ipython3
APPLY(ΓΠ.square, [42])
```

Call via "Pythonic" round brackets:

```{code-cell} ipython3
ΓΠ.square(42)
```

Works on anonymous procedures, too:

```{code-cell} ipython3
Λ(lambda π: π.x * π.x, ['x'])(42)
```

Test multiple parameters and arguments:

```{code-cell} ipython3
Λ(lambda π: π.x * ECHO('π', π).y, ['x', 'y'])(8, 7)
```

The bound variables in the procedures, for example, `x`, don't leak: they are not bound in $\Gamma\Pi$:

```{code-cell} ipython3
try:
    ΓΠ.x
except Exception as e:
    print(e.args)
```

## Test `Application`

+++

Recall that an `Application` is a data object that represents a procedure applied to actual arguments.

```{code-cell} ipython3
ωfoo = Application(ΓΠ.square, [42])
```

Notice that the parameters are _not yet bound to values_, not until the `Application` is evaluated, that is, until call-time of the procedure in the `Application`.

```{code-cell} ipython3
ECHO('ωfoo', ωfoo);
```

To perform the procedure call, `EVAL` the `Application`.

```{code-cell} ipython3
EVAL(Application(ΓΠ.square, [42]))
```

 Internally, `EVAL` calls `EVAL_APPLICATION`. The results are identical.

```{code-cell} ipython3
EVAL_APPLICATION(Application(ΓΠ.square, [42]))
```

We prefer `EVAL` most of the time because it's shorter.

+++

Test the [Greek](#greek) shortcut $\Xi$ for `Application`, now treating the string in the procedure slot implicitly as a `Var` looked up in $\Gamma\Pi$:

```{code-cell} ipython3
EVAL(Ξ('square', [42]))
```

`Applications` require explicit `Vars` to help with actual arguments looked up in $\Gamma\Pi$:

```{code-cell} ipython3
EVAL(Ξ('square',  # find proc in global env
       [Var('γόὂ')]))  # find binding in global env
```

However, parameters of enclosing $\lambda$s are automatically looked up as if they were `Var`s:

```{code-cell} ipython3
Λ(lambda π:
  # body of enclosing lambda:
  EVAL(
      Ξ(ΓΠ.square,  # a Procedure
        [Var('ϕοοβαρ')]),  # an actual argument
      ECHO('π', π)),  # environment where to find actual arg
  ['ϕοοβαρ'])(  # formal parameter of enclosing lambda
    43)  # actual argument of call of enclosing lambda
```

If you forget the `Var`, you will try to multiply a `str` times an `int` (the exception automatically converts the `str` to a sequence, explaining "sequence" in the error message):

```{code-cell} ipython3
try:
    Λ(lambda π:
      # body of outer lambda
      EVAL(Ξ('square', ['ϕοοβαρ']), π),  # actual argument
      ['ϕοοβαρ'])(  # formal parameter of outer lambda
        42)  # actual argument of call of outer lambda
    
except TypeError as e:
    print(e.args)
```

`Applications` may have explicit procedures, instead of strings, in their first slot:

```{code-cell} ipython3
EVAL(
    Ξ(Λ(lambda π: π.x * π.x, ['x']), 
      [Var('γόὂ')]  # find binding in global env
     ))
```

$\Xi$ honors sub-environments even when looking up procedures and arguments by their symbolic names.

```{code-cell} ipython3
Λ(lambda π:
  EVAL(
      Ξ('square',  # Look me up in ΓΠ.
        [Var('ϕοοβαρ')]),  # Look me up in π.
      π),
  ['ϕοοβαρ'])(  # formal parameter of the Λ
    42)  # actual argument of implied call of APPLY
```

$\phi\omicron\omicron\beta\alpha\rho$ is not bound in the enclosing global environment $\Gamma\Pi$, as we see by attempting the `EVAL` in $\Gamma\Pi$:

```{code-cell} ipython3
try:
    Λ(lambda π:
      EVAL(
          Ξ('square', 
            [Var('ϕοοβαρ')]), 
          ΓΠ),  # <~~~ wrong env
      ['ϕοοβαρ'])(
        42)
except NameError as e:
    print(e.args)
```

# DEFINE(sym, val, $\pi$)<a id="define"></a>

+++

Package up the "defining" boilerplate.

+++

By default, `DEFINE` binds symbols in $\Gamma\Pi$. The "return value" is consistent with Gambit Scheme, which doesn't return anything from a `define`.

```{code-cell} ipython3
def DEFINE(
        sym: str, 
        val: Any, 
        π: Environment=ΓΠ  # default
) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
    return val
```

## Examples:

+++

Do some fancy stuff:

```{code-cell} ipython3
import numpy
ΓΠ.square(numpy.array([[3, 4],[1, 2]]))
```

```{code-cell} ipython3
DEFINE(
    'saxpy', 
    Λ(lambda π: 
      # Fancy!
      numpy.dot(π.a, π.x) \
      if isinstance(π.a, numpy.ndarray) 
      and isinstance (π.x, numpy.ndarray) \
      # Regular
      else π.a * π.x + π.y, 
      ['a', 'x', 'y']));
```

```{code-cell} ipython3
import numpy
ΓΠ.saxpy(numpy.array([[1, 2, 3], [4, 5, 6]]),
         numpy.array([[7], [11],[13]]),
         numpy.array([[42], [43]]))
```

or just some ordinary stuff:

```{code-cell} ipython3
ΓΠ.saxpy(4, 10, 2)
```

## SICP 3.2.2<a id="sicp-322"></a>

```{code-cell} ipython3
DEFINE('square', 
       Λ(lambda π: π.x * π.x, ['x']))

DEFINE('sum_of_squares',
       Λ(lambda π: π.square(π.x) + π.square(π.y), ['x', 'y']))

DEFINE('f',
       Λ(lambda π: π.sum_of_squares(1 + π.a, 2 * π.a), ['a']))

ΓΠ.f(5)
```

Having `f` defined will bother us below. Get rid of it now.

```{code-cell} ipython3
try:
    del ΓΠ.ϕ.f
except:
    pass
```

## SICP Exercise 3.9

```{code-cell} ipython3
DEFINE('factorial',
       Λ(lambda π: 1 if π.n < 2 else \
         π.n * π.factorial(π.n - 1), ['n']))

ΓΠ.factorial(6)
```

This next example doesn't tail-recurse because Python does not tail-recurse. We mitigate that in section [Tail Recursion](#tail-recursion).

```{code-cell} ipython3
DEFINE('fact_iter',
       Λ(lambda π: π.product if π.counter > π.max_count else \
         π.fact_iter(
           π.counter * π.product,
           π.counter + 1,
           π.max_count
           ), ['product', 'counter', 'max_count']));

ΓΠ.fact_iter(1, 1, 6)
```

# Procedures that Apply Procedures

+++

Here is a procedure of `f` and `x` that applies `f` to `x`. Bind `f` to `square`, `x` to 42, then invoke `f`.

```{code-cell} ipython3
# Outer parens necessary to break lines for comments (Python syntax booger).
(Λ(lambda E1:     # Calling this λ chains environment E1 to ΓΠ.
  E1.f(E1.x),     # Apply E1.f to E1.x.
  ['f', 'x'])     # formal parameters
(ΓΠ.square, 42))  # <~~~ Bind f to square, x to 42.
```

Here is a procedure that applies an internal anonymous procedure of `n` in `E2`. The outer procedure of `m` in `E1` is rooted in $\Gamma\Pi$.

```{code-cell} ipython3
Λ(lambda E1:      # Calling this λ chains environment E1 to ΓΠ.
  Λ(lambda E2:    # Calling this λ chains environment E2 to ΓΠ.
    E2.n * E2.n,  # <~~~ n is bound in E2;
    ['n']         #      E2 is sibling to E1.
   )(             # Parent environment implicitly ΓΠ.
     E1.m),       # <~~~ invocation; Look up m in E1, bind to n in E2.
  ['m'])(42)      # <~~~ Bind m to 42 in E1.
```

The inner environment, `E2` is not explicitly chained to `E1`. In the next example `m` is a [free variable](#free-variable) in the inner $\lambda$. It is not found in `E2`:

```{code-cell} ipython3
try:
    Λ(lambda E1:      # Calling this λ chains environment E1 to ΓΠ.
      Λ(lambda E2:    # Calling this λ chains environment E2 to ΓΠ.
        E2.n * ECHO('E2', E2).m,  # <~~~ n is bound in E2; m is not bound in E2.
        ['n']         #      E2 is sibling to E1.
       )(             # Parent environment implicitly ΓΠ.
          E1.m),      # <~~~ invocation. Look up m in E1, bind to n in E2.
      ['m'])(42)      # <~~~ Bind m to 42 in E1.
except NameError as e:
    print(e.args)
```

That's not the default behavior in Scheme. Scheme `define` _automatically_ chains environments static closures, in which free variables are bound.

+++

Here's a Scheme example that calls an automatically chained closure:

```{raw-cell}
> (define foo (lambda (m) ((lambda (n) (* n m)) m)))
> (foo 42)
1764
```

How do we emulate this? Schemulator requires an explicit chaining parameter because Schemulator does not know there are free variables in the body of the $\Lambda$. Schemulator does not know because Schemulator does not parse, on purpose, [as explained in the introduction](#semantics). If Schemulator were Scheme, and thus parsing, it could determine that free variables occur in the body and then it could automatically chain environments. The code presented below would appear in an implementation of Scheme, but the chaining of environments would be syntactically hidden.

+++

Here's the example above in Schemulator:

```{code-cell} ipython3
Λ(lambda E1:      # Calling it creates environment E1 in ΓΠ.
  Λ(lambda E2:    # !!!! Define in E1 !!!!
    E2.n * E2.m,  # <~~~ n in E1, x in E2.
    ['n'],        #      (E2 is child of E1, written E1<--E2)
    E1)(          # !!!! Parent environment *explicitly* E1 !!!!
         E1.m),   # <~~~ Look up n in E1, bind x in E2->E1
  ['m'])(42)      # <~~~ Bind n to 42 in E1
```

Here's a Scheme example that returns a closure and then calls it externally. This also happens to be an example of [_currying_](#thunk).

```{raw-cell}
> (define foo (lambda (m) (lambda (n) (* n m))))
> ((foo 42) 42)
1764
```

Here's the same example in Schemulator. The outer procedure returns the inner closure and the `ECHO` displays the chain of environments.

```{code-cell} ipython3
Λ(lambda E1:
  Λ(lambda E2:
    E2.n * ECHO('E2', E2).m,
    ['n'],
    E1),
  ['m'])(42)(42)
```

## Static Closure

+++

Static [closures](#closures) are created at define-time. In the next example, we assign a static closure to a Python variable, `foo`, then invoke it.

```{code-cell} ipython3
foo = Λ(lambda E1:
        Λ(lambda E2:  # This Λ ...
          E2.n * ECHO('E2', E2).m,
          ['n'],
          E1)  # <~~~ is defined in E1
        (E1.m),
        ['m'])
foo(42)
```

## Dynamic Closure

+++

If we explicitly `EVAL` the inner procedure of `E2` in the environment `E1` of the outer procedure, the evaluator dynamically splices the environments. The downside of this approach is that evaluation of the closure, `bar`, splices the environments on every invocation. The upside is that it lets us create closures at run time from unclosed or partially closed procedures.

```{code-cell} ipython3
bar = Λ(lambda E1:
        EVAL(
            Λ(lambda E2:  # <~~~ This Λ is ...
              E2.n * ECHO('E2', E2).m,
              ['n']
             ),  # <~~~ ... defined in global, and ...
            E1)  # <~~~ ... evaluated in E1; envrmt spliced.
        (E1.m),
        ['m'])
bar(42)
```

Scheme does not support dynamic closures, i.e., re-defining environments at run time. In the following, there is no way for `m` to acquire a binding:

```{raw-cell}
> (define foo (lambda (n) (* n m))) ;; m can never be bound!
> foo
#<procedure #5 foo>
> ((lambda (m) (foo 42)) 43)
*** ERROR IN foo, (console)@8.28 -- Unbound variable: m
```

If we do both, defining _and_ evaluating the inner $\lambda$ in E1, we get only one copy of E1 containing `m`. Dynamic splicing in [`Environment`](#environment) checks this case and optimizes it away:

```{code-cell} ipython3
Λ(lambda E1:        
  EVAL(               # EVAL the inner procedure itself ....
      Λ(lambda E2:  
        E2.n * ECHO('E2', E2).m,  # <~~~ n and m are bound in E2;
        ['n'], E1         # !!!! explicitly defined in outer E1
       ), E1)         # .... in the outer environment E1.
  (E1.m),             # <~~~ Look up m in E1, bind to n in E2.
  ['m'])(42)          # <~~~ Bind m to 42 in E1.
```

## Shadowing

+++

Because the two variables `m` and `n` are in different environments, they can have the same name. `E2.n` ___shadows___ `E1.n`, which is inaccessible from `E2`.

```{code-cell} ipython3
Λ(lambda E1:
  Λ(lambda E2:
    E2.n * ECHO('E2', E2).n,
    ['n'],
   E1)
  (ECHO('E1', E1).n),
  ['n'])(42)
```

# Procedures that Return Procedures

+++

The outer $\Lambda$ below is the identity function: it returns its argument. It's applied to `ΓΠ.square`, returning it, and the result is applied to 42:

```{code-cell} ipython3
Λ(lambda E1:  # Calling it creates environment E1 in ΓΠ.
  E1.f,       # Just return the value of parameter f.
  ['f'])(     # Parent environment is implicitly ΓΠ.
 ΓΠ.square)(  # <~~~ 'square' is bound in ΓΠ.
 42)          # Apply the returned procedure.
```

Return a fresh anonymous procedure:

```{code-cell} ipython3
Λ(lambda π: π.f, ['f'])(             # identity function as above ...
    Λ(lambda π: π.x * π.x, ['x']))(  # ... applied to anonymous procedure;
42)                                  # Apply the returned procedure.
```

# Thunks and Currying<a id="thunk"></a>

+++

> __DEFINITION__: A ___thunk___ is a procedure of no arguments.

+++

> __DEFINITION__: A ___1-thunk___ is a procedure of one argument. 1-thunks are the only kind of procedure, and they're all pure functions, with no side-effects.

+++

> __DEFINITION__: A ___curried___ function is a procedure of many arguments transformed into a composition of 1-thunks.

+++

For example, in ordinary Python, the following $\lambda$ of `x` and `y`:

```{code-cell} ipython3
(lambda x, y: x + y**2)(43, 42)
```

is curried into the following: a $\lambda$ of `x` returning a $\lambda$ of `y`:

```{code-cell} ipython3
(lambda x: (lambda y: x + y**2))(43)(42)
```

Here are these examples in Schemulator.

```{code-cell} ipython3
Λ(lambda π: π.x + π.y**2, ['x', 'y'])(43, 42)
```

```{code-cell} ipython3
Λ(lambda πo: 
  Λ(lambda πi: πi.x + πi.y**2, 
    ['y'], 
    πo),  # <~~~ explicit chaining
  ['x'])(43)(42)
```

Notice that `x` occurs [free](#free-variables) in the inner $\lambda$ but [bound](#bound-variables) in the outer $\lambda$. As always, be wary of [the confusing terminology "bound variable"](#confusing). Also notice that the curried form requires two invocations, one for each formal parameter of the two 1-thunks.

+++

The name "curried" comes from Haskell Curry, who developed the theory of analyzing all functions as 1-thunks. Haskell Curry also gave his name to the World's most prominent pure functional programming language, Haskell.

+++

In many examples below, we will work with curried functions.

+++

# Partial Evaluation and Automatic Currying

+++

TODO

+++

# $\Upsilon$: Squaring Square Roots of Functions

+++

Anonymous recursive procedures are fundamental.

+++

> __OBSERVATION__: The Ultimate Imperative requires anonymous tail-recursive procedures.

+++

We develop them fully in the following few sections.

+++

[See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md) for detailed explanations of the following development.

+++

The running example is non tail-recursive factorial.

+++

Don't forget non-default `πsf`, lest `sf` be unbound in the inner $\lambda$. `sf` is the "square root" of the recursive function we want, square-root in an abstract algebraic sense where function application is multiplication:

```{code-cell} ipython3
Λ(lambda πsf:  # <~~~ Apply this Λ ...
  Λ(lambda πn: 
    # Observe the "multiplication" sf(sf):
    1 if πn.n < 1 else πn.n * πn.sf(πn.sf)(πn.n - 1), 
    ['n'], πsf), 
  ['sf'])(  # <~~~ ... to a copy of itself.
    Λ(lambda πsf:  # <~~~ bind this Λ to upper 'sf'
      Λ(lambda πn: 
        1 if πn.n < 1 else πn.n * πn.sf(πn.sf)(πn.n - 1), 
        ['n'], πsf), 
      ['sf']))(6)
```

Abstract `sf(sf)(m)` into a `f`, a delayed $\lambda$ of `m`. The environments are chained through every level, as exhibited by the `ECHO`:

```{code-cell} ipython3
Λ(lambda πsf:
  Λ(lambda πf:  # Domain code d is a function of business code f.
    Λ(lambda πn:  # Business code f is a function of business parameter n.
      1 if πn.n < 1 else πn.n * πn.f(πn.n - 1), 
      ['n'], πf), 
    ['f'], πsf)(
      Λ(lambda πm:  # Delayed application of business code sf(sf).
        πm.sf(πm.sf)(ECHO('πm', πm).m), 
        ['m'], πsf)), 
  ['sf'])(  # <~~~ Apply to copy of self:
Λ(lambda πsf:
  Λ(lambda πf:  # Domain code d is a function of business code f.
    Λ(lambda πn:  # Business code f is a function of business parameter n.
      1 if πn.n < 1 else πn.n * πn.f(πn.n - 1), 
      ['n'], πf), 
    ['f'], πsf)(
      Λ(lambda πm:  # Delayed application of business code sf(sf).
        πm.sf(πm.sf)(πm.m), 
        ['m'], πsf)), 
  ['sf']))(6)
```

Abstract the ___domain code___ into `d`, a function of the ___business code___ `f`, which is, in-turn, a function of the ___business parameter___ `n`.

```{code-cell} ipython3
Λ(lambda πd:
  Λ(lambda πsf:  # sf; copy this Λ below ...
    πd.d(Λ(lambda πm: πm.sf(πm.sf)(ECHO('πm', πm).m),
          ['m'], πsf)),
    ['sf'], πd)(  # <~~~ "squaring," i.e., self-application
      Λ(lambda πsf:  # sf; .... right here
        πd.d(Λ(lambda πm: πm.sf(πm.sf)(πm.m), 
              ['m'], πsf)),
        ['sf'], πd)), 
  ['d'])(  # d is the formal parameter for domain code
    Λ(lambda πf:  # Domain code d is a function of business code f.
      Λ(lambda πn:  # Business code f is a function of business parameter n.
        1 if πn.n < 1 else πn.n * πn.f(πn.n - 1), 
        ['n'], πf),  # n: business parameter
      ['f'])  # square of sf, recursive function
    )(6)
```

Abstract the squaring (self-application) into `g`:

```{code-cell} ipython3
Λ(lambda πd:  # function of domain code, d
  Λ(lambda πg:  # generic squaring gizmo 
    πg.g(πg.g), 
    ['g'], πd)(
      Λ(lambda πsf: 
        πd.d(Λ(lambda πm: πm.sf(πm.sf)(πm.m), 
               ['m'], πsf)),
        ['sf'], πd)), 
  ['d'])(  # formal parameter d for domain code
    Λ(lambda πf:  # Domain code d is a function of business code f.
      Λ(lambda πn:  # Business code f is a function of business parameter n.
        1 if πn.n < 1 else πn.n * πn.f(πn.n - 1), # business code
        ['n'], πf),  # n: business parameter
      ['f'])  # square of sf, recursive function
)(6)
```

## Recursive Factorial via $\Upsilon{}1$

+++

The glyph that looks like "Y" is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). Names in user code should not collide with it if users remember to [avoid Greek](#greek).

+++

Package into a system function, $\Upsilon{}1$, for later use. The "1" in the name means that this is domain codes that return [1-thunk](#thunk) business codes, procedures of one parameter:

```{code-cell} ipython3
DEFINE('Υ1', 
       Λ(lambda πd: # function of domain code, d
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of one parameter
             Λ(lambda πsf: 
               πd.d(Λ(lambda πm: πm.sf(πm.sf)(πm.m), 
                      ['m'], πsf)),
               ['sf'], πd)), 
         ['d']))
```

### Example:

```{code-cell} ipython3
DEFINE('fact_recursive',
      Λ(lambda π: # domain code; function of business code, f
        Λ(lambda π: 
          1 if π.n < 1 else π.n * π.f(π.n - 1), # business code
          ['n'], π), # 1 business parameter, n
        ['f'])) # recursive function

ΓΠ.Υ1(ΓΠ.fact_recursive)(6)
```

## Iterative Factorial via $\Upsilon{}3$<a id="iterative-factorial"></a>

+++

$\Upsilon$ can be tailored for a given number of business parameters. This one is for three.

```{code-cell} ipython3
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
         ['d']));
```

Later, we generalize $\Upsilon$ to any number $N$ of business parameters.

+++

Here is user-level domain code, redefining `fact_iter` in domain-code form. Any domain code is a function of `f`, recursive business code. In this case, `f` is a function of 3 business parameters. This will get us to a tail-recursive solution in the [section on tail recursion](#tail-recursion).

+++

### Example:

```{code-cell} ipython3
# λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter', # domain code is a function of f ...
       Λ(lambda π: # ... which is business code.
         Λ(lambda π: 
           π.m 
           if π.c > π.x 
           else π.f(π.m * π.c, π.c + 1, π.x), # business code
           ['m', 'c', 'x'], π), # business parameters
         ['f']));

ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 6)
```

# Tail Recursion<a id="tail-recursion"></a>

+++

> __OBSERVATION__: The Ultimate Imperative requires anonymous _tail-recursive_ procedures.

+++

Thanks to [Thomas Baruchel for the idea of Exceptions to implement tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion).

+++

If users are aware that their domain code is tail-recursive, then they may call it via `LOOP` instead of via $\Upsilon$.

+++

Scheme detects tail recursion automatically. Tt parses user code and marks un-nested tail calls. Schemulator doesn't parse, [on purpose](#semantics), and [Python infamously avoids tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). In Python and Schemulator, users must invoke tail recursion explicitly. This isn't terrible. Tail-calls are lexically obvious (un-nested self-calls), so users should always know. In Clojure, there is precedent: users explicitly write `loop` and `recur`. In any event, tail-recursive domain code can always be called via non-tail-recursive $\Upsilon$.

+++

## `LOOP3`

+++

`LOOP3` has the same signature as $\Upsilon3$; it takes domain code of business code of three arguments as its sole argument.

+++

The glyph that looks like "P" below is Greek Capital Rho for "recur." Names in user code will not collide with P if users remember to [avoid Greek](#greek). As with $\Upsilon$, Rho and `LOOP` must know their argument counts. That's OK for now (TODO: reconsider).

+++

> __WARNING__: ***Results are undefined if any `LOOP` function is called with non-tail-recursive domain code.***

```{code-cell} ipython3
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
```

## Example:

```{code-cell} ipython3
LOOP3(ΓΠ.fact_iter)(1, 1, 6)
```

## Prove It on `fact_iter`

+++

The recursive call via $\Upsilon$ blows Python's recursion limit.

```{code-cell} ipython3
try:
    print(ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

Call via `LOOP3` does not. Notice the domain code `fact_iter` is EXACTLY the same as in the recursive call above.

```{code-cell} ipython3
try:
    print(LOOP3(ΓΠ.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

## Tail-Recursive Fibonacci<a id="tail-recursive-fibonacci"></a>

+++

Write domain code for catastropically slow, non-tail-recursive, exponentially diverging Fibonacci:

```{code-cell} ipython3
DEFINE('fib_slow', 
       Λ(lambda π: 
         Λ(lambda π: 1 if π.n < 2 else 
           π.f(π.n - 1) + π.f(π.n - 2), ['n'], π),
         ['f']))

ΓΠ.Υ1(ΓΠ.fib_slow)(6)
```

This is miserable even for $n=23$. Don't call it for bigger arguments.

```{code-cell} ipython3
ΓΠ.Υ1(ΓΠ.fib_slow)(23)
```

The following takes 10 seconds. Uncomment if you want to see the time per iteration: about 1,000 ms; YES, a full second!

```{code-cell} ipython3
# timeit(ΓΠ.Υ1(ΓΠ.fib_slow)(23))
```

Without linearization, Fibonacci 500 would not complete in $10^{30}$ times the Age of the Universe. One way to linearize is tail recursion. Another way is [memoization](#memoization) (_sic:_ not _memorization_).

+++

Tail-recursive memoization is possible but not necessary. A tail-recursive Fibonacci easy and blazingly fast:

```{code-cell} ipython3
DEFINE('fib_iter',
       Λ(lambda π:
         Λ(lambda π: π.b if π.n < 1 else 
           π.f(π.b, π.a + π.b, π.n - 1),
           ['a', 'b', 'n'], π),
         ['f']));
```

Check it:

```{code-cell} ipython3
LOOP3(ΓΠ.fib_iter)(0, 1, 23)
```

Time it.

+++

The following takes 10 seconds. Uncomment if you want see 250 _micro_ seconds, or so, 4000 times faster on this argument, 23. Exponentially faster on bigger arguments.

```{code-cell} ipython3
# timeit(LOOP3(ΓΠ.fib_iter)(0, 1, 23))
```

Stress it, remembering that the non-tail-recursive version would not complete in astronomical time:

```{code-cell} ipython3
LOOP3(ΓΠ.fib_iter)(0, 1, 500)
```

# Memoized [sic] Fibonacci<a id="memoization"></a>

+++

Linearize execution of Fibonacci by recording intermediate results in a memo table instead of recomputing them. This is an easy instance of [_Dynamic Programming_](https://en.wikipedia.org/wiki/Dynamic_programming).

+++

## Curried Memo Table

+++

One way to pass a memo table is through a Curried second argument. We'll need $\Upsilon2C$, generic for 2-parameter, Curried business code:

```{code-cell} ipython3
DEFINE('Υ2C', 
       Λ(lambda π: # function of domain code, d ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # with business code of 2 parameters, curried
             Λ(lambda π: 
               π.d(Λ(lambda π: 
                     Λ(lambda π: π.sf(π.sf)(π.m)(π.n), 
                       ['n'], π), ['m'], π)),
               ['sf'], π)), 
         ['d']));
```

Notice that we eased our burden of writing this $\Upsilon$ by not bothering to distinguish the environment parameters $\pi$ by name. Ordinary scoping rules disambiguate them, affording us a more fluid and concise style. 

+++

The domain code for a memoized, Curried Fibonacci follows. The parameter `a` is the _accumulator_, _associator_, or memo table, whatever word you like best. This is easiest to read (and to write) from the bottom up. It looks horrendous, but it isn't really. We disambiguated the names of the environment parameters as a guide. We won't do so in the future. In the `ECHO` output, count the 13 levels of chained environments.

```{code-cell} ipython3
DEFINE('fib_fast',
       Λ(lambda πf: # of f; level 1
         Λ(lambda πa: # of a; level 2
           Λ(lambda πn: # of n; level 3
             (πn.a, 1) if πn.n < 2 else
             Λ(lambda πn1: # of n_1; level 4
               (πn1.a, πn1.a[πn1.n_1]) # optimizer should remove these two lines
               if πn1.n_1 in πn1.a else # ^^^
               Λ(lambda πfim1: # of fim1; level 5
                 Λ(lambda πm1: # of m1; level 6
                   Λ(lambda πr1: # of r1; level 7
                     Λ(lambda πa1: # of a1; level 8
                       Λ(lambda πn2: # of n_2; level 9
                         (πn2.a1, πn2.r1 + πn2.a1[πn2.n_2]) # <~~~ a quick exit
                         if πn2.n_2 in πn2.a1 else 
                         Λ(lambda πfim2: # of fim2; level 10
                           Λ(lambda πm2: # of m2; level 11
                             Λ(lambda πr2: # of r2; level 12
                               Λ(lambda πa2: # of a2; level 13
                                 (ECHO('πa2', πa2).a2, πa2.r1 + πa2.r2), # <~~~ the money line
                                 ['a2'], πr2)(πr2.m2[0] | {πr2.n_2: πr2.r2}),  # <~~~ update memo
                               ['r2'], πm2)(πm2.m2[1]), # unpack
                             ['m2'], πfim2)(πfim2.fim2(πfim2.n_2)), # unpack
                           ['fim2'], πn2)(πn2.f(πn2.a1)), # <~~~ recurse
                         ['n_2'], πa1)(πa1.n - 2), # DRY
                       ['a1'], πr1)(πr1.m1[0] | {πr1.n_1: πr1.r1}), # <~~~ update memo
                     ['r1'], πm1)(πm1.m1[1]), # unpack
                   ['m1'], πfim1)(πfim1.fim1(πfim1.n_1)), # unpack
                 ['fim1'], πn1)(πn1.f(πn1.a)), # <~~~ recurse
               ['n_1'], πn)(πn.n - 1), # DRY
             ['n'], πa), # business parameter
           ['a'], πf), # curried memo
         ['f'])) # domain code 
ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1]
```

It's about 1 millisecond per iteration, 1,000 times faster than the original. The following takes 10 seconds. Uncomment if you want proof.

```{code-cell} ipython3
# timeit(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1])
```

Still blows the recursion limit:

```{code-cell} ipython3
try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(200)[1])
except RecursionError as e:
    print(e.args)
```

We fix that immediately below.

+++

## Memo Table as Business Parameter

+++

Before doing tail-recursion with a memo table, show the memo as un-Curried. Currying is useful in general, but complicates $\Upsilon$. Get rid of it. Notice, also, that we no longer bother to disambiguate the environment parameters, letting Nature take care of it.

```{code-cell} ipython3
DEFINE('fib_fast_uncurried',
      Λ(lambda π: # of f; level 1
        Λ(lambda π: # of a, n; level 2
          (π.a, 1) if π.n < 2 else
          Λ(lambda π: # of n_1; level 3
            Λ(lambda π: # of t1; level 4
              Λ(lambda π: # of m1; level 5
                Λ(lambda π: # of r1; level 6
                  Λ(lambda π: # of a1; level 7
                    Λ(lambda π: # of n_2; level 8
                      (π.a1, π.r1 + π.a1[π.n_2]) # <~~~ quick exit
                      if π.n_2 in π.a1 else 
                      Λ(lambda π: # of t_2; level 9
                        Λ(lambda π: # of m_2; level 10
                          Λ(lambda π: # of r_2; level 11
                            Λ(lambda π: # of a_2; level 12
                              (π.a2, π.r1 + π.r2), # <~~~ the money line
                              ['a2'], π)(π.m2 | {π.n_2: π.r2}), # <~~~ update memo
                            ['r2'], π)(π.t2[1]), # nupaci
                          ['m2'], π)(π.t2[0]), # unpack
                        ['t2'], π)(π.f(π.a1, π.n_2)), # <~~~ recurse
                      ['n_2'], π)(π.n - 2), # dry
                    ['a1'], π)(π.m1 | {π.n_1: π.r1}), # <~~~ update memo
                  ['r1'], π)(π.t1[1]), # unpac
                ['m1'], π)(π.t1[0]), # unpack
              ['t1'], π)(π.f(π.a, π.n_1)), # <~~~ recurse
            ['n_1'], π)(π.n - 1), # DRY
          ['a', 'n'], π), # busines parameters
        ['f'])); # domain-code signature
```

Now need only an ordinary $\Upsilon2$ to call it:

```{code-cell} ipython3
DEFINE('Υ2', 
       Λ(lambda π: # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of two parameters
             Λ(lambda π: 
               π.d(Λ(lambda π: π.sf(π.sf)(π.m, π.c), 
                     ['m', 'c'], π)), 
               ['sf'], π)), 
         ['d']));
```

The recursion limit is a little higher, but we don't want any of that.

```{code-cell} ipython3
try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 200)[1])
except RecursionError as e:
    print(e.args)
```

```{code-cell} ipython3
try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 250)[1])
except RecursionError as e:
    print(e.args)
```

This code is not tail-recursive, because it sums the results of two tail-recursive calls. An attempt to call it through a specialization of `LOOP` produces incorrect results, as warned above:

```{code-cell} ipython3
def LOOP2(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    nyms = ['a', 'β']
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
```

```{code-cell} ipython3
try:
    print(LOOP2(ΓΠ.fib_fast_uncurried)({}, 250))
except RecursionError as e:
    print(e.args)
```

## Recursive With Memo

+++

Section [Tail-Recursive Fibonacci](#tail-recursive-fibonacci) exhibits a very short solution without a memo table. Could we write a tail-recursive version with a memo table? Is it worth the effort? Perhaps as a mental exercise. It grows into a function of 5 parameters:

+++

Pseudocode:

+++

```
f(r1, r2, a, n, x):
    (a, 1) if n < 1 else
    f(r2, r1 + r2, a | {x - (n-1): r1, x - (n-2): r2}, x)
```

+++

### Non-Tail-Recursive

```{code-cell} ipython3
DEFINE('Υ5', 
       Λ(lambda π: # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of five parameters
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m, π.c, π.x, π.a, π.b), 
                   ['m', 'c', 'x', 'a', 'b'], π)), 
               ['sf'], π)), 
         ['d']));
```

```{code-cell} ipython3
DEFINE('fib_tc_memo',
      Λ(lambda π: 
        Λ(lambda π:
          (π.a | {π.x: π.r2}, π.r2) if π.n < 1 else \
          π.f(π.r2, π.r1 + π.r2, 
              π.a | {π.x-π.n: π.r2},
              π.n - 1,
              π.x),
         ['r1', 'r2', 'a', 'n', 'x'], π), 
        ['f']));
```

```{code-cell} ipython3
ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)
```

+++ {"tags": []}

### Tail-Recusive

```{code-cell} ipython3
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
```

```{code-cell} ipython3
LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)
```

### Test the Limits

```{code-cell} ipython3
try:
    print(ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)    
```

```{code-cell} ipython3
try:
    print(LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)
```

# `LOOP` and $\Upsilon$ of $N$ Business Parameters

+++

We finish off [PART 1: BASICS](#basics) with straightforward generalizations of `LOOP` and $\Upsilon$ to any number of business parameters, without undue explanation. We test them below in the section on `DO`.

```{code-cell} ipython3
def LOOPN(
        d: Procedure, 
        vars_: List[str]  # <~~~ NOTA BENE, extra parameters to LOOPN
) -> Procedure:
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
```

```{code-cell} ipython3
DEFINE('ΥN',
       Λ(lambda πd:  # of d, the domain code and vars ...
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(
             # of business code of N parameters
             Λ(lambda πsf:
               πd.d(Λ(lambda πvs:
                      πvs.sf(πvs.sf)(
                          *[πvs[var] for var in πvs.vars_]),
                      πsf.vars_,  # <~~~ list of parameters
                      πsf)),
               ['sf'], πd)),
         ['d', 'vars_']));
```

# PART II: IMPERATIVES<a id="imperatives"></a>

+++

Everything above is Schemulator set-up. We finally get to the Pynultimate Imperatives.

+++

# SET_BANG<a id="set-bang"></a>

+++

This is assignment without lambda. We do better later, but SICP and _Lambda the Ultimate Imperative_ use it. Implement it just to complete coverage of Chapter 3 of SICP.

+++

This has its own recursive lookup, exactly the same as that in [`Procedure`](#procedure), just for a different purpose.

+++

Tested below in [BLOCK](#block). Like Gambit and unlike Common Lisp and Emacs Lisp, we can only `set!` symbols that are already `define`d.

```{code-cell} ipython3
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
```

# BLOCK / BEGIN<a id="block"></a>

+++

Sequenced execution is not fundamental, but sequential dependence is fundamental. To simulate sequential execution, chain calls of sequentially dependent $\lambda$s. Feed the result of each call into the single argument of the next.

+++

In the following implementation, every $\lambda$ must be a [***thunk***](#thunk): a procedure of no arguments (our $\lambda$s always have the conventional parameter $\pi$; our thunks do not use any parameters bound in $\pi$). All but the last thunk are for side-effect; all thunks are evaluated; all but the last value are discarded.

+++

Steele's paper calls this form `BLOCK`. Scheme calls it `BEGIN`. Common Lisp calls it `PROGN`.

```{code-cell} ipython3
def BLOCK(
        *ss: "Procedure | Procedure",  # <~~~ PEP 438 type notation
        π: Environment = ΓΠ
) -> Any:
    ρ = None
    for s in ss:
        ρ = APPLY(s, [], π=π)  # <~~~ thunks take no args
    return ρ
```

## Examples:

+++

This block first sets `x` in $\Gamma\Pi$ to 6 and then accesses that variable:

```{code-cell} ipython3
DEFINE('x', 0)  # <~~~ Binding must preexist in ΓΠ for SET_BANG
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)), 
    Λ(lambda π: π.x * 7))
```

```{code-cell} ipython3
DEFINE('y', 42)  # <~~~ in ΓΠ
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)), 
    Λ(lambda π: SET_BANG('x', π.x * 7, π)),
    Λ(lambda π: π.x * π.y))
```

Check for unbound variables.

```{code-cell} ipython3
try:
    BLOCK(
        Λ(lambda π: SET_BANG('x', 6, π)), 
        Λ(lambda π: SET_BANG('x', π.x * 7, π)),
        Λ(lambda π: print({'expect 0': π.x * π.y})),
        Λ(lambda π: π.z))  # <~~~ no binding
except NameError as e:
    print(e.args)
```

Test BLOCK in a non-global environment $\pi$:

```{code-cell} ipython3
Λ(lambda π0:  # <~~~ Make a non-global π.
  print(
  BLOCK(
      Λ(lambda π: SET_BANG('x1', 7, π), π=π0),
      Λ(lambda π: SET_BANG('y1', 6, π), π=π0),
      Λ(lambda π: π.x1 * π.y1, π=π0),
      π=π0
  )),
  ['x1', 'y1'])(0, 0)
```

BLOCK automatically propagates the environments, so the $\pi_0$ arguments to the individual $\lambda$s are overkill:

```{code-cell} ipython3
Λ(lambda π0:  # <~~~ Make a non-global π.
  print(
  BLOCK(
      Λ(lambda π: SET_BANG('x1', 7, π)),
      Λ(lambda π: SET_BANG('y1', 6, π)),
      Λ(lambda π: π.x1 * π.y1),
      π=π0
  )),
  ['x1', 'y1'])(0, 0)
```

Names `x1` and `y1` are not bound in the global environment, even though they were in the non-global environment. The following example proves no "binding leakage."

```{code-cell} ipython3
try:
    ΓΠ.x1
except NameError as e:
    print(e.args)
    
try:
    ΓΠ.y1
except NameError as e:
    print(e.args)
```

Check that intermediate lambdas that don't return `None` are NOT a problem:

```{code-cell} ipython3
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)), 
    Λ(lambda π: SET_BANG('x', π.x * 7, π)),
    Λ(lambda π: π.x * π.y),
    Λ(lambda π: π.x * π.y))
```

Check nested `BLOCK`s. Don't forget to wrap the nested `BLOCK` in a [thunk](#thunk)!

```{code-cell} ipython3
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, ECHO('π', π))), 
    Λ(lambda π:  # <~~~ Don't forget to wrap it!
      BLOCK(Λ(lambda π: SET_BANG('x', π.x * 7, π)),
            Λ(lambda π: π.x * π.y))))
```

# Clear the Global Environment

+++

Get rid of `x` and `y` and other gaseous bindings lest they cause trouble below. We shouldn't just make a new global environment because some system procedures, like [`DEFINE`](#define), are closed over the old one and we'd just have to specify `ΓΠ` everywhere explicitly rather than using the convenient defaults.

```{code-cell} ipython3
del ΓΠ.ϕ.γόὂ
del ΓΠ.ϕ.saxpy
del ΓΠ.ϕ.sum_of_squares
del ΓΠ.ϕ.factorial
del ΓΠ.ϕ.fact_iter
del ΓΠ.ϕ.fact_recursive
del ΓΠ.ϕ.fib_slow
del ΓΠ.ϕ.fib_iter
del ΓΠ.ϕ.fib_fast
del ΓΠ.ϕ.fib_fast_uncurried
del ΓΠ.ϕ.fib_tc_memo
del ΓΠ.ϕ.x
del ΓΠ.ϕ.y
ΓΠ
```

`PN` does not appear because we have not yet called `LOOPN`.

+++

Define the Scheme-like synonym `BEGIN` for `BLOCK`:

```{code-cell} ipython3
BEGIN = BLOCK
```

# LET*, LET, LETREC<a id="the-lets"></a>

+++

`LET_STAR` is sequential binding of locals, syntactically like assignment, but purely with $\lambda$ expressions. Later bindings may depend on earlier ones. `LET` is parallel binding, where bindings are independent and unordered, but can't depend on one another. `LETREC` is mutually recursive `LET`, where any bindings may depend on any and all other bindings.

+++

We face a design choice: should the binding pairs be a list of tuples, as in Scheme, or a flat list of pairs, as in Clojure? The former introduces a level of collections, the latter simplifies implementation via slicinng, but introduces more checks for types and for element counts. We choose the Scheme-like option because the Schemulator is supposed to be like Scheme on the surface unless there is a profound reason not to be.

+++

Remember that $\Xi$ is a [Greek](#greek) shortcut for [`Application`](#application). Body of any of the _Lets_ must be an `Application` so that the environment $\pi$ can be propagated at the point where it is known.

+++

> __WARNING__: ***Results of `LET*`, `LET`, and `LETREC` are undefined if the body is not an [`Application`](#application) or $\Xi$.***

+++

## LET_STAR<a id="let-star"></a>

```{code-cell} ipython3
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
```

### Examples:

+++

Test depth 0, no bindings. The final expression can be a procedure:

```{code-cell} ipython3
LET_STAR([], 
         Λ(lambda π: 43 * π.x, ['x']))
```

Which can be called:

```{code-cell} ipython3
LET_STAR([], 
         Λ(lambda π: 43 * π.x, ['x']))(42)
```

Applications are evaluated in place:

```{code-cell} ipython3
LET_STAR([], 
         Ξ(Λ(lambda π: 43 * 42)))
```

The `LET_STAR` chains bindings in environments and evaluates applications in the chain. Procedures in applications enjoy free variables from any environments in the chain.

```{code-cell} ipython3
LET_STAR([('z', 42)],
        Ξ(Λ(lambda π: ECHO('π', π).z * π.z)))
```

Later bindings can acces and can shadow earlier ones:

```{code-cell} ipython3
LET_STAR([('z', 42),
          ('z', Ξ(Λ(lambda π: π.z + 1)))],
        Ξ(Λ(lambda π: ECHO('π', π).z * π.z)))
```

```{code-cell} ipython3
LET_STAR([('z', 42)],
        Λ(lambda π: π.z * π.z))
```

```{code-cell} ipython3
LET_STAR([('z', 42)],
        Λ(lambda π: π.z * π.z))()
```

Test depth 1 (don't forget the `Var` in $\Xi$:

```{code-cell} ipython3
LET_STAR([('z', 42)], 
         Ξ(ΓΠ.square, [Var('z')]))
```

### Free Variables<a id="free-variables"></a>

+++

> __DEFINITION__: A ___free variable___ in the body of a lambda is a variable _NOT_ in the parameter list.

+++

`z`, `y`, and `y` are free in the $\lambda$s below:

+++

Test depth 2 with free variables:

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', 43)], 
         Ξ(Λ(lambda π: print(π.z * π.y))))
```

Inspect them with an `ECHO` around the environment variable:

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', 43)], 
         Ξ(Λ(lambda π: print(ECHO('π', π).z * π.y))))
```

Test depth 3 with free varialbes:

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', Ξ(Λ(lambda π: π.z + 1))), 
          ('w', Ξ(Λ(lambda π: π.z * π.y)))],
         body=Ξ(Λ(lambda π: print(ECHO('π', π).w))))
```

### Bound Variables<a id="bound-variables"></a>

+++

> __DEFINITION__: A ___bound variable___ in the body of a $\lambda$ is a variable that appears in the parameter list.

+++

Remember that [this definition of bound variables is confusing](#confusing).

+++

### Closed Term<a id="closed-term"></a>

+++

> __DEFINITION__: A ___closed procedure___ or ___closed term___ is a procedure with no free variables (see [this web page](https://web.mat.bham.ac.uk/R.W.Kaye/logic/freevar.html)).

+++

Test depth 3 with bound variables. To access earlier bindings, write an [`Application` or $\Xi$](#application) that, when evaluated, [accesses earlier bindings as `Var`s](#var). Notice the bound vars shadow free vars with the same names.

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', Ξ(Λ(lambda π: π.z + 1, ['z']),  # <~~~ z shadows ...
                  [Var('z')])),  # <~~~ ... this Var.
          # Shadowing occurs here, too.
          ('w', Ξ(Λ(lambda π: ECHO('π', π).z * π.y, ['z', 'y']), 
                  [Var('z'), Var('y')])
          )], 
         body=Ξ(Λ(lambda π0: print(ECHO('π0', π0).w))))
```

The names of the bound variables do not matter. In this case, we avoid shadowing, so the names `z`, `y`, and `w` are available in the body of `w`'s procedure:

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', Ξ(Λ(lambda π: π.zz + 1, ['zz']),
                  [Var('z')])), 
          # Use parameters:
          ('w', Ξ(Λ(lambda π: ECHO('π', π).zzz * π.yy, ['zzz', 'yy']), 
                  [Var('z'), Var('y')])
          )], 
         body=Ξ(Λ(lambda π: π.w)))
```

```{code-cell} ipython3
LET_STAR([('z', 42), 
          ('y', Ξ(Λ(lambda π: π.zz + 1, ['zz']),
                  [Var('z')])), 
          # Ignore parameters; use free variables:
          ('w', Ξ(Λ(lambda π: ECHO('π', π).z * π.y, ['zzz', 'yy']), 
                  [Var('z'), Var('y')])
          )], 
         body=Ξ(Λ(lambda π: π.w)))
```

### Closures<a id="closures"></a>

+++

> __DEFINITION__: A ___closure___ is a procedure along with its environment chain. All variables, free and bound, may be ___resolved___ in the environment chain.

+++

The body of this next example evaluates `g` without returning a closure:

```{code-cell} ipython3
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
        body=Ξ(Λ(lambda π: ECHO('π', π).g(42))))
```

But, we may return a closure, meaning that its environment chain is still alive. We may invoke it outside the `LET_STAR`.The application $\Xi$ pulls the closure out of the environment created by `LET_STAR`. 

```{code-cell} ipython3
α = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
             body=Ξ(Λ(lambda π: ECHO('π', π).g)))
α(42)
```

Without he application `Ξ`, the value is an unevaluated procedure returning a closure:

```{code-cell} ipython3
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
        body=Λ(lambda π: π.g))
```

We can evaluate the returned closure with an extra, explicit call:

```{code-cell} ipython3
foo = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
               body=Λ(lambda π: π.g))
foo()(42)
```

```{code-cell} ipython3
try:
    LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
             body=Λ(lambda π: π.g))()
except NameError as e:
    print(e.args)
```

```{code-cell} ipython3
try:
    LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
             body=Λ(lambda π: π.g(42)))()
except NameError as e:
    print(e.args)
```

Ensure no leakage:

```{code-cell} ipython3
try:
    ΓΠ.g
except Exception as e:
    print(e.args)
```

Procedures can be bound variables, too, with arbitrary names:

```{code-cell} ipython3
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
         body=Ξ(Λ(lambda π: π.gg(42), ['gg']), [Var('g')]))
```

Unevaluated, it's still a closure:

```{code-cell} ipython3
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
         body=Ξ(Λ(lambda π: π.gg, ['gg']), [Var('g')]))(
42)
```

## LET<a id="let"></a>

+++

`LET` is parallel "assignment." All variables must be bound in the enclosing environment and may not depend on one another. This implementation is not curried. `body` is usually an [`Application`](#application).

```{code-cell} ipython3
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
```

### Examples:

+++

Test depth 0:

```{code-cell} ipython3
LET([], 
    Ξ(Λ(lambda π: print(43 * 42))))
```

Test depth 1:

```{code-cell} ipython3
LET([('z', 42)], 
    Ξ(ΓΠ.square, [Var('z')]))
```

Test depth 2:

```{code-cell} ipython3
LET([('z', 42), 
     ('y', 43)], 
    Ξ(Λ(lambda π: print(π.z * π.y))))
```

Reversed:

```{code-cell} ipython3
LET([('y', 42), 
     ('z', 43)], 
    Ξ(Λ(lambda π: print(π.z * π.y))))
```

With applications as values, the inner `y` is evaluated in the local environment, not leaking down from the global environment $\Gamma\Pi$, where `y` is 0:

```{code-cell} ipython3
DEFINE('y', 0)
LET([('y', 42), 
     ('z', Ξ(Λ(lambda π: π.y + 1)))],  # Outer y = 0, not inner y = 42
    Ξ(Λ(lambda π: print(π.z * π.y))))  # Inner y = 42 * inner z = 1
```

Order does not matter:

```{code-cell} ipython3
LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)], 
    Ξ(Λ(lambda π: print(π.z * π.y)))) # Inner y = 42 * inner z = 1
```

Print the environment to check that all symbols are bound in it:

```{code-cell} ipython3
LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)], 
    Ξ(Λ(lambda π: print(ECHO('π', π).z * π.y)))) # Inner y = 42 * inner z = 1
```

Prove global `y` is unchanged and that `z` is bound only in local environment, not global.

```{code-cell} ipython3
print({'expect y = 0':  ΓΠ.y})
try:
    print(ΓΠ.z)
except NameError as e:
    print(e.args)
```

Test nested `LET`. Don't forget to chain the environments! The default is $\Gamma\Pi$.

```{code-cell} ipython3
LET([('z0', 42)],
    Ξ(Λ(lambda π0:
        LET([('y0', Ξ(Λ(lambda π1: π1.z0 + 1)))],
            Ξ(Λ(lambda π2: π2.z0 * π2.y0)),
            π=π0))))  # <~~~ Don't forget to chain!
```

The free variable `x` in the $\lambda$ below is looked up in the local environment established by `LET`.

+++

First, delete `x` again, just in case, so we can check that it does not get bound accidentally.

```{code-cell} ipython3
try: 
    del ΓΠ.ϕ.x
except:
    pass
```

```{code-cell} ipython3
LET([('x', 42)],
   Ξ(Λ(lambda π: π.x * π.x)))
```

The variable `x` did not leak out of the local environment:

```{code-cell} ipython3
try:
    ΓΠ.x
except Exception as e:
    print(e.args)
```

We can get the same result as above when the internal $\lambda$ does not have free variables. When evaluating the application $\Xi$, grab the local value of `x` as an actual argument and substitute it for the bound variable `y` in the body of the $\lambda$:

```{code-cell} ipython3
LET([('x', 42)],
   Ξ(Λ(lambda π: π.y * π.y, ['y']), [Var('x')]))
```

### Test `EVAL` on Collections<a id="test-collections"></a>

+++

[Notice that `EVAL` recurses into Dicts, Tuples, Lists, and numpy arrays](#eval). `LET_STAR` and `LET` gives us good tools for testing that.

+++

Lists, with free variables:

```{code-cell} ipython3
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.x * π.y)))
```

With bound variables:

```{code-cell} ipython3
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.xx * π.yy, ['xx', 'yy']), [Var('x'), Var('y')]))
```

Tuples, with free variables:

```{code-cell} ipython3
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: (π.forty_two, π.forty_two + 1)))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.x * π.y)))
```

Dictionaries, with free variables:

```{code-cell} ipython3
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: {'π.forty_two': π.forty_two, 
                         'forty-three': π.forty_two + 1}))),
    ('y', 3)],
   Ξ(Λ(lambda π: [π.x['π.forty_two'], 
                  π.x['forty-three']] * π.y)))
```

Numpy arrays, with free variables:

```{code-cell} ipython3
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: numpy.array(
        [π.forty_two,
         π.forty_two + 1])))),
    ('y', 3)],
    Ξ(Λ(lambda π: π.x * π.y)))
```

## LETREC<a id="letrec"></a>

+++

`LETREC` binds codependent values in a new environment _before_ evaluating them. `LET` evaluates values before binding them. 

+++

`LETREC` patches the environments of any contained procedures to ensure they have access to new bindings. [Lengthy code in `EVAL` does the monkey-patching for us](#eval) later. `LETREC` differs from `LET` only in the absence of `EVAL`s on the value side. Those `EVAL`s are done later.

```{code-cell} ipython3
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
    _ = [setattr(E1.ϕ, p[0], p[1]) for p in pairs]  # <~~~ DON'T EVAL!
    ρ = EVAL(body, E1)
    return ρ
```

### Examples

+++

Factorial, familiar by now, accesses itself through the patched environment; `fact` is a free variable in the body of `fact`:

```{code-cell} ipython3
LETREC([('fact', 
         Λ(lambda π: 
           (π.a
            if π.m <= 0 
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Ξ(Λ(lambda π: π.fact(6, 1))))
```

Here is a version that fails if the patching is not done correctly:

```{code-cell} ipython3
Λ(lambda π: 
  LETREC([('fact', 
           Λ(lambda π: 
             (π.a
              if π.m <= π.m0  # <~~~ Watch out!
              else π.fact(π.m - 1, π.m * π.a)),
             ['m', 'a']))],
         Ξ(Λ(lambda π: π.fact(6, 1))),
         π),  # <~~~ access 'm0'
 ['m0'])(0)
```

The final application $\Xi$ is necessary to actually evaluate the final $\Lambda$, lest it be simply returned unevaluated:

```{code-cell} ipython3
LETREC([('fact', 
         Λ(lambda π: 
           (π.a
            if π.m <= 0 
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Λ(lambda π: π.fact(6, 1)))
```

But we can evaluate it:

```{code-cell} ipython3
LETREC([('fact', 
         Λ(lambda π: 
           (π.a
            if π.m <= 0 
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Λ(lambda π: π.fact(6, 1)))()  # <~~~ extra evaluation!
```

One can unroll the final application into formal parameters and actual arguments:

```{code-cell} ipython3
LETREC([('fact', 
         Λ(lambda π: 
           (π.a
            if π.m <= 0 
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Ξ(Λ(lambda π: π.fact(π.n, π.b), 
           ['n', 'b']),  # <~~~ formal parameters
         [6, 1]))  # <~~~ actual arguments
```

Mutually codependent procedures are OK:

```{code-cell} ipython3
LETREC([('z0', Λ(lambda π: 1 + π.y0(), ['y0'])),
        ('y0', Λ(lambda π: 42))],
      Ξ(Λ(lambda π: π.y0() * π.z0(π.y0))))
```

Check that `y0` does not leak into the global environment:

```{code-cell} ipython3
try:
    print(ΓΠ.y0)
except NameError as e:
    print(e.args)
```

The following shows that `z0` also does not leak from `LETREC`:

```{code-cell} ipython3
try:
    print(ΓΠ.z0)
except NameError as e:
    print(e.args)
```

The following example is [borrowed from the Racket documentation](https://docs.racket-lang.org/reference/let.html).

```{code-cell} ipython3
LETREC([('is_even',
         Λ(lambda π: True if π.n == 0 else (not π.is_odd(π.n)), 
           ['n'])),
        ('is_odd', 
         Λ(lambda π: π.n != 0 and π.is_even(abs(π.n) - 1),
           ['n']))],
      Ξ(Λ(lambda π: (
          π.is_even( 42),
          π.is_even(-43),
          π.is_odd (-42),
          π.is_odd ( 43),
      ))))
```

# LABELS<a id="labels"></a>

+++

`LABELS` is a special case of `LETREC` where all the values are mutually codependent procedures.

```{code-cell} ipython3
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
```

Our old friend, factorial:

```{code-cell} ipython3
LABELS([('fact_iter_nom',
        Λ(lambda π: 
        (π.a 
         if π.m <= 0 
         else π.fact_iter_nom(π.m - 1, π.a * π.m)),
       ['m', 'a']))],
      Ξ(Λ(lambda π: π.fact_iter_nom(6, 1))))
```

Test monkey patching again:

```{code-cell} ipython3
Λ(lambda πo: 
  LABELS([('fact_iter_nom',
           Λ(lambda π: 
             (π.a 
              if π.m <= π.m0  # <~~~ Watch out!
              else π.fact_iter_nom(π.m - 1, π.a * π.m)),
             ['m', 'a']))],
         Ξ(Λ(lambda π: π.fact_iter_nom(6, 1))),
         πo),
  ['m0'])(1)  # <~~~ Works with 1, also.
```

# DO

+++

Here is the specification of `DO` from Steele's paper, without further explanation here:

+++

```
(DO ((<var1> <init1> <step1>)
     (<var2> <init2> <step2>)
     ...
     (<varN> <initN> <stepN>))
    (<pred> <value>) 
    <optional body>)
```

+++

First, a non-tail-recursive version:

```{code-cell} ipython3
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
```

Our old friend, factorial, as non-tail-recursive `DO`:

```{code-cell} ipython3
Λ(lambda πo:
  DO_NTC([('m', πo.m, Λ(lambda π: π.m - 1)),
          ('a', 1, Λ(lambda π: π.a * π.m))],
         pred=Λ(lambda π: π.m <= 1),
         value=Λ(lambda π: π.a),
         body=Λ(lambda π: None),
         π=πo),
  ['m'])(6)
```

This blows recursion, naturally:

```{code-cell} ipython3
try:
    Λ(lambda πo:
      DO_NTC([('m', πo.m, Λ(lambda π: π.m - 1)),
              ('a', 1, Λ(lambda π: π.a * π.m))],
             pred=Λ(lambda π: π.m <= 1),
             value=Λ(lambda π: π.a),
             body=Λ(lambda π: None),
             π=πo),
      ['m'])(400)
except RecursionError as e:
    print(e.args)
```

A tail-recursive version, with the natural name `DO`. This uses `LOOPN`, defined earlier. Because tail-recursion must be the base case in Schemulator, we don't bother to test $\Upsilon{}N$, here:

```{code-cell} ipython3
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
```

```{code-cell} ipython3
Λ(lambda πo:
  DO([('m', πo.m, Λ(lambda π: π.m - 1)),
      ('a', 1, Λ(lambda π: π.a * π.m))],
     pred=Λ(lambda π: π.m <= 1),
     value=Λ(lambda π: π.a),
     body=Λ(lambda π: None),
     π=πo),
  ['m'])(6)
```

```{code-cell} ipython3
Λ(lambda πo:
  DO([('m', πo.m, Λ(lambda π: π.m - 1)),
      ('a', 1, Λ(lambda π: π.a * π.m))],
     pred=Λ(lambda π: π.m <= 1),
     value=Λ(lambda π: π.a),
     body=Λ(lambda π: None),
     π=πo),
  ['m'])(400)
```

# COND

+++

TODO

+++

# Junkyard

+++

Ignore everything below. It's saved in case we need it someday.
