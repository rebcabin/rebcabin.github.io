---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.14.4
  kernelspec:
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

# Algebraic Imperative Semantics


or, Lambda, The Pynultimate Imperative


**Version 9**


**Brian Beckman**


**16 Dec 2023**


**[Creative Commons Attribution 4.0 International Public License](https://creativecommons.org/licenses/by/4.0/legalcode)**


# Introduction


In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative constructs with just _lambda:_


> We demonstrate how to model the following common [imperative] programming constructs in ... an applicative-order language similar to LISP: Simple Recursion, Iteration, Compound Statements and Expressions, GO TO and Assignment, Continuation-Passing, Escape Expressions, Fluid Variables, Call by Name, Call by Need, and Call by Reference. The models require only (possibly self-referent) lambda application, conditionals, and (rarely) assignment.


It's useful to recap this paper in Python, which has most of the listed imperative constructs. Imagine compiling Python into an intermediate language in which the semantics, even those with side-effects, are laid bare as trees of $\lambda$ expressions. In such a representation, optimizations are
1. easy to write as tree-to-tree transforms, in a closed algebra of expressions
1. easy to extend via just function composition (even Kleisli-monadic)
2. independent of surface syntax, thus easy to share with or decompile into other imperative languages like Fortran, C, Java
3. independent of back ends, thus easy to run interactively; or to translate into LLVM, x86, ARM64, C, for execution; or to transpile into other surface languages


The use-cases above are similar to those for a SQL algebraizer. Many SQL implementations
1. translate the surface language into bare-bones expressions in a closed relational algebra, free of original syntax
2. run the algebraic expressions through symbolic optimizers, which often rearrange the expressions completely
2. incrementally extend the system by composing new optimizations
3. translate optimized expressions into commands for local and distributed servers


TODO: An older algebraic formalization, [Denotational Semantics, was defined for R5RS](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html). It focused on the non-imperative aspects of scheme. The reconstruction in this paper has not yet been compared to that one.


TODO: ? Formalize objects of Object-Oriente Programming ?


We follow Steele's and Sussman's paper more-or-less directly, with reference to [SICP](https://sarabander.github.io/sicp/), ["TSPL"](https://www.scheme.com/tspl4/), and [the Gambit Manual](https://gambitscheme.org/latest/manual/#Top).


## _Schemulation:_ Python Semantics in Python<a id="semantics"></a>


Ideally, we'd compile Python into Scheme or Clojure or Common Lisp, then write semantical transformations, translations, interpreters, debuggers, etc. in Scheme or Clojure or Common Lisp. We'd even prove out our work with Coq, as in [Software Foundations](https://softwarefoundations.cis.upenn.edu/). All of these systems have great support for formalizing semantics. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in a Scheme-like applicative-order $\lambda$-calculus embedded in Python.


Some people may find this weird. Why not just implement Python in Scheme (or Clojure or Common Lisp) instead of emulating Scheme in Python? It's an engineering judgment call. Most authors of compilers and interpreters spend undue time on syntax before even getting to semantics. Compiler textbooks tell you to do this! Semantics becomes a hidden "implementation detail," but then developers forgive themselves for making an ungodly mess of it. We prefer the other way around. Get the semantics clean, composable, extendable, maintainable, and correct, _first_, then spend time on syntax, if you even care any more. Or just let someone else do it. From the compiler-development point of view, syntax is a distraction. It's been solved for decades, but it makes young developers and professors feel powerful. The decision to spend time on syntax before semantics is not engineering, it's self-indulgence.


TODO: Revisit in the light of [Calysto Scheme](https://github.com/Calysto/calysto_scheme), a Scheme implemented in Python.


## How to Use this Notebook


The goal of this notebook is to transmit ideas from my mind to others. Most examples return easy numerical results like `42 * 43 == 1806`. Do the arithmetic and look at results. That's why I don't use Python `assert` much, in this notebook.


There is a companion Python project that `asserts` the results in pytest, proving the results in the computer. Unlike this notebook, that project is written in non-narrative style. The tests can be run in any order, largely independent of one another.


Run the cells sequentially, in order. Some cells modify the unique global environment. Later cells may depend on earlier cells.


# PART I: BASICS<a id="basics"></a>


[Chez Scheme](https://github.com/cisco/ChezScheme/blob/main/BUILDING) does not build on my Mac M1 Max. I run Scheme examples in [Gambit Scheme](https://gambitscheme.org/latest/manual/#Top). My Jupyter installation does not support multiple languages in the kernel. All code cells run in Python 3.9.6. Scheme cells must be copied and pasted into an interpreter. There is a companion file called "AlgImperSem008.scm," which can be loaded and run in Scheme like this:


(load "AlgImperSem008.scm")


# `ECHO` for Debugging


`ECHO` prints and then returns its argument. Semantically, it's the identity function.


Though the print statement in `ECHO` is technically a side-effect, it's not semantically pertinent. It's a side-effect at the interpreter level,  for user convenience.

```python
from pprint import pprint, pformat
from typing import Any
def ECHO(key: str, x: Any) -> Any:
    """In any Lisp, this would be a macro!"""
    print()
    pprint({key: x})
    return x
```

Scheme equivalent:

<!-- #raw -->
(define (echo str expr)
  (pp str)
  (newline)
  expr)
<!-- #endraw -->

<!-- #region tags=[] -->
# Environment and Frame<a id="environment"></a>
<!-- #endregion -->

The material in this section will remind you of "Operations on Environments" in [SICP 4.1.3](https://sarabander.github.io/sicp/html/4_002e1.xhtml#g_t4_002e1_002e3).


We model Scheme's environments and frames explicitly. We tried multiple Pythonic short-cut alternatives and found that none compose well.


[SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) has apparent contradictions in the definition of environment and frame. It says that "an environment is a sequence of frames," but the rest of the text and figures clearly imply that an environment has just one frame.


The best resolution appears to be:


> __DEFINITIONS__: An ___environment___ is a frame $\phi$ and a pointer $\pi$ to an enclosing environment. A ___frame___ is a mathematical function from variable names to values; no variable name may appear more than once in a frame.


We note in passing that this works only for a single thread. [Clojure, for instance, solves that problem with _Vars_](https://clojure.org/reference/vars).


## Greek and ALL CAPS<a id="greek"></a>


$\pi$ for an enclosing environment is a nice pun: it evokes $\pi\eta\rho\iota$, a Greek prefix meaning "surrounding," as in "perimeter" (_sic_, not "parameter").


This notebook uses Greek letters in the names of system attributes and variables. These letters won't collide with user symbols if users follow the rule below. With this rule, we don't need `gensym`, a thorn in every programmer's side:


> __RULE__: Avoid Greek in user-level Python code to avoid clobbering system-supplied names.


Greek is problematic in my installation of Gambit, so I avoid Greek in Scheme code.


Names in ALL CAPS denote Schemulator procedures visible at user level.


## Bindings<a id="bindings"></a>


> __DEFINITION__: A ___binding___ is an association from a variable name to a value. A binding is an entry in a frame.


We might model a binding as a pair, a row in a table, an element of a relation (subset of a Cartesian product), an _item_ in a Python dictionary, or as an attribute of a Python object. We prefer attributes of Python objects because they afford _dot_ notation, that is, `obj.foo`. Dot notation is shorter than dictionary syntax `dict['foo']`. Thanks to [divs1210](https://gist.github.com/divs1210?page=3) for this idea.


If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a sequence of frames via the chain of pointers to enclosing environments.


> __INVARIANT__: The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`.


> __OBSERVATION__: A frame $\phi$ belongs to a sequence of environments implied by the unidirectional chain of enclosing environments ending at the unique global environment.


> __DEFINITIONS__: The ___value of a variable in an environment___ is the value in the first binding in any frame of the sequence ending at _global_. Bindings lower in the chain may ___shadow___ bindings higher in the chain. If no frame in a chain has a binding for a variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another.


> __LEMMA__: A chain of environments implements a mathematical partial function from variable names to values. Shadowing ensures that any name appearing more than once in a chain has only one value when looked up.


Informally, an accessible variable in a chain is a _found_ variable. For all intents and purposes, a _found_ variable is a _bound_ variable and vice versa, though [beware of the confusing usage of the term "bound variable."](#confusing)


In the Environment class below, `__getattr__` is overridden to avoid a separate method for recursive lookup. We can't also override `__setattr__` to call `setattr(self.ϕ ...)` because `self.ϕ` diverges on `getattr(self.ϕ ...) = getattr(getattr(getattr...))`.


The `Environment` class is lengthy only due to code for _splicing_, necessary to bind [___free variables___](#free-variables) in procedure bodies. Jupyter notebooks do not furnish a convenient way to break up the code for classes, so we put essential information in comments; read the comments as if they were text in the notebook.

```python
from dataclasses import dataclass, field
from types import FunctionType
from typing import Any

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
        - The end of a trunk is the last, non-global env.
        - The root of a chain is the final global.

        lemmas:
        - A chain cannot be None.
        - A chain could be simply a ref to global. That's a
          chain with no tip or trunk, only a root.
        - The tip of a chain is None if the chain is just the
          final global.
        - The end of a trunk can be None.
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
            # Walk up the chain, excluding global.
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
                # Walk up the chain, excluding global.
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


The unique global environment is $\Gamma\Pi$, defined once for each session. The frame $\phi$, a Python object that ___carries___ attributes, is an empty function, namely `lambda: None`.

```python
ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
```

## Example:


Show setting and getting the binding for a made-up variable, `γόὂ` as an attribute on $\phi$

```python
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ
```

<!-- #region tags=[] -->
# Procedure(code{body, params}, $\pi$)<a id="procedure"></a>
<!-- #endregion -->

From SICP again:


> __DEFINITIONS__: A ___procedure___ is a pair of _code_ and environment.


The environment specifies the ___closure chain___ for the procedure. The closure chain is the chain of environments that contain [_free-variable bindings_](#free-variables), that is, bindings for non-parameters.


> __DEFINITION__: A ___closure___ is a procedure along with its closure chain of environments. All variables, free and bound, may be ___resolved___, that is, looked up, in the environment chain.


In a typical scenario, a procedure will call other procedures. The other procedures may be passed in as arguments. More typically, however, the names of these other procedures are free variables. For example, in the following procedure (in ordinary Python)

```python
def complex_from_polar(r, θ):
    from math import sin, cos
    result = complex(r * cos(θ), r * sin(θ))
    return result
from math import pi
complex_from_polar(1.0, pi/4)
```

`sin` and `cos` are [free variables](#free-variables), and `r` and `theta` are not free. In the following, more tedious example, `math.sin` and `math.cos` are passed in as arguments bound to the parameters named `sin` and `cos`:

```python
def complex_from_polar(r, θ, sin, cos):
    result = complex(r * cos(θ), r * sin(θ))
    return result
import math
complex_from_polar(1.0, math.pi/4, math.sin, math.cos)
```

Procedures are values, like numbers. Notionally, they are either

1. lookup tables from input values to output values, if they are pure functions, or

2. blocks of code that compute outputs from imputs and optionally perform side effects.


> __DEFINITIONS__: ___Code___ is a dictionary of two items: a list of parameter names and a `body`. _Body_ is a $\lambda$ expression taking a single parameter, $\pi$. $\pi$ is bound to the closure chain or ___define-time environment___. The define-time environment is the old [`Environment`](#environment) in which [`APPLY`] evaluates [actual arguments](#actual-arguments) at call time before binding them to [formal parameters](#formal-parameters) in a fresh [`Environment`](#environment).


> __DEFINITIONS__: ___Call___, ___invocation___, and ___application___ are synonyms for expressions in which a procedure's [formal parameters](#formal-parameters) are bound in a fresh environment to [actual arguments](#actual-arguments): values of `Any` type. Invocations may be evaluated _in-situ_ or may be captured in variables of type [`Application`](#application) for later evaluation.


This definition sidesteps questions of _order of evaluation_. These questions include shallow ones, such as right-to-left versus left-to-right, and deeper ones, such as [_applicative order_ versus _normal order_](https://sarabander.github.io/sicp/html/4_002e2.xhtml#g_t4_002e2_002e1).


> __DEFINITION__: ___Arguments___ or ___[actual arguments](#actual-arguments)___ are values of `Any` type that appear in 1-to-1 correspondence to [formal parameters](#formal-parameters) in a call expression, a.k.a, invocation expression or application expression.


## Example of _Code_


The following is an example of the (un-named) type of a **Code**, that is, a dictionary with a _body_ and a list of _formal parameters_.

```python
{"body": lambda π: π.x + (2 * π.y),
 "parameters": ['x', 'y', 'z']};
```

## Confusing Terminology<a id="confusing"></a>


When speaking of the body of a procedure, variables in the parameter list, i.e., [formal parameters](#formal-parameters), are called [___bound variables___](#bound-variables) even though they do not have bound values. Confer `x` and `y` above, noting that `z` does not appear in the parameter list of the body -- it is an [___ignored parameter___](#ignored-parameter).


> __WARNING__: This terminology is confusing, but common.


> __EMPHASIS__: [Formal parameters](#formal-parameters) are not bound to values when the procedure is _defined_, only when the procedure is _called_ with [actual arguments](#actual-arguments). [Formal parameters](#formal-parameters) are presumably called _bound variables_ because it's shorter than calling them "potentially bound variables" or "eventually bound variables."


## Parameters, Arguments<a id="formal-parameters"></a><a id="actual-arguments"></a>


In ordinary Python, a call, invocation, or application of an anonymous function looks like this:

```python
(lambda x, y, z:  # <~~~ formal parameters
 x + 2*y)(
    40, 1, None)  # <~~~ actual arguments
```

The formal parameters are `x`, `y`, and `z`. The actual arguments are `40`, `1`, and `None`.


> __DEFINITION__: ___Formal___ means "machine-checkable."


Formal parameters are called "formal" because one can machine-check properties of the parameter list, such as absence of duplicated symbols.


Not all formal parameters need be mentioned in the body. Vice versa, the body may mention variables that are not in the parameter list. Such variables are [___free variables___](#free-variables).


In Schemulator, we write the code part of the example above as follows:

```python
lambda π: π.x + (2 * π.y);
```

Notice that the unused parameter `z` is nowhere to be seen. How do we know it's there? We might wait until the procedure is called with the wrong number of actual arguments, producing a run-time error. That's not great!


To make all parameters, even [_ignored parameters_](#ignored-parameter) explicit, the body code is paired with a list of formal parameters.

```python
{"body": lambda π: π.x + (2 * π.y),
 "parameters": ['x', 'y', 'z']};
```

When speaking of the (formal) parameters of a procedure or the (actual) arguments in an invocation of a procedure, we do not mean $\pi$, the closure chain. Rather, we mean the variables bound in the fresh environment that [`APPLY`](#apply) chains to the front of $\pi$.


### Ambiguous Language


Sloppily, one says "a procedure of $n$ arguments" and really means "a procedure with $n$ (formal) parameters." More carefully, we may say "a procedure _call_ with $n$ (actual) arguments," or "a procedure _invocation_ with $n$ arguments," or "a procedure _application_ with $n$ arguments."


Notice we gradually elide "formal parameters" and "actual arguments" to the shorter "parameters" and "arguments," without loss of precision.


### Positional Arguments Only


For now, unlike ordinary Python, Schemulator has only positional parameters, with the non-ignored ones 1-to-1 with arguments. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the call has too many or too few arguments. Since Schemulator should be semantically compatible with Scheme, it's OK to have positional parameters and arguments only. Likewise, Schemulator does not yet have Python's (admittedly wonderful) keyword arguments.


## Anonymous versus Named


Procedures need not have a name. In ordinary Python, contrast the named procedure `foo`

```python
def foo(x):
    return x * x
foo(42)
```

against the anonymous procedure with an identical body (except for the lack of `return`):

```python
(lambda x: x * x)(42)
```

By default, if the user gives a name to a Schemulator procedure, that name is bound in the unique global environment, $\Gamma\Pi$. Schemulator allows nested definitions in non-global environments, as with `def` inside `def` in ordinary Python.


## Call Notation via `__call__`


The `Procedure` class below includes a `__call__` override. To test it, we need [`APPLY`](#apply). We prototype [`APPLY`](#apply) here, just printing actual arguments. [Later, in section `APPLY`](#apply), we implement the real work of binding parameters (in a fresh environment) to arguments (evaluated in an existing environment). That implementation requires [a codependent procedure, `EVAL`](#eval), which, in-turn, needs `APPLY` (see [SICP Figure 4.1](https://sarabander.github.io/sicp/html/4_002e1.xhtml#g_t4_002e1_002e1)).


The definition of `Procedure` again illustrates codependent types. `Procedure`'s `__call__` syntax depends on `APPLY` and `APPLY` depends on `Procedure`. Python (at least, this version of Python) requires writing to-be-defined types in string quotes.

```python
from typing import Dict, List, Tuple, Any, Union
Parameters = List[str]  # type synonym; positional, ordered arguments only
def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
          args: Union[List["Expression"], None],
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


Following SICP 3.2.1, define `square` in the global environment and test `APPLY`.

```python
setattr(  # Bind a variable ...
    ΓΠ.ϕ,  # ... in the frame of the global environment ...
    "square",  # ... a variable named "square" ...
    Procedure(  # ... to this Schemulator procedure.
        {"body": lambda π: π.x * π.x,
         "parameters": ['x']}))  # Don't forget the parameter list!
```

Test it! Remember, `APPLY`, for now, just prints the arguments of the applied procedure!

```python
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

Test detection of duplicate parameters:

```python
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

## Function, Routine, Method<a id="function"></a>


> __DEFINITION__: A ___function___ is a mathematical object that associates input arguments to unique output values.


The best way to think of a function is as a simple lookup table, where a key may appear no more than once. This works even if the notional lookup table is uncountably infinite, as with a function from real numbers.


In Schemulator, a function is a special case of a [___procedure___](#procedure): a procedure without side effects. Every invocation of a function with the same arguments produces the same result.


> __DEFINITION__: ___Routine___ is a synonym for _Procedure_.


> __DEFINITION__: A ___method___ is a procedure whose first argument is a Python object, as with `self` in ordinary Python.


This definition of _method_ hides the entire topic of object-oriented programming, only tangentially relevant here. One only needs to know that much of Schemulator is implemented in terms of Python's classes, objects, and methods.


## Shortcut: $\Lambda$($\lambda$, params, $\pi$)


Note: The `code` attributes of procedures are an unordered dictionary of parameters and body. The $\Lambda$ shortcut puts them in an order. $\Lambda$ expressions would be more clear if the list of parameters preceded the body, but defaulting the list to `None` would not be easy and one would have to write empty brackets `[]` in every instance of $\Lambda$. Better this way.


The example procedure above has a name, "square", bound in the global environment. "Square" is not anonymous, but the [`Procedure`](#procedure) value bound to the name "square" is anonymous.


$\Lambda$ is syntactical help to shorten definitions of anonymous procedures. Its default parameter list is empty and its default environment is the unique global environment $\Gamma{}\Pi$. The shortest $\Lambda$ expression is `Λ(lambda π: None)`.


For parameters whose values are of "function" type, Python requires their type notation in strings.

```python
def Λ(
        body: "(π: Environment) -> Any",
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


Procedures need closure chains for looking up free variables and for evaluating actual arguments. The next few examples have no free variables.


Test the $\Lambda$ syntax with the current `APPLY`, which just prints.

```python
setattr(  # Give a name ...
    ΓΠ.ϕ,  # ... in the frame of the global environment ...
    "square",
    Λ(lambda π: π.x * π.x, ['x']))  # ... to this anonymous procedure
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

<!-- #region tags=[] -->
# Application(head, args, $\pi$)<a id="application"></a>
<!-- #endregion -->

> __DEFINITION__: An `Application` is an unevaluated data object with a [`Procedure`](#procedure) or ___symbol___ of type `str` equal to the name of the procedure, and a list of actual arguments. The name of the procedure may be a name in the global environment, or may be the name of a formal parameter. In either case, the value must be eventually of type `Procedure`


Do not confuse this [_Application_](#application), capital "A", with _application_, little "a", meaning a call or invocation of a procedure. An _Application_ with capital "A" is inert data representing a procedure call, invocation, or application. We test it later, after [`EVAL`](#eval) and [`APPLY`](#apply) are fully defined.


`Application` is needed in [LET_STAR](#let-star) and related constructs to delay evaluation until the environment with parameter bindings is established, where [EVAL](#eval) can look them up by name.


`Application` is a placeholder for a more general [QUOTE](#quote) or macro mechanism, which prevents evaluation in all cases (TODO). In real Scheme, `LET` and friends are syntactical forms that delay evaluation via Scheme macros, i.e., code rewriters. We prefer explicit delaying via `Application` objects here at the semantical level, so as not to obscure the algebraic structures we pursue. TODO: there may be observble differences between Scheme and this Schemulator if Scheme delays the evaluation of arguments, not just the application of the procedure.


An alternative is an embedded macro DSL (Domain-Specific Language) in Python, overkill here. If we go down that road, we might as well just implement Scheme altogether.


`Application` includes a `__call__` override for Pythonic calling syntax.

```python
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
    args: List["Expression"] = field(default_factory=list)  # args, not params!
    π: Environment = ΓΠ
    def __call__(self, π=None):
        return EVAL_APPLICATION(self, π or self.π)
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


Just as [Procedure](#procedure) has a [system-reserved Greek](#greek) shortcut, $\Lambda$, we make a Greek shortcut, $\Xi$, for `Application`.

```python
Ξ = Application
```

Test later.

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# Var(sym)<a id="var"></a>
<!-- #endregion -->

In [applications](#application), we sometimes interpret strings as symbolic references to variables in an environment. The type that manages that need is `Var`. We test `Var` [after defining `EVAL`](#eval).

```python
@dataclass
class Var:
    sym: str
```

<!-- #region tags=[] -->
# EVAL(expr, $\pi$)<a id="eval"></a>
<!-- #endregion -->

`EVAL` calls `APPLY`, but `APPLY` calls `EVAL`. We can't test `EVAL` until [`APPLY` is corrected, below](#apply).


First, we correct `EVAL_APPLICATION`, [originally stubbed above](#application). The first slot of an `Application` may contain:

1. a string, [treated as a `Var` in the define-time environment](#var), that must evaluate to a procedure

2. or an explicit procedure


To evaluate an [`Application`](#application), evaluate the procedure or string (implicitly a `Var`) in the first slot, then evaluate the arguments, then [`APPLY`](#apply) the procedure.

```python
def EVAL(
        expr: Any,
        π: Environment = ΓΠ,
        tag=None
) -> Any:
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
        # 1/4. Evaluate first slot in the closure environment  ...
        proc = expr.head
    else:
        raise ValueError(
            f'The head of {expr} must be a string or a Procedure, '
            f'not a {expr.head}')
    # 2/4. Evaluate the proc to splice the free vars ...
    proc = EVAL(proc, π)
    # 3/4. Evaluate all args ...
    eargs = [EVAL(arg, π) for arg in expr.args]
    # 4/4. Apply the procedure.
    ρ = APPLY(proc, eargs, π)  # 3.3. Apply the procedure.
    return ρ
```

`EVAL_PROCEDURE` splices environments for [free variables](#free-variables).

```python
def EVAL_PROCEDURE(
        λ: Procedure,
        π: Environment = ΓΠ
) -> Procedure:
    # λ.π is mutable; modify it by side-effect.
    λ.π = λ.π._splice_onto_lower(π)
    return λ
```

Many types other than [`Application`](#application), [`Procedure`](#application),  or [`Var`](#var) evaluate to themselves, so `EVAL` does not need cases for them. `EVAL` only needs more cases for collections. [We test that below](#test-collections) after defining [`LET_STAR`](#let-star) and [`LET`](#let).

```python
from typing import Any, Dict, Tuple, List
import numpy

Atom = Union[str, int, float, bool]
Expression = Union[
    Dict, Tuple, List, numpy.ndarray,
    Var, Application, Procedure, Atom]

def EVAL(
        expr: Expression,
        π: Environment = ΓΠ,
        tag=None  # Call EVAL with tag="debug" to get a printout.
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
    else: # Handle self-evaluating types.
        ρ = expr
    return ρ  # Hang a breakpoint here.
```

## Test Var Lookup


Earlier, we bound the little [Greek](#greek) system-test variable `γόὂ` in [section "Global Environment"](#global-environment).

```python
EVAL(Var('γόὂ'))
```

Without `VAR`, strings are literal data.

```python
type(EVAL('γόὂ'))
```

<!-- #region tags=[] -->
# APPLY(proc, args, $\pi$)<a id="apply"></a>
<!-- #endregion -->

It's easier to mentally track environment chaining if one remembers that every procedure call creates a new environment for binding [formal parameters](#formal-parameters) to [actual arguments](#actual-arguments), and that [free variables](#free-variables) are bound anywhere in the closure chain, i.e., the _define-time environment_ of the procedure.


> __MEMORIZE__: `APPLY` (1) makes a new environment parented in the closure-chain environment (default, $\Gamma\Pi$), then (2) evaluates actual arguments in the old, parent environment, then (3) binds parameters in the new environment to the values of the actual arguments.


Optimization: if a procedure has no parameters, `APPLY` doesn't make a fresh environment.


TODO: explore use cases where the environment for actual arguments given to `APPLY` is different from the define-time environment of the procedure given to `APPLY`.

```python
class IllegalArgumentsError(ValueError):
    pass

def APPLY(
        proc: Procedure,
        args: Union[List[Expression], None] = None,  # Python doesn't like mutable [] here, ...
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

```python
APPLY(ΓΠ.square, [42])
```

Call `APPLY` via "Pythonic" round brackets:

```python
ΓΠ.square(42)
```

Works on anonymous procedures, too:

```python
Λ(lambda π: π.x * π.x, ['x'])(42)
```

Scheme equivalent:

<!-- #raw -->
> ((lambda (x) (* x x)) 42)
1764
<!-- #endraw -->

Test multiple parameters and arguments:

```python
Λ(lambda π: π.x * ECHO('π', π).y, ['x', 'y'])(8, 7)
```

Test [ignored parameters](#ignored-parameter):

```python
Λ(lambda π: π.x * ECHO('π', π).y, ['x', 'y','z'])(8, 7, None)
```

Scheme equivalent (must define a name because Gambit `break` doesn't work on anonymous lambda expressions); Gambit debugger prints the entire environment chain excepting the global environment:

<!-- #raw -->
> (goo 8 7)
*** STOPPED IN goo, (stdin)@56.20
x = 8
y = 7
1> ,c
56
<!-- #endraw -->

The bound variables in the procedures, for example, `x`, don't leak: they are not bound in $\Gamma\Pi$:

```python
try:
    ΓΠ.x
except Exception as e:
    print(e.args)
```

Scheme equivalent:

<!-- #raw -->
> x
*** ERROR IN (stdin)@63.1 -- Unbound variable: x
<!-- #endraw -->

## Test `Application`


Recall that an [`Application`](#application) is a data object representing a procedure call on actual arguments.

```python
ωfoo = Application(ΓΠ.square, [42])
```

Notice that the parameters are _not bound to actual arguments when the `Application` is defined_, only when the `Application` is evaluated, that is, until call-time of the procedure in the `Application`. The following does not have a binding for the `x` parameter of `square`:

```python
ECHO('ωfoo', ωfoo);
```

To perform the call, `EVAL` the `Application`.

```python
EVAL(Application(ΓΠ.square, [42]))
```

Scheme equivalent:

<!-- #raw -->
> square
#<procedure #7 square>
> (define omegafoo '(square 42))
> (eval omegafoo)
1764
<!-- #endraw -->

 Internally, `EVAL` calls `EVAL_APPLICATION`. The results are identical.

```python
EVAL_APPLICATION(Application(ΓΠ.square, [42]))
```

Round-bracket _call_ notation also works, but don't pass arguments. As a procedure, an `Application` is a [_thunk_](#thunk), a procedure with no arguments:

```python
Application(ΓΠ.square, [42])()
```

Test the [Greek](#greek) shortcut $\Xi$ for `Application`, now treating the string in the procedure slot implicitly as a `Var` looked up in $\Gamma\Pi$:

```python
Ξ('square', [42])()
```

`Applications` require explicit `Vars` to help with actual arguments looked up in $\Gamma\Pi$:

```python
Ξ('square',  # find proc in global env
   [Var('γόὂ')])()  # find binding in global env
```

Without the `Var`, `EVAL` treats `'γόὂ'` as a Python string, producing a `TypeError`:

```python
try:
    EVAL(Ξ('square',  # find proc in global env
           ['γόὂ']))  # get horribly confused
except TypeError as e:
    print(e.args)
```

Scheme equivalent (Scheme doesn't need `Var`, [though Clojure has it](https://clojure.org/reference/vars)):

<!-- #raw -->
> (define gammaoo 43)
> (square gammaoo)
1849
<!-- #endraw -->

[`Applications`](#application) may have explicit procedures, instead of strings, in their first slot:

```python
EVAL(
    Ξ(Λ(lambda π: π.x * π.x, ['x']),
      [Var('γόὂ')]  # find binding in global env
     ))
```

Scheme equivalent:

<!-- #raw -->
> (eval '((lambda (x) (* x x)) gammaoo))
1849
<!-- #endraw -->

$\Xi$ honors sub-environments even when looking up procedures and arguments by their symbolic names. Expect the fresh env to have a binding for `ϕοοβαρ`:

```python
Λ(lambda π:  # fresh env created by implied call of APPLY
  EVAL(
      Ξ('square',  # Look me up in ΓΠ.
        [Var('ϕοοβαρ')]),  # Look me up in fresh env chained to π.
      ECHO('π', π)),  # Pass the fresh env created above to EVAL.
  ['ϕοοβαρ'])(  # formal parameter of the Λ. The open paren implicitly calls APPLY.
    42)  # actual argument of implied call of APPLY, which makes fresh env
```

$\phi\omicron\omicron\beta\alpha\rho$ is not bound in the $\pi$ of the $\Lambda$, wchich is $\Gamma\Pi$, as we see by attempting the `EVAL` in $\Gamma\Pi$:

```python
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

$\Gamma\Pi$ is the default of `EVAL`:

```python
try:
    Λ(lambda π:
      EVAL(
          Ξ('square',
            [Var('ϕοοβαρ')])),  # <~~~ no env means ΓΠ
      ['ϕοοβαρ'])(
        42)
except NameError as e:
    print(e.args)
```

Scheme equivalent:


> __WARNING__: [Gambit Scheme's eval](https://gambitscheme.org/latest/manual/#index-_002ddebug_002denvironments), unlike [r5rs](https://schemers.org/Documents/Standards/R5RS/r5rs.pdf), always evaluates in the global environment.

<!-- #raw -->
> ((lambda (foobar) (eval '(square foobar))) 42)
*** ERROR -- Unbound variable: foobar
<!-- #endraw -->

Finally, round brackets -- call syntax on $\Xi$ -- can take an optional environment argument:

```python
Λ(lambda π:  # fresh env created by implied call of APPLY
    Ξ('square',  # Look me up in ΓΠ.
      [Var('ϕοοβαρ')])(  # round brackets on the Ξ evaluate it ...
      ECHO('π', π)),  # ... in this env, the fresh env of the outer call
  ['ϕοοβαρ'])(  # formal parameter of the Λ; open paren implicitly calls APPLY
    42)  # actual argument of implied call of APPLY, which makes fresh env
```

There is no equivalent in Gambit Scheme.

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# DEFINE(sym, val, $\pi$)<a id="define"></a>
<!-- #endregion -->

Package up the "defining" boilerplate.


By default, `DEFINE` binds symbols in $\Gamma\Pi$. The "return value" is consistent with Gambit Scheme, which doesn't return anything from a `define`.

```python
def DEFINE(
        sym: str,
        val: Any,
        π: Environment=ΓΠ  # default
) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
    return None
```

## Examples:<a id="fancy"></a>


Do some fancy stuff:

```python
import numpy
ΓΠ.square(numpy.array([[3, 4],[1, 2]]))
```

> __IMPORTANT TODO__: Notice we are not delving into Scheme's numerical semantics, just piggy-backing on the Python ecosystem.

```python
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

```python
import numpy
ΓΠ.saxpy(numpy.array([[1, 2, 3], [4, 5, 6]]),
         numpy.array([[7], [11],[13]]),
         numpy.array([[42], [43]]))
```

or just some ordinary stuff:

```python
ΓΠ.saxpy(4, 10, 2)
```

## SICP 3.2.2<a id="sicp-322"></a>

```python
DEFINE('square',
       Λ(lambda π: π.x * π.x, ['x']))

DEFINE('sum_of_squares',
       Λ(lambda π: π.square(π.x) + π.square(π.y), ['x', 'y']))

DEFINE('f',
       Λ(lambda π: π.sum_of_squares(1 + π.a, 2 * π.a), ['a']))

ΓΠ.f(5)
```

Having `f` defined will bother us below. Get rid of it now.

```python
try:
    del ΓΠ.ϕ.f
except:
    pass
```

## SICP Exercise 3.9

```python
DEFINE('factorial',
       Λ(lambda π: 1 if π.n < 2 else \
         π.n * π.factorial(π.n - 1), ['n']))

ΓΠ.factorial(6)
```

This next example doesn't tail-recurse because Python does not tail-recurse. We mitigate that in section [Tail Recursion](#tail-recursion).

```python
DEFINE('fact_iter',
       Λ(lambda π: π.product if π.counter > π.max_count else \
         π.fact_iter(
           π.counter * π.product,
           π.counter + 1,
           π.max_count
           ), ['product', 'counter', 'max_count']));

ΓΠ.fact_iter(1, 1, 6)
```

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# Procedures that Apply Procedures
<!-- #endregion -->

Here is a procedure of two parameters, `f` and `x`, that applies `f` to `x`. In the example, bind `f` to `square`, `x` to 42, then invoke `f`.


(This example exposes a gargoyle in Python syntax regarding the positioning of call-parens in the presence of line-comments.)

```python
Λ(lambda E1:     # Calling this λ chains a binding environment E1 to ΓΠ.
  E1.f(ECHO('E1', E1).x),  # Apply E1.f to E1.x.
  ['f', 'x'])(   # formal parameters (open paren must be on this line)
ΓΠ.square, 42)   # <~~~ Bind f to square, x to 42.
```

Scheme equivalent:

<!-- #raw -->
> ((lambda (f x) (f x)) square 42)
1764
<!-- #endraw -->

Here is a procedure that applies an internal anonymous procedure of `n` in `E2`. The outer procedure of `m` in `E1` is rooted in $\Gamma\Pi$. Read bottom-up:

```python
Λ(lambda E1:      # Calling this Λ chains environment E1 to ΓΠ.
  Λ(lambda E2:    # Calling this Λ chains environment E2 to ΓΠ.
    E2.n * ECHO('E2', E2).n,  # <~~~ n is bound in E2;
    ['n']         #      E2 is sibling to E1; parent of E2 is ΓΠ.
   )(             # Parent of E1 is implicitly ΓΠ (open paren must be here).
     ECHO('E1', E1).m),  # <~~~ invocation; Look up m in E1, bind to n in E2.
  ['m'])(42)      # <~~~ Bind m to 42 in E1.
```

Scheme equivalent:

<!-- #raw -->
> ((lambda (m)
   ((lambda (n) (* n n))
    m))
 42)
1764
<!-- #endraw -->

`E1` and `E2` are siblings and can't see one another. In the next example `m` is a [free variable](#free-variable) in the inner $\lambda$, not found in `E2`:

```python
try:
    Λ(lambda E1:      # Calling this λ chains environment E1 to ΓΠ.
      Λ(lambda E2:    # Calling this λ chains environment E2 to ΓΠ.
        E2.n * ECHO('E2', E2).m,  # <~~~ DIFFERENT: m is not found or bound in E2.
        ['n']         #      E2 is sibling to E1.
       )(             # Parent environment implicitly ΓΠ.
          ECHO('E1', E1).m),  # <~~~ invocation. Look up m in E1, bind to n in E2.
      ['m'])(42)      # <~~~ Bind m to 42 in E1.
except NameError as e:
    print(e.args)
```

Scheme _automatically_ chains environments into static closures, in which free variables are bound.

<!-- #raw -->
> ((lambda (m)
   ((lambda (n) (* n m))  ; DIFFERENT
    m))
 42)
1764
<!-- #endraw -->

How do we emulate this? Schemulator requires an explicit chaining parameter because Schemulator does not know there are free variables in the body of the $\Lambda$. Schemulator does not know because Schemulator does not parse, on purpose, [as explained in the introduction](#semantics). If Schemulator were Scheme, and thus parsing, it could determine that free variables occur in the body and then it could automatically chain environments.


Schemulator forces the user to write environments, on purpose.


Here's the example above in Schemulator:

```python
Λ(lambda E1:      # Calling it creates environment E1 in ΓΠ.
  Λ(lambda E2:    # !!!! LOOK BELOW Define this Λ in E1, not in ΓΠ, the default !!!!
    E2.n * ECHO('E2', E2).m,  # <~~~ n in E1, x in E2.
    ['n'],        #      (E2 is child of E1, written E1<--E2)
    E1)(          # !!!! DIFFERENT Parent environment *explicitly* E1 !!!!
         ECHO('E1', E1).m),  # <~~~ Look up n in E1, bind x in E2->E1
  ['m'])(42)      # <~~~ Bind n to 42 in E1
```

There is no Scheme equivalent.


## Return a Closure


Here's a Scheme example that, when called, returns a closure, that is, in-turn, called externally. This also happens to be an example of [_currying_](#thunk).

<!-- #raw -->
> (((lambda (m)
       (lambda (n) (* n m))) 42) 42)
1764
<!-- #endraw -->

Here's the same example in Schemulator. The outer procedure doesn't _call_ the inner closure, but returns it.

```python
Λ(lambda E1:
  Λ(lambda E2:
    E2.n * ECHO('E2', E2).m,
    ['n'],
    ECHO('E1', E1)),
  ['m'])(42)(42)
```

## Static Closure<a id="static-closure"></a>


Static [closures](#closures) are created at define-time. In the next example, we assign a static closure to a Python variable, `foo`, then invoke it externally.

```python
foo = Λ(lambda E1:
        Λ(lambda E2:  # This Λ ...
          E2.n * ECHO('E2', E2).m,
          ['n'],
          ECHO('E1', E1))  # <~~~ This Λ is defined in E1.
        (E1.m),
        ['m'])  # <~~~ This Λ is defined in ΓΠ, the default.
foo(42)  # <~~~ 42 is bound to E1.m, in-turn bound to E2.n.
```

Scheme equivalent:

<!-- #raw -->
> (define foo
    (lambda (m)
      (lambda (n) (* n m))))

> (pp foo)
(lambda (m) (lambda (n) (* n m)))

> (pp (foo 42))
(lambda (n) (* n m))

> (pp ((foo 42) 42))
1764
<!-- #endraw -->

## Dynamic Closure


If we explicitly `EVAL` the inner procedure of `E2` in the environment `E1` of the outer procedure, the evaluator dynamically splices the environments. The downside of this approach is that evaluation of the closure below, `bar`, splices the environments on every invocation. The upside is that it lets us create closures at run time from unclosed or partially closed procedures.

```python
bar = Λ(lambda E1:
        EVAL(
            Λ(lambda E2:  # <~~~ This Λ is ...
              E2.n * ECHO('E2', E2).m,
              ['n']
             ),  # <~~~ ... defined in global, but ...
            ECHO('E1', E1))  # <~~~ ... evaluated in E1; envrmt spliced.
        (E1.m),
        ['m'])
bar(42)
```

Gambit Scheme does not support dynamic closures, i.e., evaluating procedures in environments specified at run time. In the following, there is no way for `m`, as a free variable, to acquire a binding:

<!-- #raw -->
> (define foo
    (lambda (n) (* n m)))

> (with-exception-handler
      pp
    (lambda () (pp (foo 43))))
#<unbound-global-exception #22>
#<type-exception #23>
#!void

> (with-exception-handler
      pp
    (lambda () (pp ((foo 43) 42))))
   #<unbound-global-exception #24>
#<type-exception #25>
#<nonprocedure-operator-exception #26>
#!void
<!-- #endraw -->

If we do both, defining _and_ evaluating the inner $\lambda$ in E1, we get only one copy of E1 containing `m`. Dynamic splicing in [`Environment`](#environment) checks this case and optimizes it away:

```python
Λ(lambda E1:
  EVAL(        # EVAL the inner procedure itself ....
      Λ(lambda E2:
        E2.n * ECHO('E2', E2).m,  # <~~~ n and m are Found in E2 by chaining ...
        ['n'],                    #      ... but n is not Bound in E2.
        E1     # DIFFERENT: !!!! explicitly defined in outer E1
       ), E1)  # .... EVAL in the outer environment E1.
  (E1.m),      # <~~~ Look up m in E1, bind to n in E2.
  ['m'])(42)   # <~~~ Bind m to 42 in E1.
```

There is no Scheme equivalent.


## Shadowing


Because the two variables `m` and `n` are in different environments, they can have the same name. `E2.n` (value 42) ___shadows___ `E1.n` (value 43), meaning that `E1.n` is inaccessible from `E2`.

```python
Λ(lambda E1:
  Λ(lambda E2:
    E2.n * ECHO('E2.n', ECHO('E2', E2).n),
    ['n']),
  ['n'])(43)(42)
```

Scheme equivalent; the inner `n`, bound to 43, is inaccessible:

<!-- #raw -->
> (define foo
    (lambda (n)
      (lambda (n) (* n n))))

> (pp foo)
(lambda (n) (lambda (n) (* n n)))

> (pp (foo 43))
(lambda (n) (* n n))

> (pp ((foo 43) 42))
1764
<!-- #endraw -->

The same happens, even when we explicitly chain environments. See by inspecting the addresses in hex, that `E2` precedes `E1` in the chain, so a lookup of `n` starting at `E2` will find `n` in `E2` and never progress to `E1`.

```python
Λ(lambda E1:
  Λ(lambda E2:
    E2.n * ECHO('E2.n', ECHO('E2', E2).n),
    ['n'],
   ECHO('E1', E1)),
  ['n'])(43)(42)
```

As usual, there is no Scheme equivalent for explicitly chained environments.

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# Procedures that Return Procedures
<!-- #endregion -->

The outer $\Lambda$ below is the identity function: it returns its argument. It's applied to `ΓΠ.square`, which is closed _only_ over the global environment, returning it. The result is applied to 42:

```python
Λ(lambda E1:  # Calling it creates environment E1 in ΓΠ.
  E1.f,       # Just return the value of parameter f.
  ['f'])(     # Parent environment is implicitly ΓΠ.
 ΓΠ.square)(  # <~~~ 'square' is bound in ΓΠ.
 42)          # Apply the returned procedure.
```

Return a fresh anonymous procedure:

```python
Λ(lambda π: π.f, ['f'])(             # identity function as above ...
    Λ(lambda π: π.x * π.x, ['x']))(  # ... applied to anonymous procedure;
42)                                  # Apply the returned procedure.
```

Scheme equivalents:

<!-- #raw -->
> (((lambda (f) f) square) 42)
1764
> (((lambda (f) f) (lambda (x) (* x x))) 42)
1764
<!-- #endraw -->

<!-- #region tags=[] -->
# Thunks and Currying<a id="thunk"></a>
<!-- #endregion -->

> __DEFINITION__: A ___thunk___ is a procedure of no arguments.


> __DEFINITION__: A ___1-thunk___ is a procedure of one argument.


> __DEFINITION__: A ___curried___ function is a procedure of many arguments transformed into a composition of 1-thunks.


For example, in ordinary Python, the following $\lambda$ of `x` and `y`:

```python
(lambda x, y: x + y**2)(43, 42)
```

is curried into the following: a $\lambda$ of `x` returning a $\lambda$ of `y`:

```python
(lambda x: (lambda y: x + y**2))(43)(42)
```

Here are these examples in Schemulator.

```python
Λ(lambda π: π.x + π.y**2, ['x', 'y'])(43, 42)
```

```python
Λ(lambda πo:
  Λ(lambda πi: πi.x + πi.y**2,
    ['y'],
    πo),  # <~~~ explicit chaining
  ['x'])(43)(42)
```

Notice that `x` occurs [free](#free-variables) in the inner $\lambda$ but [bound](#bound-variables) in the outer $\lambda$. As always, be wary of [the confusing terminology "bound variable"](#confusing). Also notice that the curried form requires two invocations, one for each formal parameter of the two 1-thunks.


The name "curried" comes from Haskell Curry, who developed the theory of analyzing all functions as 1-thunks. Haskell Curry also gave his name to the World's most prominent pure functional programming language, Haskell.


In many examples below, we will work with curried functions, but eventually leave them behind because they're often less than intuitive and they are mathematically isomorphic to non-curried forms.

<!-- #region tags=[] -->
# $\Upsilon$: Squaring Square Roots of Functions
<!-- #endregion -->

Anonymous recursive procedures are fundamental; the Ultimate Imperative requires them.


[See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md) for detailed explanations of the following development.


The running examples are factorial and Fibonacci.


Don't forget non-default `πsf` on the inner definition, lest `sf` be unbound in the inner $\lambda$. `sf` is the "square root" of the recursive function we want, square-root in an abstract algebraic sense where function application is multiplication:

```python
Λ(lambda πsf:  # <~~~ Apply this Λ ...
  Λ(lambda πn:
    # Observe the "multiplication" sf(sf):
    1 if πn.n < 1 else πn.n * πn.sf(πn.sf)(πn.n - 1),
    ['n'], πsf),  # <~~~ Don't forget!
  ['sf'])(  # <~~~ ... to a copy of itself.
    Λ(lambda πsf:  # <~~~ bind this Λ to upper 'sf'
      Λ(lambda πn:
        1 if πn.n < 1 else πn.n * πn.sf(πn.sf)(πn.n - 1),
        ['n'], πsf),   # <~~~ Don't forget!
      ['sf']))(6)
```

Scheme equivalent:

<!-- #raw -->
(((lambda (sf)
    (lambda (n)
      (if (< n 1)
          1
          (* n ((sf sf) (- n 1))))))
  (lambda (sf)
    (lambda (n)
      (if (< n 1)
          1
          (* n ((sf sf) (- n 1)))))))
 6)
<!-- #endraw -->

Abstract `sf(sf)(m)` into a `f`, a delayed $\lambda$ of `m`. That means "replace `sf(sf)(m)` with `f(m)` in the business code, and bind `f` to a new $\lambda(m)$ that is `sf(sf)(m)`. The environments are chained through every level, as exhibited by the `ECHO`:

```python
Λ(lambda πsf:
  Λ(lambda πf:  # Domain code d is a function of business code f.
    Λ(lambda πn:  # Business code f is a function of business parameter n.
      1 if πn.n < 1 else πn.n * πn.f(πn.n - 1),
      ['n'], πf),
    ['f'], πsf)(
      Λ(lambda πm:  # Delayed application of business code f = sf(sf).
        πm.sf(πm.sf)(ECHO('πm', πm).m),
        ['m'], πsf)),
  ['sf'])(  # <~~~ Apply to copy of self:
Λ(lambda πsf:
  Λ(lambda πf:  # Domain code d is a function of business code f.
    Λ(lambda πn:  # Business code f is a function of business parameter n.
      1 if πn.n < 1 else πn.n * πn.f(πn.n - 1),
      ['n'], πf),
    ['f'], πsf)(
      Λ(lambda πm:  # Delayed application of business code f = sf(sf).
        πm.sf(πm.sf)(πm.m),
        ['m'], πsf)),
  ['sf']))(6)
```

Scheme equivalent:

<!-- #raw -->
(((lambda (sf)
    ((lambda (f)
       (lambda (n)
         (if (< n 1)
             1
             (* n (f (- n 1))))))
     (lambda (m) ((sf sf) m))))
  (lambda (sf)
    ((lambda (f)
       (lambda (n)
         (if (< n 1)
             1
             (* n (f (- n 1))))))
     (lambda (m) ((sf sf) m)))))
 6)
<!-- #endraw -->

Abstract the ___domain code___ into `d`, a function of the ___business code___ `f`, which, in-turn, is a function of the ___business parameter___ `n`, then involute $\lambda(d)$ and $\lambda(\sqrt{f})$ to bring $\lambda(d)$ outward. Notice that the domain code is isolated and no longer duplicated.


The two steps are broken out in the Scheme equivalents. The Schemulator code below is the result of the second, involuation step:

```python
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

Scheme equivalent, step 1; abstract domain code:

<!-- #raw -->
(((lambda (sf)
    ((lambda (d)
       (d (lambda (m) ((sf sf) m))))
     (lambda (f)
       (lambda (n)
         (if (< n 1)
             1
             (* n (f (- n 1))))))))
  (lambda (sf)
    ((lambda (d)
       (d (lambda (m) ((sf sf) m))))
     (lambda (f)
       (lambda (n)
         (if (< n 1)
             1
             (* n (f (- n 1)))))))))
 6)
<!-- #endraw -->

Scheme equivalent, step 2; involute $\lambda(d)$ and $\lambda(\sqrt{f})$. Notice that domain code is now isolated outside and appears only once:

<!-- #raw -->
(((lambda (d)
    ((lambda (sf)
       (d (lambda (m) ((sf sf) m))))
     (lambda (sf)
       (d (lambda (m) ((sf sf) m))))))
  (lambda (f)
    (lambda (n)
      (if (< n 1)
          1
          (* n (f (- n 1)))))))
 6)
 720
<!-- #endraw -->

Remove the duplication of the squaring code; abstract it (the self-application) into `g`:

```python
Λ(lambda πd:  # function of domain code, d
  Λ(lambda πg:  # generic squaring gizmo
    πg.g(πg.g), ['g'], πd)(
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

Scheme equivalent:

<!-- #raw -->
(((lambda (d)
    ((lambda (g) (g g))
     (lambda (sf)
       (d (lambda (m) ((sf sf) m))))))
  (lambda (f)
    (lambda (n)
      (if (< n 1)
          1
          (* n (f (- n 1)))))))
 6)
 720
<!-- #endraw -->

## Recursivion via $\Upsilon{}1$


Now we can see that the function of domain code `d` is generic. Let's call it $\Upsilon$1 and define it in the global environment. The "1" in the name means that $\Upsilon{}1$ takes domain codes that return [1-thunk](#thunk) business codes, i.e., business codes of one parameter.


The glyph that looks like "Y" is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). To enter it in the notebook, keyboard `\Upsilon` and then `[tab]`. Names in user code should not collide with it if users remember to [avoid Greek](#greek).

```python
DEFINE('Υ1',
       Λ(lambda πd: # Υ1 is function of domain code, d.
         # d is a function of business code f, that
         # returns new business code that can refer to f.
         Λ(lambda πg: πg.g(πg.g), ['g'], πd)(  # λ(g) applied to ...
             Λ(lambda πsf:  # generic square root.
               πd.d(Λ(lambda πm: πm.sf(πm.sf)(πm.m),
                      ['m'], πsf)),
               ['sf'], πd)),
         ['d']))
```

### Fact-Recursive


Write a domain code that returns a business code of recursive, but externally anonymous, `f`:

```python
DEFINE('fact_recursive',
      Λ(lambda π: # Domain code is a function of business code, f ...
        Λ(lambda π: # ... that returns business code that can call f.
          1 if π.n < 1 else π.n * π.f(π.n - 1), # business code
          ['n'], π), # 1 business parameter, n
        ['f'])) # recursive function

ΓΠ.Υ1(ΓΠ.fact_recursive)(6)
```

Scheme equivalent:

<!-- #raw -->
(define (fact-recursive f)
  "domain code; Return a curried business code."
  (lambda (n) (if (< n 1)
                  1
                  (* n (f (- n 1))))))

(define (Y1 d)
  "d is domain code, a function that receives business code."
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda (m) ((sf sf) m))))))

((Y1 fact-recursive) 6)
720
<!-- #endraw -->

## Examples via $\Upsilon{}3$<a id="iterative-factorial"></a>


$\Upsilon$ can be tailored for a given number of business parameters. This one is for three.

```python
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

Scheme equivalent:

<!-- #raw -->
(define (Y3 d)
  "d is domain code, a function that receives business code, a
function of three business parameters."
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda (m c x)
                     ((sf sf) m c x))))))

((Y3 fact-iter) 1 1 6)
720
<!-- #endraw -->

Later, we generalize $\Upsilon$ to any number $N$ of business parameters.


### Fact-Iter


Here is user-level domain code, redefining `fact_iter` in domain-code form. Any domain code is a function of `f`, recursive business code. In this case, `f` is a function of 3 business parameters. This will get us to a tail-recursive solution in the [section on tail recursion](#tail-recursion).

```python
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

Scheme equivalent:

<!-- #raw -->
(define (fact-iter f)
  "domain code; Return a business code of three parameters."
  (lambda (m c x)
    (if (> c x)
        m
        (f (* m c) (+ c 1) x))))

((Y3 fact-iter) 1 1 6)
720
<!-- #endraw -->

<!-- #region tags=[] -->
# Tail Recursion<a id="tail-recursion"></a>
<!-- #endregion -->

> __OBSERVATION__: The Ultimate Imperative requires anonymous _tail-recursive_ procedures.


Thanks to [Thomas Baruchel for the idea of Exceptions to implement tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion).


If users are aware that their domain code is tail-recursive, then they may call it via `LOOP` instead of via $\Upsilon$.


Scheme detects tail recursion automatically. It parses user code and marks un-nested tail calls. Schemulator doesn't parse, [on purpose](#semantics), and [Python infamously avoids tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). In Python and Schemulator, users must invoke tail recursion explicitly. This isn't terrible. Tail-calls are lexically obvious, so users should always know. In Clojure, there is precedent: users explicitly write `loop` and `recur`. In any event, tail-recursive domain code can always be called via non-tail-recursive $\Upsilon$, it just won't be as fast nor as resistent to stack overflow.


## `LOOP3`


`LOOP3` has the same signature as $\Upsilon3$; it takes domain code of business code of three arguments as its sole argument.


The glyph that looks like "P" below is Greek Capital Rho for "recur." Names in user code will not collide with P if users remember to [avoid Greek](#greek). As with $\Upsilon$, Rho, `LOOP`, and `LOOP3` must know their argument counts. That's fixed below with `LOOPN`.


`LOOP3` receives domain code, which receives recursive business code `f` of three business parameters. In the base case of its recursion, `f` just returns a result from the business parameters, else `f` calls `f` recursively. `LOOP3` calls the domain code with `f = P3` (Rho3), which recurses by raising an exception that overwrites arguments with new values. `LOOP3` loops until that exception is not raised.


> __WARNING__: ***Results are undefined if any `LOOP` function is called with non-tail-recursive domain code.***

```python
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

Example:

```python
LOOP3(ΓΠ.fact_iter)(1, 1, 6)
```

Scheme automatically tail-recurses, so explicit alternatives are not necessary. However, they are instructive.


Gambit Scheme has an extension for exceptions, permitting a direct transcription of the implementation above:

<!-- #raw -->
(define (LOOP3 d)
  ;; P3 calls looper with new args:
  (define (P3 m+ c+ x+)
    (raise (list m+ c+ x+)))  ; <~~~ Gambit extension
  (define (looper m c x)
    (with-exception-handler  ; <~~~ Gambit extension
        ;; This is how P3 calls looper, indirectly:
        (lambda (e)
          (apply looper e))
        ;; Pass P3 to domain code. If domain code calls P3,
        ;; looper recurses, otherwise returns result below.
        (lambda ()  ; With-exception-handler requires thunk.
          ((d P3) m c x))))
  looper)

((LOOP3 fact-iter) 1 1 6)
720
<!-- #endraw -->

That is not idiomatic Scheme. Instead, we must use `call-with-current-continuation`, or `call/cc`:

<!-- #raw -->
(define (LOOP3 d)
  (define (P3  m c x)
    (call/cc (lambda (k)
               (k ((d P3) m c x)))))
  P3)

((LOOP3 fact-iter) 1 1 6)
720
<!-- #endraw -->

This is frighteningly consise, like all `call/cc` code, at first glance, but not too bad if explained: `LOOP3` takes user's domain code `d` and closes over it in the _generic_ recursive business code `P3` of three business parameters named (capriciously) `m`, `c`, `x`. The recursive business code `P3` is passed to the domain code `d`. If `(d P3)` calls `P3`, as it should do in the recursive case, then `P3` recurses unconditionally by calling the continuation `k`. Otherwise, in the base case of its recursion, `(d P3)` doesn't call `P3` and `P3` just returns the result of `(d P3)` applied to actual-argument values for `m`, `c`, and `x`, 1, 1, and 6 in this case.


It is often much more difficult to devise scenarios with `call/cc` than it is to explain them. I suppose that is the reason that programmers prefer `try-except` to `call/cc` and mainstream programming languages like Python don't even offer `call/cc` or its generalization, [delimited continuations](https://en.wikipedia.org/wiki/Delimited_continuation).


## Prove It on `fact_iter`


The recursive call via Schemulator $\Upsilon$ blows Python's recursion limit.

```python
try:
    print(ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

Call via `LOOP3` does not. Notice the domain code `fact_iter` is EXACTLY the same as in the recursive call above.

```python
try:
    print(LOOP3(ΓΠ.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

## Tail-Recursive Fibonacci<a id="tail-recursive-fibonacci"></a>


Write domain code for catastropically slow, non-tail-recursive, exponentially diverging Fibonacci:

```python
DEFINE('fib_slow',
       Λ(lambda π:
         Λ(lambda π: 1 if π.n < 2 else
           π.f(π.n - 1) + π.f(π.n - 2), ['n'], π),
         ['f']))

ΓΠ.Υ1(ΓΠ.fib_slow)(6)
```

This is miserable even for $n=23$. Don't call it for bigger arguments.

```python
ΓΠ.Υ1(ΓΠ.fib_slow)(23)
```

The following takes 10 seconds. Uncomment if you want to see the time per iteration: about 1,000 ms; YES, a full second!

```python
# timeit(ΓΠ.Υ1(ΓΠ.fib_slow)(23))
```

Without linearization, Fibonacci 500 would not complete in $10^{30}$ times the Age of the Universe. One way to linearize is tail recursion. Another way is [memoization](#memoization) (_sic:_ not _memorization_).


Tail-recursive memoization is possible but not necessary. A tail-recursive Fibonacci easy and blazingly fast:

```python
DEFINE('fib_iter',
       Λ(lambda π:
         Λ(lambda π: π.b if π.n < 1 else
           π.f(π.b, π.a + π.b, π.n - 1),
           ['a', 'b', 'n'], π),
         ['f']));
```

Check it:

```python
LOOP3(ΓΠ.fib_iter)(0, 1, 23)
```

Time it. The following takes 10 seconds. Uncomment if you want see 250 _micro_ seconds, or so, per loop: 4000 times faster on this argument, 23. Exponentially faster on bigger arguments.

```python
# timeit(LOOP3(ΓΠ.fib_iter)(0, 1, 23))
```

Stress it, remembering that the non-tail-recursive version would not complete in astronomical time:

```python
LOOP3(ΓΠ.fib_iter)(0, 1, 500)
```

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# Memoized [sic] Fibonacci<a id="memoization"></a>
<!-- #endregion -->

We do not bother with Scheme equivalents for code in this chapter. Such would be lengthy and not instructive.


Linearize execution of Fibonacci by recording intermediate results in a memo table instead of recomputing them. This is an easy instance of [_Dynamic Programming_](https://en.wikipedia.org/wiki/Dynamic_programming).


## Curried Memo Table


One way to pass a memo table is through a Curried second argument. We'll need $\Upsilon2C$, generic for 2-parameter, Curried business code:

```python
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


The domain code for a memoized, Curried Fibonacci follows. The parameter `a` is the _accumulator_, _associator_, or memo table, whatever word you like best. This is easiest to read (and to write) from the bottom up. It looks horrendous, but it isn't really. We disambiguated the names of the environment parameters as a guide. We won't do so in the future. In the `ECHO` output, count the 13 levels of chained environments.

```python
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

```python
# timeit(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1])
```

Still blows the recursion limit:

```python
try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(200)[1])
except RecursionError as e:
    print(e.args)
```

We fix that immediately below.


## Memo Table as Business Parameter


Before doing tail-recursion with a memo table, show the memo as un-Curried. Currying is useful in general, but complicates $\Upsilon$. Get rid of it. Notice, also, that we no longer bother to disambiguate the environment parameters, letting Nature take care of it.

```python
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

```python
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

```python
try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 200)[1])
except RecursionError as e:
    print(e.args)
```

```python
try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 250)[1])
except RecursionError as e:
    print(e.args)
```

This code is not tail-recursive, because it sums the results of two tail-recursive calls. An attempt to call it through a specialization of `LOOP` produces incorrect results, as warned above:

```python
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

```python
try:
    print(LOOP2(ΓΠ.fib_fast_uncurried)({}, 250))
except RecursionError as e:
    print(e.args)
```

## Recursive With Memo


Section [Tail-Recursive Fibonacci](#tail-recursive-fibonacci) exhibits a very short solution without a memo table. Could we write a tail-recursive version with a memo table? Is it worth the effort? Perhaps as a mental exercise. It grows into a function of 5 parameters:


Pseudocode:


```
f(r1, r2, a, n, x):
    (a, 1) if n < 1 else
    f(r2, r1 + r2, a | {x - (n-1): r1, x - (n-2): r2}, x)
```


### Non-Tail-Recursive

```python
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

```python
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

```python
ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)
```

<!-- #region tags=[] -->
### Tail-Recusive
<!-- #endregion -->

```python
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

```python
LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)
```

### Test the Limits

```python
try:
    print(ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)
```

```python
try:
    print(LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)
```

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# `LOOP` and $\Upsilon$ of $N$ Business Parameters
<!-- #endregion -->

We finish off [PART 1: BASICS](#basics) with straightforward generalizations of `LOOP` and $\Upsilon$ to any number of business parameters, without undue explanation. We test them extensively below in [the section on `DO`](#do).

```python
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

LOOPN(ΓΠ.fact_iter, ['m', 'c', 'x'])(1, 1, 6)
```

Scheme equivalent:

<!-- #raw -->
(define (LOOPN d)
  (define (PN . L)
    (call/cc (lambda (k)
               (k (apply (d PN) L)))))
  PN)

((LOOPN fact-iter) 1 1 6)
720
<!-- #endraw -->

```python
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
         ['d', 'vars_']));  # <~~~ extra parameters
```

```python
ΓΠ.ΥN(ΓΠ.fact_iter, ['m', 'c', 'x'])(1, 1, 6)
```

Scheme equivalent:

<!-- #raw -->
(define (YN d)
  ((lambda (g) (g g))
   (lambda (sf) (d (lambda L
                     (apply (sf sf) L))))))

((YN fact-iter) 1 1 6)
720
<!-- #endraw -->

# PART II: IMPERATIVES<a id="imperatives"></a>


Everything above is Schemulator set-up. We finally get to the Pynultimate Imperatives.


# SET_BANG<a id="set-bang"></a>


This is assignment without lambda. We do better later, but SICP and _Lambda the Ultimate Imperative_ use it. Implement it just to complete coverage of Chapter 3 of SICP.


This has its own recursive lookup, exactly the same as that in [`Procedure`](#procedure), just for a different purpose.


Tested below in [BLOCK](#block). Like Gambit, we can only `set!` symbols that are already `define`d. Also, like Gambit, we return `None`.

```python
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


In the following implementation, every $\lambda$ must be a [_thunk_](#thunk): a procedure of no arguments (our $\lambda$s always have the conventional parameter $\pi$; our thunks do not use any parameters bound in $\pi$). All but the last thunk are for side-effect; all thunks are evaluated; all but the last value are discarded.


Steele's paper calls this form `BLOCK`. Scheme calls it `BEGIN`. Common Lisp calls it `PROGN`.

```python
def BLOCK(
        *thunks: "Procedure | Procedure",  # <~~~ PEP 438 type notation
        π: Environment = ΓΠ
) -> Any:
    ρ = None
    for thunk in thunks:
        ρ = APPLY(thunk, [], π=π)  # <~~~ thunks take no args
    return ρ
```

## Examples:


### Set-and-Test


This block first sets `x` in $\Gamma\Pi$ to 6 and then accesses that variable:

```python
DEFINE('x', 0)  # <~~~ Binding must preexist in ΓΠ for SET_BANG
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)),
    Λ(lambda π: π.x * 7))
```

Python equivalent (using [Greek](#greek) to avoid collisions with user names). Python $\lambda$s can't have sequences of "statements." We packages sequences of statements in `def`s with made-up names:


As a general habit, the return value of any Python equivalent is called $\rho$. We imclude it specifically so that t a compiler can notice the beginning and the end of a construction. It's also a convenient place to hang a debugger breakpoint.

```python
x = 0
def γ000():
    global x
    x = 6  # <~~~ Λ thunks in BLOCK are automatically called
    ρ = x * 7
    return ρ
γ000()
```

Scheme equivalent:

<!-- #raw -->
> (define x 0)
> (begin (set! x 6)
         (* x 7))
42
<!-- #endraw -->

### Set and Set Again

```python
DEFINE('y', 42)  # <~~~ in ΓΠ
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)),
    Λ(lambda π: SET_BANG('x', π.x * 7, π)),
    Λ(lambda π: π.x * π.y))
```

Python equivalent

```python
y = 42
def γ001():
    global x, y
    x = 6
    x = x * 7
    ρ = x * y
    return ρ
γ001()
```

Scheme equivalent:

<!-- #raw -->
> (define x 0)
> (define y 42)
> (begin (set! x 6)
         (set! x (* x 7))
         (* x y))
1764
<!-- #endraw -->

Check for unbound variables.

```python
try:
    BLOCK(
        Λ(lambda π: SET_BANG('x', 6, π)),
        Λ(lambda π: SET_BANG('x', π.x * 7, π)),
        Λ(lambda π: print({'expect 1764': π.x * π.y})),
        Λ(lambda π: π.z))  # <~~~ no binding
except NameError as e:
    print({'from Schemulator': e.args})
```

Python equivalent:

```python
def γ002():
    global x, y, z
    x = 6
    x = x * 7
    print({'expect 1764': x * y})
    ρ = z
    return ρ
try:
    γ002()
except Exception as e:
    print({'from Python': e.args})
```

Test BLOCK in a non-global environment $\pi$:

```python
Λ(lambda π0:  # <~~~ Make a non-global π.
  print(
  BLOCK(
      Λ(lambda π: SET_BANG('x1', 7, ECHO('π', π)), π=π0),
      Λ(lambda π: SET_BANG('y1', 6, π), π=π0),
      Λ(lambda π: π.x1 * π.y1, π=π0),
      π=ECHO('π0', π0)
  )),
  ['x1', 'y1'])(0, 0)
```

Python equivalent; the only way to combine local variables with squencing is via `def`:

```python
def γ003():
    x1 = 0
    y1 = 0
    def γ004():
        nonlocal x1, y1
        x1 = 7
        y1 = 6
        ρ = x1 * y1
        return ρ
    ρ = γ004()
    return ρ
γ003()
```

BLOCK automatically propagates the environments, so the $\pi_0$ arguments to the individual $\lambda$s are overkill:

```python
Λ(lambda π0:  # <~~~ Make a non-global π.
  print(
  BLOCK(
      Λ(lambda π: SET_BANG('x1', 7, ECHO('π', π))),  # <~~~ overkill
      Λ(lambda π: SET_BANG('y1', 6, π)),  # <~~~ overkill
      Λ(lambda π: π.x1 * π.y1),           # <~~~ overkill
      π=ECHO('π0', π0)  # <~~~ necessary
  )),
  ['x1', 'y1'])(0, 0)
```

Names `x1` and `y1` are not bound in the global environment, even though they were in the non-global environment. The following example proves no "binding leakage."

```python
try:
    ΓΠ.x1
except NameError as e:
    print(e.args)

try:
    ΓΠ.y1
except NameError as e:
    print(e.args)
```

Python equivalent:

```python
try:
    global x1
    _ = x1
except Exception as e:
    print({'from Python', e.args})

try:
    global y1
    _ = y1
except Exception as e:
    print({'from Python': e.args})
```

Check that intermediate lambdas that don't return `None` are NOT a problem:

```python
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π)),
    Λ(lambda π: SET_BANG('x', π.x * 7, π)),
    Λ(lambda π: π.x * π.y),
    Λ(lambda π: π.x * π.y))
```

Check nested `BLOCK`s. Don't forget to wrap the nested `BLOCK` in a [thunk](#thunk)!

```python
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, ECHO('π', π))),
    Λ(lambda π:  # <~~~ Don't forget to wrap it!
      BLOCK(Λ(lambda π: SET_BANG('x', π.x * 7, π)),
            Λ(lambda π: π.x * π.y))))
```

# Clear the Global Environment


Get rid of `x` and `y` and other gaseous bindings lest they cause trouble below. We shouldn't just make a new global environment because some system procedures, like [`DEFINE`](#define), are closed over the old one and we'd just have to specify `ΓΠ` everywhere explicitly rather than using the convenient defaults.

```python
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


Define the Scheme-like synonym `BEGIN` for `BLOCK`:

```python
BEGIN = BLOCK
```

# LET*, LET, LETREC<a id="the-lets"></a>


`LET_STAR` is sequential binding of locals, syntactically like assignment, but purely with $\lambda$ expressions. Later bindings may depend on earlier ones. `LET` is parallel binding, where bindings are independent and unordered, but can't depend on one another. `LETREC` is mutually recursive `LET`, where any bindings may depend on any and all other bindings.


We face a design choice: should the binding pairs be a list of tuples, as in Scheme, or a flat list of pairs, as in Clojure? The former introduces a level of collections, the latter simplifies implementation via slicing, but introduces more checks for types and for element counts. We choose the Scheme-like option because the Schemulator is supposed to be like Scheme on the surface unless there is a profound reason not to be.


Remember that $\Xi$ is a [Greek](#greek) shortcut for [`Application`](#application). Body of any of the _Lets_ must be an `Application` so that the environment $\pi$ can be propagated at the point where it is known.


> __WARNING__: ***Results of `LET*`, `LET`, and `LETREC` are undefined if the body is not an [`Application`](#application) or $\Xi$.***


## LET_STAR<a id="let-star"></a>

```python
def LET_STAR(
        binding_pairs: List[Tuple[str, Expression]],
        body: Expression,
        π: Environment = ΓΠ
) -> Any:
    if len(binding_pairs) == 0:  # <~~~ Empty bindings are allowed.
        ρ = EVAL(body, π)
        return ρ
    key, val = binding_pairs[0]
    E1 = Environment(lambda: None, π)
    setattr(E1.ϕ, key, EVAL(val, π))
    if len(binding_pairs) == 1:
        return EVAL(body, E1)
    else:
        return LET_STAR(binding_pairs[1:], body, E1)
```

### Examples:


Test depth 0, no bindings. The final expression -- the returned value -- can be a `Procedure`:

```python
LET_STAR([],
         Λ(lambda π: 43 * π.x, ['x']))
```

Python equivalent:

```python
lambda x: 43 * x
```

Which can be called:

```python
LET_STAR([],
         Λ(lambda π: 43 * π.x, ['x']))(42)
```

Python equivalent:

```python
(lambda x: 43 * x)(42)
```

Applications are evaluated in place:

```python
LET_STAR([],
         Ξ(Λ(lambda π: 43 * 42)))
```

Python equivalent:

```python
(lambda: 43 * 42)()
```

The `LET_STAR` chains bindings in environments and evaluates applications in the chain. Procedures in applications enjoy free variables from any environments in the chain.

```python
LET_STAR([('z', 42)],
        Ξ(Λ(lambda π: ECHO('π', π).z * π.z)))
```

Python does not have `LET`. Local variables must be defined in `def`s with made-up names (we use [Greek](#greek) to avoid collision with user names. Equivalent to the above:

```python
def γ004():
    z = 42  # <~~~ Local variables must be args or defined in defs.
    ρ = (lambda: z * z)()  # <~~~ Evaluating Ξ calls its Λ.
    return ρ
γ004()
```

Alternative:

```python
def γ005(z):  # <~~~ Local vars can be args.
    ρ = (lambda: z * z)()  # <~~~ Ξ call
    return ρ
γ005(42)
```

Later bindings can acces and can shadow earlier ones:

```python
LET_STAR([('z', 42),
          ('z', Ξ(Λ(lambda π: π.z + 1)))],
        Ξ(Λ(lambda π: ECHO('π', π).z * π.z)))
```

Python equivalent:

```python
def γ006():
    z = 42
    def γ007():  # <~~~ Cust have a def; can't sequence 'nonlocal' stmt in a λ
        nonlocal z  # <~~~ necessary; refers to outer z.
        z = (lambda: z + 1)()  # <~~~ Ξ call
        return z
    z = γ007()  # <~~~ Old z is shadowed.
    ρ = (lambda: z * z)()  # <~~~ Ξ call
    return ρ
γ006()
```

Returned `Procedure` is a closure over local variables:

```python
LET_STAR([('z', 42)],
        Λ(lambda π: π.z * π.z))
```

Python equivalent:

```python
def γ008():
    z = 42
    ρ = lambda: z * z
    return ρ
γ008()
```

As always, the returned procedure may be called externally:

```python
LET_STAR([('z', 42)],
        Λ(lambda π: π.z * π.z))()
```

Python equivalent:

```python
def γ009():
    z = 42
    ρ = lambda: z * z
    return ρ
γ009()()
```

Test depth 1 (don't forget the `Var` in $\Xi$:

```python
LET_STAR([('z', 42)],
         Ξ(ΓΠ.square, [Var('z')]))
```

Python equivalent; `Var('z')` is like `eval('z')`:

```python
def square(x):
    ρ = x * x
    return ρ
def γ010():
    z = 42
    ρ = square(eval('z'))
    return ρ
γ010()
```

### Free Variables<a id="free-variables"></a>


> __DEFINITION__: A ___free variable___ in the body of a lambda is a variable _NOT_ in the parameter list.


`z`, `y`, and `y` are free in the $\lambda$s below:


Test depth 2 with free variables:

```python
LET_STAR([('z', 42),
          ('y', 43)],
         Ξ(Λ(lambda π: π.z * π.y)))
```

Python equivalent:

```python
def γ011():  # <~~~ Environment here will have all bindings.
    z = 42
    y = None  # <~~~ inserted in second pass after 'y' is discovered
    def γ012():
        nonlocal y, z
        y = 43
    γ012()
    ρ = z * y
    return ρ
γ011()
```

Inspect bindings with an `ECHO` around the environment variable:

```python
LET_STAR([('z', 42),
          ('y', 43)],
         Ξ(Λ(lambda π: ECHO('π', π).z * π.y)))
```

Test depth 3 with free varialbes:

```python
LET_STAR([('z', 42),
          ('y', Ξ(Λ(lambda π: π.z + 1))),
          ('w', Ξ(Λ(lambda π: π.z * π.y)))],
         body=Ξ(Λ(lambda π: ECHO('π', π).w)))
```

Python equivalent:

```python
def γ013():
    z = 42
    y = None
    w = None
    def γ014():
        nonlocal y, z  # <~~~ Build up the nonlocals.
        y = z + 1
        def γ015():
            nonlocal w, y, z  # <~~~ Build up the nonlocals.
            w = z * y
        γ015()
    γ014()
    ρ = w
    return ρ
γ013()
```

### Bound Variables<a id="bound-variables"></a>


> __DEFINITION__: A ___bound variable___ in the body of a $\lambda$ is a variable that appears in the parameter list.


Remember that [this definition of bound variables is confusing](#confusing).


### Closed Term<a id="closed-term"></a>


> __DEFINITION__: A ___closed procedure___ or ___closed term___ is a procedure with no free variables (see [this web page](https://web.mat.bham.ac.uk/R.W.Kaye/logic/freevar.html)).


Test depth 3 with bound variables. To access earlier bindings, write an [`Application` or $\Xi$](#application) that, when evaluated, [accesses earlier bindings as `Var`s](#var). Notice the bound vars shadow free vars with the same names.

```python
LET_STAR([('z', 42),
          ('y', Ξ(Λ(lambda π: π.z + 1, ['z']),  # <~~~ z shadows ...
                  [Var('z')])),  # <~~~ ... this Var.
          # Shadowing occurs here, too.
          ('w', Ξ(Λ(lambda π: ECHO('π', π).z * π.y, ['z', 'y']),
                  [Var('z'), Var('y')])
          )],
         body=Ξ(Λ(lambda π0: ECHO('π0', π0).w)))
```

Python equivalent:

```python
def γ016():
    z = 42
    y = None
    w = None
    def γ017():
        nonlocal y, z
        y = (lambda z: z+1)(eval('z'))
        def γ018():
            nonlocal w, y, z
            w = (lambda z, y: z * y)(eval('z'), eval('y'))
        γ018()
    γ017()
    ρ = w
    return ρ
γ016()
```

The names of the bound variables do not matter. In this case, we avoid shadowing, so the names `z`, `y`, and `w` are available in the body of `w`'s procedure:

```python
LET_STAR([('z', 42),
          ('y', Ξ(Λ(lambda π: π.zz + 1, ['zz']),
                  [Var('z')])),
          # Use parameters:
          ('w', Ξ(Λ(lambda π: ECHO('π', π).zzz * π.yy, ['zzz', 'yy']),
                  [Var('z'), Var('y')])
          )],
         body=Ξ(Λ(lambda π: π.w)))
```

Python equivalent:

```python
def γ019():
    z = 42
    y = None
    w = None
    def γ020():
        nonlocal y, z
        y = (lambda zz: zz+1)(eval('z'))
        def γ021():
            nonlocal w, y, z
            w = (lambda zzz, yy: zzz * yy)(eval('z'), eval('y'))
        γ021()
    γ020()
    ρ = w
    return ρ
γ019()
```

Ignore parameters; use free variables:

```python
LET_STAR([('z', 42),
          ('y', Ξ(Λ(lambda π: π.zz + 1, ['zz']),
                  [Var('z')])),
          # Ignore parameters; use free variables:
          ('w', Ξ(Λ(lambda π: ECHO('π', π).z * π.y, ['zzz', 'yy']),
                  [Var('z'), Var('y')])
          )],
         body=Ξ(Λ(lambda π: π.w)))
```

Python equivalent:

```python
def γ019():
    z = 42
    y = None
    w = None
    def γ020():
        nonlocal y, z
        y = (lambda zz: zz+1)(eval('z'))
        # Ignore parameters; use free variables:
        def γ021():
            nonlocal w, y, z
            w = (lambda zzz, yy: z * y)(eval('z'), eval('y'))
        γ021()
    γ020()
    ρ = w
    return ρ
γ019()
```

### Closures<a id="closures"></a>


The body of this next example evaluates `g` without returning a closure:

```python
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
        body=Ξ(Λ(lambda π: ECHO('π', π).g(42))))
```

Python equivalent:

```python
def γ022():
    g = lambda x: x * x
    ρ = g
    return ρ
γ022()(42)
```

But, we may return a closure, meaning that its environment chain is still alive. We may invoke it outside the `LET_STAR`.The application $\Xi$ pulls the closure out of the environment created by `LET_STAR`.

```python
α = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
             body=Ξ(Λ(lambda π: ECHO('π', π).g)))
α(42)
```

Python equivalent:

```python
def γ023():
    g = lambda x: x * x
    ρ = g
    return ρ
β = γ023()
β(42)
```

Without he application `Ξ`, the value is an unevaluated procedure returning a closure:

```python
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
        body=Λ(lambda π: π.g))
```

We can evaluate the returned closure with an extra, explicit call:

```python
foo = LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
               body=Λ(lambda π: π.g))
foo()(42)
```

The Python equivalents were demonstrated above.


Ensure no leakage:

```python
try:
    ΓΠ.g
except Exception as e:
    print(e.args)
```

Python equivalent:

```python
try:
    _ = g
except Exception as e:
    print({'from Python': e.args})
```

Procedures can be bound variables, too, with arbitrary names:

```python
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
         body=Ξ(Λ(lambda π: π.gg(42), ['gg']), [Var('g')]))
```

Python equivalent:

```python
def γ024():
    g = lambda x: x * x
    ρ = (lambda gg: gg(42))(eval('g'))
    return ρ
γ024()
```

Unevaluated, it's still a closure:

```python
LET_STAR([('g', Λ(lambda π: π.x * π.x, ['x']))],
         body=Ξ(Λ(lambda π: π.gg, ['gg']), [Var('g')]))(
42)
```

Python equivalent:

```python
def γ025():
    g = lambda x: x * x
    ρ = (lambda gg: gg)(eval('g'))
    return ρ
γ025()(42)
```

## LET<a id="let"></a>


`LET` is parallel "assignment." All "local variables" must be bound in the enclosing environment and may not depend on one another. This implementation is not curried. `body` is usually an [`Application`](#application) that automatically receives the local variables in its environment. The $\Lambda$ argument of the `Application` receives this environment as its $\pi$ parameter, so it may access the local variables as free variables in its body.

```python
def LET(
        pairs: List[Tuple[str, Expression]],
        body: Expression,
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


Test depth 0; embedded $\Lambda$ is a thunk (no parameters):

```python
LET([],
    Ξ(Λ(lambda π: print(43 * 42))))
```

Test depth 1; embedded `Procedure` takes one argument, supplied as a `Var` in the environment of the `LET`:

```python
LET([('z', 42)],
    Ξ(ΓΠ.square, [Var('z')]))
```

Test depth 2; embedded `Procedure` receives an environment with bindings for the local variables:

```python
LET([('z', 42),
     ('y', 43)],
    Ξ(Λ(lambda π: ECHO('π', π).z * π.y)))
```

Depth 2 with external invocation / evaluation; body -- return value -- of the `LET` is an un-called `Procedure`. The `Procedure` is evaluated, but not called, to propagate the environment, before it is returned:

```python
LET([('z', 42),
     ('y', 43)],
    Λ(lambda π: π.z * π.y))()
```

Reversed; order does not matter in `LET`:

```python
LET([('y', 42),
     ('z', 43)],
    Ξ(Λ(lambda π: π.z * π.y)))
```

With applications as values for local variables, the inner `y` is evaluated in the local environment, not leaking down from the global environment $\Gamma\Pi$, where `y` is 0:

```python
DEFINE('y', 0)
LET([('y', 42),
     ('z', Ξ(Λ(lambda π: π.y + 1)))],  # Outer y = 0, not inner y = 42
    Ξ(Λ(lambda π: print(π.z * π.y))))  # Inner y = 42 * inner z = 1
```

Order does not matter:

```python
LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)],
    Ξ(Λ(lambda π: print(π.z * π.y)))) # Inner y = 42 * inner z = 1
```

Print the environment to check that all symbols are bound in it:

```python
LET([('z', Ξ(Λ(lambda π: π.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)],
    Ξ(Λ(lambda π: print(ECHO('π', π).z * π.y)))) # Inner y = 42 * inner z = 1
```

Prove global `y` is unchanged and that `z` is bound only in local environment, not global.

```python
print({'expect y = 0':  ΓΠ.y})
try:
    print(ΓΠ.z)
except NameError as e:
    print(e.args)
```

Test nested `LET`. Don't forget to chain the environments! The default is $\Gamma\Pi$.

```python
LET([('z0', 42)],
    Ξ(Λ(lambda π0:
        LET([('y0', Ξ(Λ(lambda π1: π1.z0 + 1)))],
            Ξ(Λ(lambda π2: π2.z0 * π2.y0)),
            π=π0))))  # <~~~ Don't forget to chain!
```

The free variable `x` in the $\lambda$ below is looked up in the local environment established by `LET`.


First, delete `x` again, just in case, so we can check that it does not get bound accidentally.

```python
try:
    del ΓΠ.ϕ.x
except:
    pass
```

```python
LET([('x', 42)],
   Ξ(Λ(lambda π: π.x * π.x)))
```

The variable `x` did not leak out of the local environment:

```python
try:
    ΓΠ.x
except Exception as e:
    print(e.args)
```

We can get the same result as above when the internal $\lambda$ does not have free variables. When evaluating the application $\Xi$, grab the local value of `x` as an actual argument and substitute it for the bound variable `y` in the body of the $\lambda$:

```python
LET([('x', 42)],
   Ξ(Λ(lambda π: π.y * π.y, ['y']), [Var('x')]))
```

### Test `EVAL` on Collections<a id="test-collections"></a>


[Notice that `EVAL` recurses into Dicts, Tuples, Lists, and numpy arrays](#eval). `LET_STAR` and `LET` gives us good tools for testing that.


Lists, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.x * π.y)))
```

With bound variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: [π.forty_two, π.forty_two + 1]))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.xx * π.yy, ['xx', 'yy']), [Var('x'), Var('y')]))
```

Tuples, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: (π.forty_two, π.forty_two + 1)))),
    ('y', 3)],
   Ξ(Λ(lambda π: π.x * π.y)))
```

Dictionaries, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: {'π.forty_two': π.forty_two,
                         'forty-three': π.forty_two + 1}))),
    ('y', 3)],
   Ξ(Λ(lambda π: [π.x['π.forty_two'],
                  π.x['forty-three']] * π.y)))
```

Numpy arrays, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', Ξ(Λ(lambda π: numpy.array(
        [π.forty_two,
         π.forty_two + 1])))),
    ('y', 3)],
    Ξ(Λ(lambda π: π.x * π.y)))
```

## LETREC<a id="letrec"></a>


`LETREC` binds codependent values in a new environment _before_ evaluating them. `LET` evaluates values before binding them.


`LETREC` patches the environments of any contained procedures to ensure they have access to new bindings. [Lengthy code in `EVAL` does the monkey-patching for us](#eval) later. `LETREC` differs from `LET` only in the absence of `EVAL`s on the value side. Those `EVAL`s are done later.

```python
def LETREC(
        pairs: List[Tuple[str, Any]],
        body: Expression,
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


Factorial, familiar by now, accesses itself through the patched environment; `fact` is a free variable in the body of `fact`:

```python
LETREC([('fact',
         Λ(lambda π:
           (π.a
            if π.m <= 0
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Ξ(Λ(lambda π: π.fact(6, 1))))
```

Here is a version that fails if the patching is not done correctly:

```python
Λ(lambda πo:
  LETREC([('fact',
           Λ(lambda π:
             (π.a
              if π.m <= π.m0  # <~~~ Watch out!
              else π.fact(π.m - 1, π.m * π.a)),
             ['m', 'a']))],
         Ξ(Λ(lambda π: π.fact(6, 1))),
         πo),  # <~~~ access 'm0'
 ['m0'])(0)
```

Now fully parameterized:

```python
Λ(lambda πo:
  LETREC([('fact',
           Λ(lambda π:
             (π.a
              if π.m <= π.m0  # <~~~ Watch out!
              else π.fact(π.m - 1, π.m * π.a)),
             ['m', 'a']))],
         Ξ(Λ(lambda π: π.fact(π.n, 1))),
         πo),  # <~~~ access 'm0' and 'n'
 ['m0', 'n'])(0, 6)
```

The final application $\Xi$ is necessary to actually evaluate the final $\Lambda$, lest it be simply returned unevaluated:

```python
LETREC([('fact',
         Λ(lambda π:
           (π.a
            if π.m <= 0
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Λ(lambda π: π.fact(6, 1)))
```

But we can evaluate it:

```python
LETREC([('fact',
         Λ(lambda π:
           (π.a
            if π.m <= 0
            else π.fact(π.m - 1, π.m * π.a)),
           ['m', 'a']))],
       Λ(lambda π: π.fact(6, 1)))()  # <~~~ extra evaluation!
```

One can unroll the final application into formal parameters and actual arguments:

```python
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

```python
LETREC([('z0', Λ(lambda π: 1 + π.y0(), ['y0'])),
        ('y0', Λ(lambda π: 42))],
      Ξ(Λ(lambda π: π.y0() * π.z0(π.y0))))
```

Check that `y0` does not leak into the global environment:

```python
try:
    print(ΓΠ.y0)
except NameError as e:
    print(e.args)
```

The following shows that `z0` also does not leak from `LETREC`:

```python
try:
    print(ΓΠ.z0)
except NameError as e:
    print(e.args)
```

The following example is [borrowed from the Racket documentation](https://docs.racket-lang.org/reference/let.html).

```python
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

### TODO: Tanai

<!-- #region tags=[] -->
### [Tak](https://en.wikipedia.org/wiki/Tak_(function))
<!-- #endregion -->

Here is [`tak`](https://en.wikipedia.org/wiki/Tak_(function)) in ordinary Python, with a global counter of number of calls.

```python
ntakcalls = 0
def tak(x, y, z):
    global ntakcalls
    ntakcalls += 1
    r = (tak(tak(x - 1, y, z),
             tak(y - 1, z, x),
             tak(z - 1, x, y))
         if y < x else
         z)
    return r
```

```python
ntakcalls = 0
print(tak(3, 2, 1))

print({"ntakcalls": ntakcalls})
```

Here is a version with `yield` from a `contextmanager`; TODO: eventually implement this with a `call/cc` in Scheme and an `Exception` in Python:

```python
from contextlib import contextmanager

@contextmanager
def wtak():
    global ntakcalls
    ntakcalls = 0
    yield
    print({"ntakcalls": ntakcalls})

with wtak():
    print(tak(12, 8, 4))
```

```python
with wtak():
    print(tak(18, 12, 6))
```

```python
with wtak():
    print(tak(28, 20, 12))
```

#### Functional Instrumented Tak


Here is a functional flavoring of the instrumentation, not requiring "non-local" control

```python
def itak(c, x, y, z):
    """c is the number of tak calls so far."""
    itakx = lambda c: itak(c, x - 1, y, z)
    itaky = lambda c: itak(c, y - 1, z, x)
    itakz = lambda c: itak(c, z - 1, x, y)
    if y < x:
        lx = itakx(c + 1)
        ly = itaky(lx[0] + 1)
        lz = itakz(ly[0] + 1)
        lr = itak(lz[0] + 1, lx[1], ly[1], lz[1])
        return lr
    else:
         return (c, z)
```

```python
itak(1, 3, 2, 1)
```

```python
itak(1, 12, 8, 4)
```

```python
itak(1, 18, 12, 6)
```

```python
itak(1, 28, 20, 12)
```

#### Instrumented Recursive Tak


Scheme equivalent:

<!-- #raw -->
(define (itak c x y z)
  (let ((itakx (lambda (c) (itak c (- x 1) y z)))
        (itaky (lambda (c) (itak c (- y 1) z x)))
        (itakz (lambda (c) (itak c (- z 1) x y))))
    (if (< y x)
     (let* ((lx (itakx (+ c 1)))
            (ly (itaky (+ (car lx) 1)))
            (lz (itakz (+ (car ly) 1))))
       (itak (+ (car lz) 1) (cadr lx) (cadr ly) (cadr lz)))
     (list c z))))

(pp (itak 1  3  2  1))  ; (5 2)
(pp (itak 1 12  8  4))  ; (1733 5)
(pp (itak 1 18 12  6))  ; (63609 7)
;; (pp (itak 1 28 20 12))  ; (2493349 13)
<!-- #endraw -->

Schemulator:

```python
DEFINE('irtak',
       Λ(lambda πo:
         LET([('itakx', Λ(lambda π: ΓΠ.irtak(π.c, π.x - 1, π.y, π.z), ['c'])),
              ('itaky', Λ(lambda π: ΓΠ.irtak(π.c, π.y - 1, π.z, π.x), ['c'])),
              ('itakz', Λ(lambda π: ΓΠ.irtak(π.c, π.z - 1, π.x, π.y), ['c']))],
             Ξ(Λ(lambda πi:
                 LET_STAR(
                     [('lx', Ξ(Λ(lambda π: π.itakx(π.c + 1)))),
                      ('ly', Ξ(Λ(lambda π: π.itaky(π.lx[0] + 1)))),
                      ('lz', Ξ(Λ(lambda π: π.itakz(π.ly[0] + 1))))],
                     Ξ(Λ(lambda πii:
                         ΓΠ.irtak(
                             πii.lz[0] + 1,
                             πii.lx[1],
                             πii.ly[1],
                             πii.lz[1]))),
                     π=πi  # <~~~ don't forget me!
                 ) if πi.y < πi.x
                 else
                 (πi.c, πi.z)
                )),
             π=πo),
         ['c', 'x', 'y', 'z']))
```

```python
ΓΠ.irtak(1, 3, 2, 1)  # expect (5, 2)
```

```python
ΓΠ.irtak(1, 12, 8, 4)  # expect(1733, 5)  # 72msec
```

```python
# assert ΓΠ.irtak(1, 18, 12, 6) == (63609, 7)  # 2sec 521msec
# assert ΓΠ.irtak(1, 28, 20, 12) == (2493349, 13)  # 1min 36sec
```

#### Tak with $\Upsilon$

```python
DEFINE('tak',
      Λ(lambda πt:
        Λ(lambda π:
          π.tak(
              π.tak(π.x - 1, π.y, π.z),
              π.tak(π.y - 1, π.z, π.x),
              π.tak(π.z - 1, π.x, π.y))
          if π.y < π.x else
          π.z, ['x', 'y', 'z'], πt),
        ['tak']))
ΓΠ.ΥN(ΓΠ.tak, ['xt', 'yt', 'zt'])(18, 12, 6)
```

Too slow:

```python
# ΓΠ.Υ3(ΓΠ.tak)(28, 20, 12)
```

# LABELS<a id="labels"></a>


`LABELS` is a special case of `LETREC` where all the values are mutually codependent procedures.

```python
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

```python
LABELS([('fact_iter_nom',
        Λ(lambda π:
        (π.a
         if π.m <= 0
         else π.fact_iter_nom(π.m - 1, π.a * π.m)),
       ['m', 'a']))],
      Ξ(Λ(lambda π: π.fact_iter_nom(6, 1))))
```

Test monkey patching again:

```python
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

<!-- #region jp-MarkdownHeadingCollapsed=true tags=[] -->
# DO<a id="do"></a>
<!-- #endregion -->

Here is the specification of `DO` from Steele's paper, without further explanation here:

<!-- #raw -->
(DO ((<var1> <init1> <step1>)
     (<var2> <init2> <step2>)
     ...
     (<varN> <initN> <stepN>))
    (<pred> <value>)
    <optional body>)
<!-- #endraw -->

First, a non-tail-recursive version:

```python
def CHECK_TYPE(x: Any, t: Any) -> Any:
    assert isinstance(x, t)
    return x

def DO_NTC(
        triples: List[Tuple[str, Expression, Procedure]],
        pred: Procedure,
        value: Expression,
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
           else πb.λoop(  # <~~~ non-tail recursion
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

```python
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

```python
try:
    Λ(lambda πo:
      DO_NTC([('m', πo.m, Λ(lambda π: π.m - 1)),
              ('a', 1, Λ(lambda π: π.a * π.m))],
             pred=Λ(lambda π: π.m <= 1),
             value=Λ(lambda π: π.a),
             body=Λ(lambda π: None),
             π=πo),
      ['m'])(800)
except RecursionError as e:
    print(e.args)
```

A tail-recursive version, with the natural name `DO`. This uses `LOOPN`, defined earlier. Because tail-recursion must be the base case in Schemulator, we don't bother to test $\Upsilon{}N$, here:

```python
def DO(
        triples: List[Tuple[str, Expression, Procedure]],
        pred: Procedure,
        value: Procedure,
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
        Ξ(Λ(lambda πl:
            LOOPN(πl.λoop, ['βody_result', *vars])  # <~~~ Tail Recursion
            (None, *[EVAL(init, E1) for init in inits]))),
        E1)
    return r
```

```python
Λ(lambda πo:
  DO([('m', πo.m, Λ(lambda π: π.m - 1)),
      ('a', 1, Λ(lambda π: π.a * π.m))],
     pred=Λ(lambda π: π.m <= 1),
     value=Λ(lambda π: π.a),
     body=Λ(lambda π: None),
     π=πo),
  ['m'])(6)
```

Now the recursion limit is not blown:

```python
Λ(lambda πo:
  DO([('m', πo.m, Λ(lambda π: π.m - 1)),
      ('a', 1, Λ(lambda π: π.a * π.m))],
     pred=Λ(lambda π: π.m <= 1),
     value=Λ(lambda π: π.a),
     body=Λ(lambda π: None),
     π=πo),
  ['m'])(800)
```

# Junkyard / Workshop


Ignore everything below. It's saved in case we need it someday.


## GOTO


## COND


## Named Let


## Partial Evaluation and Automatic Currying


## QUOTE, QUASIQUOTE, UNQUOTE, MACROS<a id="quote"></a>


## Normal-Order (Lazy) Evaluation


## Streams (scons, scar, scdr, ...)


## Tanai


## Laziest Tanai


## Types


## Primitive Functions

```python
DEFINE('γ2', 42)
```

```python
ΓΠ.γ2
```

```python
ΓΠ["γ2"]
```

```python
π_integer_bin_ops = Environment(lambda: None, ΓΠ)
```

```python
π_integer_bin_ops
```

```python
DEFINE("plus_int_int",
       Λ(lambda π:
         ECHO('+ii:a', π.a) + ECHO('+ii:b', π.b),
         ['a', 'b']),
       π_integer_bin_ops)

# P.FIII = (int, int) -> int
# P.FDID = X
# P.FDDD = (double, double) -> double

# U.FIII = (int, int) -> int
# U.FDID = (double, int) -> double
```

```python
Λ(lambda π: ECHO('π', π).plus_int_int(π.γ2, 2), π=π_integer_bin_ops)()
```
