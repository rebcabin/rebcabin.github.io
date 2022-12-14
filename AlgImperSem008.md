---
jupyter:
  jupytext:
    formats: ipynb,md
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.14.1
  kernelspec:
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

# Algebraic Imperative Semantics


or, Lambda, The Pynultimate Imperative


**Version 8**


**Brian Beckman**


**30 Dec 2022**


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

# Environment and Frame<a id="environment"></a>


The material in this section will remind you of "Operations on Environments" in [SICP 4.1.3](https://sarabander.github.io/sicp/html/4_002e1.xhtml#g_t4_002e1_002e3).


We model Scheme's environments and frames explicitly. We tried multiple Pythonic short-cut alternatives and found that none compose well.


[SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) has apparent contradictions in the definition of environment and frame. It says that "an environment is a sequence of frames," but the rest of the text and figures clearly imply that an environment has just one frame.


The best resolution appears to be:


> __DEFINITIONS__: An ___environment___ is a frame $\phi$ and a pointer $\pi$ to an enclosing environment. A ___frame___ is a mathematical function from variable names to values; no variable name may appear more than once in a frame.


We note in passing that this works only for a single thread. [Clojure, for instance, solves that problem with _Vars_](https://clojure.org/reference/vars).


## Greek and ALL CAPS<a id="greek"></a>


$\pi$ for an enclosing environment is a nice pun: it evokes $\pi\eta\rho\iota$, a Greek prefix meaning "surrounding," as in "perimeter" (_sic_, not "parameter").


This notebook uses Greek letters in the names system attributes and variables. These letters won't collide with user symbols if users follow the rule below. With this rule, don't need `gensym`, a thorn in every programmer's side:


> __RULE__: Avoid Greek in user-level Python code to avoid clobbering system-supplied names.


Greek is problematic in my installation of Gambit, so I avoid Greek in Scheme code.


Names in ALL CAPS denote Schemulator procedures visible at user level.


## Bindings<a id="bindings"></a>


> __DEFINITION__: A ___binding___ is an association from a variable name to a value. A binding is an entry in a frame.


We might model a binding as a pair, a row in a table, an element of a relation (subset of a Cartesian product), an _item_ in a Python dictionary, or as an attribute of a Python object. We prefer attributes of objects because they afford _dot_ notation, that is, `obj.foo`. Dot notation is shorter than dictionary syntax `dict['foo']`. Thanks to [divs1210](https://gist.github.com/divs1210?page=3) for this idea.


If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a sequence of frames via the chain of pointers to enclosing environments.


> __INVARIANT__: The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`.


> __OBSERVATION__: A frame $\phi$ belongs to a sequence of environments implied by the unidirectional chain of enclosing environments ending at the unique global environment.


> __DEFINITIONS__: The ___value of a variable in an environment___ is the value in the first binding in any frame of the sequence ending at _global_. Bindings lower in the chain may ___shadow___ bindings higher in the chain. If no frame in a chain has a binding for a variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another. 


> __LEMMA__: A chain of environments implements a mathematical partial function from variable names to values. Shadowing ensures that any name appearing more than once in a chain has only one value when looked up.


Informally, an accessible variable in a chain is a _found_ variable. For all intents and purposes, a _found_ variable is a _bound_ variable and vice versa, though [beware of the confusing usage of the term "bound variable."](#confusing)


In the Environment class below, `__getattr__` is overridden to avoid a separate method for recursive lookup. We can't also override `__setattr__` to call `setattr(self.?? ...)` because `self.??` diverges on `getattr(self.?? ...) = getattr(getattr(getattr...))`.


The `Environment` class is lengthy only due to code for _splicing_, necessary to bind [___free variables___](#free-variables) in procedure bodies. Jupyter notebooks do not furnish a convenient way to break up the code for classes, so we put essential information in comments; read them as if they were text in the notebook.

```python
from dataclasses import dataclass, field
from types import FunctionType
from typing import Any
@dataclass
@dataclass
class Environment:
    """An Environment has a frame ?? and a pointer ?? to an enclosing
    Environment. Bindings in the frame are _attributes_ on the
    object ??. Choose an empty function as the carrier object for
    such bindings / attributes. Bindings / attributes have a name and
    a value. Retrieve the value of binding ?? in frame ?? of
    Environment E via dot notation as in 'E.??.??'. Set the value v
    of binding ?? via 'setattr(E.??, ??, v)'. When getting values of
    bindings, it's OK to omit the ??, writing 'E.??', because of the
    overloaded __getattr__ of this class, Environment. Bracket
    notation is also OK, as in 'E["??"]', because of Environment's
    overloaded __getitem__."""
    ??: "() -> None"  # "frame," a nice place to hang attributes
    ??: "Environment | None"  # via Greek ????????, short name for 'enclosing'

    def _is_global(self):
        return self.?? is None

    def _is_empty(self):
        return not vars(self.??)

    def _copy(self) -> "Environment":
        """same parent ??"""
        e = Environment(lambda: None, self.??)
        for k, v in vars(self.??).items():
            setattr(e.??, k, v)
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
        r.?? = r.??._copy_trunk()  # recurse
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
        explicit ?? parameter of a procedure constructor. 'Self'
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
        while branch.??:
            branch = branch.??
        branch.?? = self  # MONKEY PATCH!
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
            while not branch.??._is_global():
                branch = branch.??
            branch.?? = None  # danger! Looks global!
            # Fix this immediately in _splice_onto_lower
        return result

    def _get_binding_val(self, var: str) -> Any:
        """Walk the sequence of Environments upward."""
        try:
            ?? = getattr(self.??, var)
        except AttributeError as _:
            if self.?? is None:
                raise NameError(
                    f'Environment: Name {var} is unbound.')
            else:  # Recurse: walk upwards.
                ?? = self.??.__getattr__(var)
        return ??

    def __getattr__(self, key: str) -> Any:
        """recursive lookup by dot notation"""
        return self._get_binding_val(key)

    def __getitem__(self, key: str) -> Any:
        """recursive lookup by bracket notation"""
        return self._get_binding_val(key)

    def __repr__(self):
        """for the debugger"""
        is_global = (self.?? is None)
        result = ("(" + hex(id(self.??))[-4:] +
                  (",????" if is_global else "") +
                  ") ") + \
                 pformat(str(list(vars(self.??).keys()))) + \
                 (">" + self.??.__repr__()
                  if not is_global
                  else "")

        return result
```

# Unique Global Environment $\Gamma\Pi$<a id="global-environment"></a>


The unique global environment is $\Gamma\Pi$, defined once for each session. The frame $\phi$, an object that ___carries___ attributes, is an empty function, namely `lambda: None`. 

```python
???? = Environment(lambda: None, None)  # ?? for "global," ?? for "environment"
```

## Example:


The example shows setting and getting the binding for a made-up variable, `???????` as an attribute on $\phi$

```python
setattr(????.??, '???????', 43)
????.???????
```

# Procedure(code{body, params}, $\pi$)<a id="procedure"></a>


From SICP again:


> __DEFINITIONS__: A ___procedure___ is a pair of _code_ and environment. 


The environment specifies the ___closure chain___ for the procedure. The closure chain is the chain of environments that contain [_free-variable bindings_](#free-variables), that is, bindings for non-parameters. 


> __DEFINITION__: A ___closure___ is a procedure along with its closure chain of environments. All variables, free and bound, may be ___resolved___, that is, looked up, in the environment chain.


In a typical scenario, a procedure will call other procedures. The other procedures may be passed in as arguments. More typically, however, the names of these other procedures are free variables. For example, in the following procedure (in ordinary Python)

```python
def complex_from_polar(r, ??):
    from math import sin, cos
    result = complex(cos(??), sin(??))
    return result
from math import pi
complex_from_polar(1.0, pi/4)
```

`sin` and `cos` are [free variables](#free-variables), and `r` and `theta` are not free. In the following, more tedious example, `math.sin` and `math.cos` are passed in as arguments bound to the parameters named `sin` and `cos`:

```python
def complex_from_polar(r, ??, sin, cos):
    result = complex(cos(??), sin(??))
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


## Example:

```python
{"body": lambda ??: ??.x + (2 * ??.y), 
 "parameters": ['x', 'y', 'z']};
```

## Confusing Terminology<a id="confusing"></a>


When speaking of the body of a procedure, variables in the parameter list, i.e., [formal parameters](#formal-parameters) are called [___bound variables___](#bound-variables); confer `x` and `y` above.


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
lambda ??: ??.x + (2 * ??.y);
```

Notice that the unused parameter `z` is nowhere to be seen. How do we know it's there? We might wait until the procedure is called with the wrong number of actual arguments, producing a run-time error. That's not great!


To make all parameters explicit, the body code is paired with a list of formal parameters.

```python
{"body": lambda ??: ??.x + (2 * ??.y), 
 "parameters": ['x', 'y', 'z']};
```

When speaking of the (formal) parameters of a procedure or the (actual) arguments in an invocation of a procedure, we do not mean $\pi$, the closure chain. Rather, we mean the variables bound in the fresh environment that [`APPLY`](#apply) chains to the front of $\pi$.


### Ambiguous Language


Sloppily, one says "a procedure of $n$ arguments" and really means "a procedure with $n$ (formal) parameters." More carefully, we may say "a procedure _call_ with $n$ (actual) arguments," or "a procedure _invocation_ with $n$ arguments," or "a procedure _application_ with $n$ arguments."


Notice we gradually elide "formal parameters" and "actual arguments" to the shorter "parameters" and "arguments," without loss of precision.


### Positional Arguments Only


For now, unlike ordinary Python, Schemulator has only positional parameters, 1-to-1 with arguments. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the call has too many or too few arguments. Since Schemulator should be semantically compatible with Scheme, it's OK to have positional parameters and arguments only. Likewise, Schemulator does not yet have Python's (admittedly wonderful) keyword arguments. 


## Anonymous versus Named


Procedures need not have a name. In ordinary Python, contrast the named procedure `foo`

```python
def foo(x):
    return x * x
foo(42)
```

against the anonymous procedure an identical body (except for the lack of `return`:

```python
(lambda x: x * x)(42)
```

By default, if the user gives a name to a Schemulator procedure, that name is bound in the unique global environment, $\Gamma\Pi$. Schemulator allows nested definitions in non-global environments, as with `def` inside `def` in ordinary Python.


## Call Notation via `__call__`


The `Procedure` class below includes a `__call__` override. To test it, we need [`APPLY`](#apply). We prototype [`APPLY`](#apply) here, just printing actual arguments. [Later, in section `APPLY`](#apply), we implement the real work of binding parameters (in a fresh environment) to arguments (evaluated in an existing environment). That implementation requires [a codependent procedure, `EVAL`](#eval), which, in-turn, needs `APPLY` (see [SICP Figure 4.1](https://sarabander.github.io/sicp/html/4_002e1.xhtml#g_t4_002e1_002e1)).


The definition of `Procedure` again illustrates codependent types. `Procedure`'s `__call__` syntax depends on `APPLY` and `APPLY` depends on `Procedure`. Python requires writing to-be-defined types in string quotes.

```python
from typing import Dict, List, Tuple, Any
Parameters = List[str]  # type synonym; positional, ordered arguments only
def APPLY(proc: "Procedure",  # <~~~ in quotes because it's not defined yet.
          args: List["Expression"] | None, 
          ??: Environment = ????) -> Any:  # defaults to global
    """forward reference; will be corrected. Needed to
    spec Procedure."""
    ECHO("APPLY.args", args)  # Just print, for now.
@dataclass
class Procedure:
    """Include __call__ override for convenient syntax."""
    code: Dict
    ??: Environment = ????  # bound in global environment by default

    def __init__(self, code, ??: Environment = ????):
        if len(set(code["parameters"])) != len(code["parameters"]):
            raise ValueError(
                f'Procedure: parameters {code["parameters"]}'
                ' must not contain duplicate symbols.')
        self.code = code
        self.?? = ??

    def __call__(self, *args):
        result = APPLY(self, args, self.??)
        return result

    def __repr__(self):
        """for the debugger"""
        result = pformat({
            '??': hex(id(self.code['body']))[-4:],
            'parms': str(self.code['parameters']),
            'env': self.?? if self.??.?? else '????'
        })
        return result
```

## Examples:<a id="procedure-examples"></a>


Following SICP 3.2.1, define `square` in the global environment and test `APPLY`.

```python
setattr(  # Bind a variable ...
    ????.??,  # ... in the frame of the global environment ...
    "square",  # ... a variable named "square" ...
    Procedure(  # ... to this Schemulator procedure.
        {"body": lambda ??: ??.x * ??.x,
         "parameters": ['x']}))  # Don't forget the parameter list!
```

Test it! Remember, `APPLY`, for now, just prints the arguments of the applied procedure!

```python
????.square(5)
????.square(5, 6)
```

Test detection of duplicate parameters:

```python
try: 
    setattr(
        ????.??,
        "square",
        Procedure(
            {"body": lambda ??: ??.x * ??.y,
             "parameters": ['x', 'x']}))
except ValueError as e:
    print(e.args)
```

## Function, Routine, Method<a id="function"></a>


> __DEFINITION__: A ___function___ is a mathematical object that associates input arguments to unique output values.


The best way to think of a function is as a simple lookup table, where a key may appear no more than once. This works even if the notional lookup table is uncountably infinite, as with a function from real numbers.


In Schemulator, a function is a special case of a procedure: a procedure without side effects. Every invocation of a function with the same arguments produces the same result.


> __DEFINITION__: ___Routine___ is a synonym for _Procedure_.


> __DEFINITION__: A ___method___ is a procedure whose first argument is an object, as with `self` in ordinary Python.


This definition of _method_ hides the entire topic of object-oriented programming, only tangentially relevant here. One only needs to know that much of Schemulator is implemented in terms of Python's classes, objects, and methods.


## Shortcut: $\Lambda$($\lambda$, params, $\pi$)


Note: The `code` attributes of procedures are an unordered dictionary of parameters and body. The $\Lambda$ shortcut puts them in an order. $\Lambda$ expressions would be more clear if the list of parameters preceded the body, but defaulting the list to `None` would not be easy and one would have to write empty brackets `[]` in every instance of $\Lambda$. Better this way.


The example procedure above has a name, "square", bound in the global environment. "Square" is not anonymous, but the [`Procedure`](#procedure) value bound to the name "square" is anonymous.


$\Lambda$ is syntactical help to shorten definitions of anonymous procedures. Its default parameter list is empty and its default environment is the unique global environment $\Gamma{}\Pi$. The shortest $\Lambda$ expression is `??(lambda ??: None)`.


Python requires function types for parameters in strings.

```python
def ??(
        body: "(??: Environment) -> Any",
        parameters=None,  # default empty
        ?? = ????            # default global
) -> Procedure:
    ?? = Procedure(
        code={"body": body, 
              "parameters": parameters or []},
        ??=??)
    return ??
```

### Example:


Procedures need closure chains for looking up free variables and for evaluating actual arguments. The next few examples have no free variables.


Test the $\Lambda$ syntax with the current `APPLY`, which just prints.

```python
setattr(  # Give a name ...
    ????.??,  # ... in the frame of the global environment ...
    "square",
    ??(lambda ??: ??.x * ??.x, ['x']))  # ... to this anonymous procedure
????.square(5)
????.square(5, 6)
```

# Application(head, args, $\pi$)<a id="application"></a>


> __DEFINITION__: An `Application` is an unevaluated data object with a [`Procedure`](#procedure) or symbol of type `str` and a list of actual arguments.


Do not confuse this [_Application_](#application), capital "A", with _application_, little "a", meaning a call or invocation of a procedure. An _Application_ with capital "A" is data representing a procedure call, invocation, or application. We test it later, after [`EVAL`](#eval) and [`APPLY`](#apply) are fully defined.


`Application` is needed in [LET_STAR](#let-star) and related constructs to delay evaluation until the environment with parameter bindings is established, where [EVAL](#eval) can look them up by name. 


`Application` is a placeholder for a more general [QUOTE](#quote) or macro mechanism, which prevents evaluation in all cases (TODO). In real Scheme, `LET` and friends are syntactical forms that delay evaluation via Scheme macros, i.e., code rewriters. We prefer explicit delaying via `Application` objects at the semantical level, so as not to obscure the algebraic structures we pursue. TODO: there may be observble differences in this model between Scheme and Schemulator if Scheme delays the evaluation of arguments, not just the application of the procedure. 


An alternative is an embedded macro DSL (Domain-Specific Language) in Python, overkill here. If we go down that road, we might as well just implement Scheme altogether.


`Application` includes a `__call__` override for Pythonic calling syntax.

```python
from typing import Union, Any
def EVAL_APPLICATION(
        expr: "Application", 
        ??: Environment = ????
) -> Any:
    """forward reference; corrected below"""
    pass
from dataclasses import (dataclass, field)
@dataclass
class Application:
    head: Union[str, Procedure]
    args: List["Expression"] = field(default_factory=list)  # args, not params!
    ??: Environment = ????
    def __call__(self, ??=None):
        return EVAL_APPLICATION(self, ?? or self.??)
    def __repr__(self):
        """for the debugger"""
        result = str({
            '??': hex(id(self))[-4:],
            'head': self.head,
            'args': self.args,
            '??': self.?? if self.??.?? else "????"
            })
        return result        
```

## Shortcut: $\Xi$(head, args, $\pi$)


Just as [Procedure](#procedure) has a [system-reserved Greek](#greek) shortcut, $\Lambda$, we make a Greek shortcut, $\Xi$, for `Application`.

```python
?? = Application
```

Test later.


# Var(sym)<a id="var"></a>


In [applications](#application), we sometimes interpret strings as symbolic references to variables in an environment. The type that manages that need is `Var`. We test `Var` [after defining `EVAL`](#eval).

```python
@dataclass
class Var:
    sym: str
```

# EVAL(expr, $\pi$)<a id="eval"></a>


`EVAL` calls `APPLY`, but `APPLY` calls `EVAL`. We can't test `EVAL` until [`APPLY` is corrected, below](#apply).


First, we correct `EVAL_APPLICATION`, [originally stubbed above](#application). The first slot of an `Application` may contain:

1. a string, [treated as a `Var` in the define-time environment](#var), that must evaluate to a procedure

2. or an explicit procedure


To evaluate an [`Application`](#application), evaluate the procedure or string (implicitly a `Var`) in the first slot, then evaluate the arguments, then [`APPLY`](#apply) the procedure.

```python
def EVAL(
        expr: Any, 
        ??: Environment = ????, 
        tag=None
) -> Any:
    """forward reference, corrected below."""
    pass
def EVAL_APPLICATION(
        expr: Application,
        ??: Environment = ????
) -> Any:
    """corrected definition; Treats 'head' a free variable in 
    an expression to recursively evaluate."""
    if isinstance(expr.head, str):  # 'head' is implicitly a Var.
        # 1/4. Evaluate first slot to find proc from string ...
        proc = ??[expr.head]
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
    proc = EVAL(proc, ??)
    # 3/4. Evaluate all args ...
    eargs = [EVAL(arg, ??) for arg in expr.args]
    # 4/4. Apply the procedure.
    ?? = APPLY(proc, eargs, ??)  # 3.3. Apply the procedure.
    return ??
```

`EVAL_PROCEDURE` splices environments for [free variables](#free-variables).

```python
def EVAL_PROCEDURE(
        ??: Procedure,
        ??: Environment = ????
) -> Procedure:
    ??.?? = ??.??._splice_onto_lower(??)
    return ??
```

Many types other than [`Application`](#application), [`Procedure`](#application),  or [`Var`](#var) evaluate to themselves. `EVAL` only needs more cases for collections. [We test that below](#test-collections) after defining [`LET_STAR`](#let-star) and [`LET`](#let).

```python
from typing import Any, Dict, Tuple, List
import numpy

Atom = Union[str, int, float, bool]
Expression = Union[
    Dict, Tuple, List, numpy.ndarray,
    Var, Application, Procedure, Atom]

def EVAL(
        expr: Expression,
        ??: Environment = ????,
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
                "env": ??})
    if isinstance(expr, Dict):
        ?? = {k: EVAL(v, ??) for k, v in expr.items()}
    elif isinstance(expr, Tuple):
        ?? = tuple((EVAL(v, ??) for v in expr))
    elif isinstance(expr, List):
        ?? = [EVAL(v, ??) for v in expr]
    elif isinstance(expr, numpy.ndarray):
        ?? = numpy.vectorize(lambda v: EVAL(v, ??))(expr)
    elif isinstance(expr, Var):
        ?? = ??[expr.sym]  # recursive lookup in Environment
    elif isinstance(expr, Application):
        ?? = EVAL_APPLICATION(expr, ??)
    elif isinstance(expr, Procedure):
        ?? = EVAL_PROCEDURE(expr, ??)
    else:
        ?? = expr
    return ??  # hang a breakpoint here
```

## Test Var Lookup


Earlier, we bound the little [Greek](#greek) system-test variable `???????` in [section "Global Environment"](#global-environment).

```python
EVAL(Var('???????'))
```

Without `VAR`, strings are literal data.

```python
type(EVAL('???????'))
```

# APPLY(proc, args, $\pi$)<a id="apply"></a>


It's easier to mentally track environment chaining if one remembers that every procedure call creates a new environment for binding [formal parameters](#formal-parameters) to [actual arguments](#actual-arguments), and that [free variables](#free-variables) are bound in the closure chain, i.e., the _define-time environment_ of the procedure.


> __MEMORIZE__: `APPLY` (1) makes a new environment parented in the closure-chain environment (default, $\Gamma\Pi$), then (2) evaluates actual arguments in the old, parent environment, then (3) binds parameters in the new environment to the values of the actual arguments.


Optimization: if a procedure has no parameters, `APPLY` doesn't make a fresh environment.


TODO: explore use cases where the environment for actual arguments given to `APPLY` is different from the define-time environment of the procedure given to `APPLY`.

```python
class IllegalArgumentsError(ValueError):
    pass

def APPLY(
        proc: Procedure,
        args: List[Expression] | None = None,  # Python doesn't like mutable [] here, ...
        ??: Environment = ????
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
        E1 = Environment(lambda: None, ??)
    else:
        E1 = ??
    # 2/3. Bind parameters in new env E1 to actual args, evaled
    #      in the old environment ...
    for k, v in zip(proc.code['parameters'], args):
        setattr(E1.??, k, EVAL(v, ??))
    # 3/3. Invoke the code body, ...
    ?? = proc.code['body'](E1)
    # ... always a lambda of an environment ??.
    return ??
```

## Examples:

```python
APPLY(????.square, [42])
```

Call `APPLY` via "Pythonic" round brackets:

```python
????.square(42)
```

Works on anonymous procedures, too:

```python
??(lambda ??: ??.x * ??.x, ['x'])(42)
```

Scheme equivalent:

<!-- #raw -->
> ((lambda (x) (* x x)) 42)
1764
<!-- #endraw -->

Test multiple parameters and arguments:

```python
??(lambda ??: ??.x * ECHO('??', ??).y, ['x', 'y'])(8, 7)
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
    ????.x
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
??foo = Application(????.square, [42])
```

Notice that the parameters are _not bound to actual arguments when the `Application` is defined_, only when the `Application` is evaluated, that is, until call-time of the procedure in the `Application`.

```python
ECHO('??foo', ??foo);
```

To perform the call, `EVAL` the `Application`.

```python
EVAL(Application(????.square, [42]))
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
EVAL_APPLICATION(Application(????.square, [42]))
```

Round-bracket _call_ notation also works, but don't pass arguments; as a procedure, and `Application` is a [_thunk_](#thunk):

```python
Application(????.square, [42])()
```

Test the [Greek](#greek) shortcut $\Xi$ for `Application`, now treating the string in the procedure slot implicitly as a `Var` looked up in $\Gamma\Pi$:

```python
??('square', [42])()
```

`Applications` require explicit `Vars` to help with actual arguments looked up in $\Gamma\Pi$:

```python
??('square',  # find proc in global env
   [Var('???????')])()  # find binding in global env
```

Without the `Var`, `EVAL` treats `'???????'` as a Python string, producing a `TypeError`:

```python
try: 
    EVAL(??('square',  # find proc in global env
           ['???????']))  # get horribly confused
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
    ??(??(lambda ??: ??.x * ??.x, ['x']), 
      [Var('???????')]  # find binding in global env
     ))
```

Scheme equivalent:

<!-- #raw -->
> (eval '((lambda (x) (* x x)) gammaoo))
1849
<!-- #endraw -->

$\Xi$ honors sub-environments even when looking up procedures and arguments by their symbolic names.

```python
??(lambda ??:  # fresh env created by implied call of APPLY
  EVAL(
      ??('square',  # Look me up in ????.
        [Var('????????????')]),  # Look me up in fresh env chained to ??.
      ECHO('??', ??)),  # pass fresh env to EVAL
  ['????????????'])(  # formal parameter of the ??; open ( implicitly calls APPLY
    42)  # actual argument of implied call of APPLY, which makes fresh env
```

$\phi\omicron\omicron\beta\alpha\rho$ is not bound in the $\pi$ of the $\Lambda$, wchich is $\Gamma\Pi$, as we see by attempting the `EVAL` in $\Gamma\Pi$:

```python
try:
    ??(lambda ??:
      EVAL(
          ??('square', 
            [Var('????????????')]), 
          ????),  # <~~~ wrong env
      ['????????????'])(
        42)
except NameError as e:
    print(e.args)
```

$\Gamma\Pi$ is the default of `EVAL`:

```python
try:
    ??(lambda ??:
      EVAL(
          ??('square', 
            [Var('????????????')])),  # <~~~ no env means ????
      ['????????????'])(
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
??(lambda ??:  # fresh env created by implied call of APPLY
    ??('square',  # Look me up in ????.
      [Var('????????????')])(  # round brackets on the ?? evaluate it ...
      ECHO('??', ??)),  # ... in this env, the fresh env of the outer call
  ['????????????'])(  # formal parameter of the ??; open ( implicitly calls APPLY
    42)  # actual argument of implied call of APPLY, which makes fresh env
```

There is no equivalent in Gambit Scheme.


# DEFINE(sym, val, $\pi$)<a id="define"></a>


Package up the "defining" boilerplate.


By default, `DEFINE` binds symbols in $\Gamma\Pi$. The "return value" is consistent with Gambit Scheme, which doesn't return anything from a `define`.

```python
def DEFINE(
        sym: str, 
        val: Any, 
        ??: Environment=????  # default
) -> None:
    """official Scheme"""
    setattr(??.??, sym, val)
    return None
```

## Examples:<a id="fancy"></a>


Do some fancy stuff:

```python
import numpy
????.square(numpy.array([[3, 4],[1, 2]]))
```

> __IMPORTANT TODO__: Notice we are not delving into numerical semantics, just piggy-backing on the Python ecosystem.

```python
DEFINE(
    'saxpy', 
    ??(lambda ??: 
      # Fancy!
      numpy.dot(??.a, ??.x) \
      if isinstance(??.a, numpy.ndarray) 
      and isinstance (??.x, numpy.ndarray) \
      # Regular
      else ??.a * ??.x + ??.y, 
      ['a', 'x', 'y']));
```

```python
import numpy
????.saxpy(numpy.array([[1, 2, 3], [4, 5, 6]]),
         numpy.array([[7], [11],[13]]),
         numpy.array([[42], [43]]))
```

or just some ordinary stuff:

```python
????.saxpy(4, 10, 2)
```

## SICP 3.2.2<a id="sicp-322"></a>

```python
DEFINE('square', 
       ??(lambda ??: ??.x * ??.x, ['x']))

DEFINE('sum_of_squares',
       ??(lambda ??: ??.square(??.x) + ??.square(??.y), ['x', 'y']))

DEFINE('f',
       ??(lambda ??: ??.sum_of_squares(1 + ??.a, 2 * ??.a), ['a']))

????.f(5)
```

Having `f` defined will bother us below. Get rid of it now.

```python
try:
    del ????.??.f
except:
    pass
```

## SICP Exercise 3.9

```python
DEFINE('factorial',
       ??(lambda ??: 1 if ??.n < 2 else \
         ??.n * ??.factorial(??.n - 1), ['n']))

????.factorial(6)
```

This next example doesn't tail-recurse because Python does not tail-recurse. We mitigate that in section [Tail Recursion](#tail-recursion).

```python
DEFINE('fact_iter',
       ??(lambda ??: ??.product if ??.counter > ??.max_count else \
         ??.fact_iter(
           ??.counter * ??.product,
           ??.counter + 1,
           ??.max_count
           ), ['product', 'counter', 'max_count']));

????.fact_iter(1, 1, 6)
```

# Procedures that Apply Procedures


Here is a procedure of two parameters, `f` and `x`, that applies `f` to `x`. In the example, bind `f` to `square`, `x` to 42, then invoke `f`.


(This example exposes a gargoyle in Python syntax regarding the positioning of call-parens.)

```python
??(lambda E1:     # Calling this ?? chains a binding environment E1 to ????.
  E1.f(ECHO('E1', E1).x),  # Apply E1.f to E1.x.
  ['f', 'x'])(   # formal parameters (open paren must be on this line)
????.square, 42)   # <~~~ Bind f to square, x to 42.
```

Scheme equivalent:

<!-- #raw -->
> ((lambda (f x) (f x)) square 42)
1764
<!-- #endraw -->

Here is a procedure that applies an internal anonymous procedure of `n` in `E2`. The outer procedure of `m` in `E1` is rooted in $\Gamma\Pi$. Read bottom-up:

```python
??(lambda E1:      # Calling this ?? chains environment E1 to ????.
  ??(lambda E2:    # Calling this ?? chains environment E2 to ????.
    E2.n * ECHO('E2', E2).n,  # <~~~ n is bound in E2;
    ['n']         #      E2 is sibling to E1; parent of E2 is ????.
   )(             # Parent of E1 is implicitly ???? (open paren must be here).
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
    ??(lambda E1:      # Calling this ?? chains environment E1 to ????.
      ??(lambda E2:    # Calling this ?? chains environment E2 to ????.
        E2.n * ECHO('E2', E2).m,  # <~~~ DIFFERENT: m is not found or bound in E2.
        ['n']         #      E2 is sibling to E1.
       )(             # Parent environment implicitly ????.
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
??(lambda E1:      # Calling it creates environment E1 in ????.
  ??(lambda E2:    # !!!! LOOK BELOW Define this ?? in E1, not in ????, the default !!!!
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
??(lambda E1:
  ??(lambda E2:
    E2.n * ECHO('E2', E2).m,
    ['n'],
    ECHO('E1', E1)),
  ['m'])(42)(42)
```

## Static Closure<a id="static-closure"></a>


Static [closures](#closures) are created at define-time. In the next example, we assign a static closure to a Python variable, `foo`, then invoke it externally.

```python
foo = ??(lambda E1:
        ??(lambda E2:  # This ?? ...
          E2.n * ECHO('E2', E2).m,
          ['n'],
          ECHO('E1', E1))  # <~~~ This ?? is defined in E1.
        (E1.m),
        ['m'])  # <~~~ This ?? is defined in ????, the default.
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
bar = ??(lambda E1:
        EVAL(
            ??(lambda E2:  # <~~~ This ?? is ...
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
??(lambda E1:        
  EVAL(        # EVAL the inner procedure itself ....
      ??(lambda E2:  
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
??(lambda E1:
  ??(lambda E2:
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
??(lambda E1:
  ??(lambda E2:
    E2.n * ECHO('E2.n', ECHO('E2', E2).n),
    ['n'], 
   ECHO('E1', E1)),
  ['n'])(43)(42)
```

As usual, there is no Scheme equivalent for explicitly chained environments.


# Procedures that Return Procedures


The outer $\Lambda$ below is the identity function: it returns its argument. It's applied to `????.square`, which is closed _only_ over the global environment, returning it. The result is applied to 42:

```python
??(lambda E1:  # Calling it creates environment E1 in ????.
  E1.f,       # Just return the value of parameter f.
  ['f'])(     # Parent environment is implicitly ????.
 ????.square)(  # <~~~ 'square' is bound in ????.
 42)          # Apply the returned procedure.
```

Return a fresh anonymous procedure:

```python
??(lambda ??: ??.f, ['f'])(             # identity function as above ...
    ??(lambda ??: ??.x * ??.x, ['x']))(  # ... applied to anonymous procedure;
42)                                  # Apply the returned procedure.
```

Scheme equivalents:

<!-- #raw -->
> (((lambda (f) f) square) 42)
1764
> (((lambda (f) f) (lambda (x) (* x x))) 42)
1764
<!-- #endraw -->

# Thunks and Currying<a id="thunk"></a>


> __DEFINITION__: A ___thunk___ is a procedure of no arguments.


> __DEFINITION__: A ___1-thunk___ is a procedure of one argument. 1-thunks are the only kind of procedure, and they're all pure functions, with no side-effects.


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
??(lambda ??: ??.x + ??.y**2, ['x', 'y'])(43, 42)
```

```python
??(lambda ??o: 
  ??(lambda ??i: ??i.x + ??i.y**2, 
    ['y'], 
    ??o),  # <~~~ explicit chaining
  ['x'])(43)(42)
```

Notice that `x` occurs [free](#free-variables) in the inner $\lambda$ but [bound](#bound-variables) in the outer $\lambda$. As always, be wary of [the confusing terminology "bound variable"](#confusing). Also notice that the curried form requires two invocations, one for each formal parameter of the two 1-thunks.


The name "curried" comes from Haskell Curry, who developed the theory of analyzing all functions as 1-thunks. Haskell Curry also gave his name to the World's most prominent pure functional programming language, Haskell.


In many examples below, we will work with curried functions, but eventually leave them behind because they're often less than intuitive and they are mathematically equivalent to non-curried forms. 


# $\Upsilon$: Squaring Square Roots of Functions


Anonymous recursive procedures are fundamental; the Ultimate Imperative requires them.


[See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md) for detailed explanations of the following development.


The running examples are factorial and Fibonacci.


Don't forget non-default `??sf` on the inner definition, lest `sf` be unbound in the inner $\lambda$. `sf` is the "square root" of the recursive function we want, square-root in an abstract algebraic sense where function application is multiplication:

```python
??(lambda ??sf:  # <~~~ Apply this ?? ...
  ??(lambda ??n: 
    # Observe the "multiplication" sf(sf):
    1 if ??n.n < 1 else ??n.n * ??n.sf(??n.sf)(??n.n - 1), 
    ['n'], ??sf),  # <~~~ Don't forget!
  ['sf'])(  # <~~~ ... to a copy of itself.
    ??(lambda ??sf:  # <~~~ bind this ?? to upper 'sf'
      ??(lambda ??n: 
        1 if ??n.n < 1 else ??n.n * ??n.sf(??n.sf)(??n.n - 1), 
        ['n'], ??sf),   # <~~~ Don't forget!
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
??(lambda ??sf:
  ??(lambda ??f:  # Domain code d is a function of business code f.
    ??(lambda ??n:  # Business code f is a function of business parameter n.
      1 if ??n.n < 1 else ??n.n * ??n.f(??n.n - 1), 
      ['n'], ??f), 
    ['f'], ??sf)(
      ??(lambda ??m:  # Delayed application of business code f = sf(sf).
        ??m.sf(??m.sf)(ECHO('??m', ??m).m), 
        ['m'], ??sf)), 
  ['sf'])(  # <~~~ Apply to copy of self:
??(lambda ??sf:
  ??(lambda ??f:  # Domain code d is a function of business code f.
    ??(lambda ??n:  # Business code f is a function of business parameter n.
      1 if ??n.n < 1 else ??n.n * ??n.f(??n.n - 1), 
      ['n'], ??f), 
    ['f'], ??sf)(
      ??(lambda ??m:  # Delayed application of business code f = sf(sf).
        ??m.sf(??m.sf)(??m.m), 
        ['m'], ??sf)), 
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
??(lambda ??d:
  ??(lambda ??sf:  # sf; copy this ?? below ...
    ??d.d(??(lambda ??m: ??m.sf(??m.sf)(ECHO('??m', ??m).m),
          ['m'], ??sf)),
    ['sf'], ??d)(  # <~~~ "squaring," i.e., self-application
      ??(lambda ??sf:  # sf; .... right here
        ??d.d(??(lambda ??m: ??m.sf(??m.sf)(??m.m), 
              ['m'], ??sf)),
        ['sf'], ??d)), 
  ['d'])(  # d is the formal parameter for domain code
    ??(lambda ??f:  # Domain code d is a function of business code f.
      ??(lambda ??n:  # Business code f is a function of business parameter n.
        1 if ??n.n < 1 else ??n.n * ??n.f(??n.n - 1), 
        ['n'], ??f),  # n: business parameter
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
??(lambda ??d:  # function of domain code, d
  ??(lambda ??g:  # generic squaring gizmo 
    ??g.g(??g.g), ['g'], ??d)(
      ??(lambda ??sf: 
        ??d.d(??(lambda ??m: ??m.sf(??m.sf)(??m.m), 
               ['m'], ??sf)),
        ['sf'], ??d)), 
  ['d'])(  # formal parameter d for domain code
    ??(lambda ??f:  # Domain code d is a function of business code f.
      ??(lambda ??n:  # Business code f is a function of business parameter n.
        1 if ??n.n < 1 else ??n.n * ??n.f(??n.n - 1), # business code
        ['n'], ??f),  # n: business parameter
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


The glyph that looks like "Y" is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). Names in user code should not collide with it if users remember to [avoid Greek](#greek).

```python
DEFINE('??1', 
       ??(lambda ??d: # ??1 is function of domain code, d.
         # d is a function of business code f, that 
         # returns new business code that can refer to f. 
         ??(lambda ??g: ??g.g(??g.g), ['g'], ??d)(  # ??(g) applied to ...
             ??(lambda ??sf:  # generic square root.
               ??d.d(??(lambda ??m: ??m.sf(??m.sf)(??m.m), 
                      ['m'], ??sf)),
               ['sf'], ??d)), 
         ['d']))
```

### Fact-Recursive


Write a domain code that returns a business code of recursive, but externally anonymous, `f`:

```python
DEFINE('fact_recursive',
      ??(lambda ??: # Domain code is a function of business code, f ...
        ??(lambda ??: # ... that returns business code that can call f.
          1 if ??.n < 1 else ??.n * ??.f(??.n - 1), # business code
          ['n'], ??), # 1 business parameter, n
        ['f'])) # recursive function

????.??1(????.fact_recursive)(6)
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
# ?? d: (?? g: g[g])(?? sf: d[?? m, c, x: sf[sf][m, c, x]]) 
DEFINE('??3', 
       ??(lambda ??d:  # of d, the domain code ...
         ??(lambda ??g: ??g.g(??g.g), ['g'], ??d)(
             # ... of business code of three parameters
             ??(lambda ??sf: ??d.d(  # domain code
                 ??(lambda ??: 
                   ??.sf(??.sf)(??.??, ??.??, ??.??),  # business code
                   ['??', '??', '??'], ??sf)),  # business parameters
               ['sf'], ??d)), 
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
# ?? f: ?? m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter', # domain code is a function of f ...
       ??(lambda ??: # ... which is business code.
         ??(lambda ??: 
           ??.m 
           if ??.c > ??.x 
           else ??.f(??.m * ??.c, ??.c + 1, ??.x), # business code
           ['m', 'c', 'x'], ??), # business parameters
         ['f']));

????.??3(????.fact_iter)(1, 1, 6)
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

# Tail Recursion<a id="tail-recursion"></a>


> __OBSERVATION__: The Ultimate Imperative requires anonymous _tail-recursive_ procedures.


Thanks to [Thomas Baruchel for the idea of Exceptions to implement tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion).


If users are aware that their domain code is tail-recursive, then they may call it via `LOOP` instead of via $\Upsilon$.


Scheme detects tail recursion automatically. It parses user code and marks un-nested tail calls. Schemulator doesn't parse, [on purpose](#semantics), and [Python infamously avoids tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). In Python and Schemulator, users must invoke tail recursion explicitly. This isn't terrible. Tail-calls are lexically obvious, so users should always know. In Clojure, there is precedent: users explicitly write `loop` and `recur`. In any event, tail-recursive domain code can always be called via non-tail-recursive $\Upsilon$, it just won't be as fast nor as resistent to stack overflow.


## `LOOP3`


`LOOP3` has the same signature as $\Upsilon3$; it takes domain code of business code of three arguments as its sole argument.


The glyph that looks like "P" below is Greek Capital Rho for "recur." Names in user code will not collide with P if users remember to [avoid Greek](#greek). As with $\Upsilon$, Rho and `LOOP` must know their argument counts. That's fixed below with `LOOPN`.


`LOOP3` receives domain code, which receives recursive business code `f` of three business parameters. In the base case of its recursion, `f` just returns a result from the business parameters, else `f` calls `f` recursively. `LOOP3` calls the domain code with `f = P3`, which recurses by raising an exception that overwrites arguments with new values. `LOOP3` loops until that exception is not raised.


> __WARNING__: ***Results are undefined if any `LOOP` function is called with non-tail-recursive domain code.***

```python
class TailCall(Exception):  
    """???????????????????? ??????????"""
    def __init__(self, *args):
        """Overwrite old args with new."""
        self.args = args

def RECUR(*args):  
    """??????????: in sincere flattery of Clojure"""
    raise TailCall(*args)

def LOOP3(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    # in the global environment, ????,
    DEFINE('??3', ??(lambda ??: RECUR(??.m, ??.c, ??.x), ['m', 'c', 'x']));
    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try: 
                return d(????.??3)(*args)
            except TailCall as e:
                args = e.args
    ?? = ??(lambda ??: looper(??.m, ??.c, ??.x), ['m', 'c', 'x'], ??=d.??)
    return ??
```

Example:

```python
LOOP3(????.fact_iter)(1, 1, 6)
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
    print(????.??3(????.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

Call via `LOOP3` does not. Notice the domain code `fact_iter` is EXACTLY the same as in the recursive call above.

```python
try:
    print(LOOP3(????.fact_iter)(1, 1, 400))
except RecursionError as e:
    print(e.args)
```

## Tail-Recursive Fibonacci<a id="tail-recursive-fibonacci"></a>


Write domain code for catastropically slow, non-tail-recursive, exponentially diverging Fibonacci:

```python
DEFINE('fib_slow', 
       ??(lambda ??: 
         ??(lambda ??: 1 if ??.n < 2 else 
           ??.f(??.n - 1) + ??.f(??.n - 2), ['n'], ??),
         ['f']))

????.??1(????.fib_slow)(6)
```

This is miserable even for $n=23$. Don't call it for bigger arguments.

```python
????.??1(????.fib_slow)(23)
```

The following takes 10 seconds. Uncomment if you want to see the time per iteration: about 1,000 ms; YES, a full second!

```python
# timeit(????.??1(????.fib_slow)(23))
```

Without linearization, Fibonacci 500 would not complete in $10^{30}$ times the Age of the Universe. One way to linearize is tail recursion. Another way is [memoization](#memoization) (_sic:_ not _memorization_).


Tail-recursive memoization is possible but not necessary. A tail-recursive Fibonacci easy and blazingly fast:

```python
DEFINE('fib_iter',
       ??(lambda ??:
         ??(lambda ??: ??.b if ??.n < 1 else 
           ??.f(??.b, ??.a + ??.b, ??.n - 1),
           ['a', 'b', 'n'], ??),
         ['f']));
```

Check it:

```python
LOOP3(????.fib_iter)(0, 1, 23)
```

Time it. The following takes 10 seconds. Uncomment if you want see 250 _micro_ seconds, or so, 4000 times faster on this argument, 23. Exponentially faster on bigger arguments.

```python
# timeit(LOOP3(????.fib_iter)(0, 1, 23))
```

Stress it, remembering that the non-tail-recursive version would not complete in astronomical time:

```python
LOOP3(????.fib_iter)(0, 1, 500)
```

# Memoized [sic] Fibonacci<a id="memoization"></a>


We do not bother with Scheme equivalents for code in this chapter. Such would be lengthy and not instructive.


Linearize execution of Fibonacci by recording intermediate results in a memo table instead of recomputing them. This is an easy instance of [_Dynamic Programming_](https://en.wikipedia.org/wiki/Dynamic_programming).


## Curried Memo Table


One way to pass a memo table is through a Curried second argument. We'll need $\Upsilon2C$, generic for 2-parameter, Curried business code:

```python
DEFINE('??2C', 
       ??(lambda ??: # function of domain code, d ...
         ??(lambda ??: ??.g(??.g), ['g'], ??)(
             # with business code of 2 parameters, curried
             ??(lambda ??: 
               ??.d(??(lambda ??: 
                     ??(lambda ??: ??.sf(??.sf)(??.m)(??.n), 
                       ['n'], ??), ['m'], ??)),
               ['sf'], ??)), 
         ['d']));
```

Notice that we eased our burden of writing this $\Upsilon$ by not bothering to distinguish the environment parameters $\pi$ by name. Ordinary scoping rules disambiguate them, affording us a more fluid and concise style.


The domain code for a memoized, Curried Fibonacci follows. The parameter `a` is the _accumulator_, _associator_, or memo table, whatever word you like best. This is easiest to read (and to write) from the bottom up. It looks horrendous, but it isn't really. We disambiguated the names of the environment parameters as a guide. We won't do so in the future. In the `ECHO` output, count the 13 levels of chained environments.

```python
DEFINE('fib_fast',
       ??(lambda ??f: # of f; level 1
         ??(lambda ??a: # of a; level 2
           ??(lambda ??n: # of n; level 3
             (??n.a, 1) if ??n.n < 2 else
             ??(lambda ??n1: # of n_1; level 4
               (??n1.a, ??n1.a[??n1.n_1]) # optimizer should remove these two lines
               if ??n1.n_1 in ??n1.a else # ^^^
               ??(lambda ??fim1: # of fim1; level 5
                 ??(lambda ??m1: # of m1; level 6
                   ??(lambda ??r1: # of r1; level 7
                     ??(lambda ??a1: # of a1; level 8
                       ??(lambda ??n2: # of n_2; level 9
                         (??n2.a1, ??n2.r1 + ??n2.a1[??n2.n_2]) # <~~~ a quick exit
                         if ??n2.n_2 in ??n2.a1 else 
                         ??(lambda ??fim2: # of fim2; level 10
                           ??(lambda ??m2: # of m2; level 11
                             ??(lambda ??r2: # of r2; level 12
                               ??(lambda ??a2: # of a2; level 13
                                 (ECHO('??a2', ??a2).a2, ??a2.r1 + ??a2.r2), # <~~~ the money line
                                 ['a2'], ??r2)(??r2.m2[0] | {??r2.n_2: ??r2.r2}),  # <~~~ update memo
                               ['r2'], ??m2)(??m2.m2[1]), # unpack
                             ['m2'], ??fim2)(??fim2.fim2(??fim2.n_2)), # unpack
                           ['fim2'], ??n2)(??n2.f(??n2.a1)), # <~~~ recurse
                         ['n_2'], ??a1)(??a1.n - 2), # DRY
                       ['a1'], ??r1)(??r1.m1[0] | {??r1.n_1: ??r1.r1}), # <~~~ update memo
                     ['r1'], ??m1)(??m1.m1[1]), # unpack
                   ['m1'], ??fim1)(??fim1.fim1(??fim1.n_1)), # unpack
                 ['fim1'], ??n1)(??n1.f(??n1.a)), # <~~~ recurse
               ['n_1'], ??n)(??n.n - 1), # DRY
             ['n'], ??a), # business parameter
           ['a'], ??f), # curried memo
         ['f'])) # domain code 
????.??2C(????.fib_fast)({})(23)[1]
```

It's about 1 millisecond per iteration, 1,000 times faster than the original. The following takes 10 seconds. Uncomment if you want proof.

```python
# timeit(????.??2C(????.fib_fast)({})(23)[1])
```

Still blows the recursion limit:

```python
try:
    print(????.??2C(????.fib_fast)({})(200)[1])
except RecursionError as e:
    print(e.args)
```

We fix that immediately below.


## Memo Table as Business Parameter


Before doing tail-recursion with a memo table, show the memo as un-Curried. Currying is useful in general, but complicates $\Upsilon$. Get rid of it. Notice, also, that we no longer bother to disambiguate the environment parameters, letting Nature take care of it.

```python
DEFINE('fib_fast_uncurried',
      ??(lambda ??: # of f; level 1
        ??(lambda ??: # of a, n; level 2
          (??.a, 1) if ??.n < 2 else
          ??(lambda ??: # of n_1; level 3
            ??(lambda ??: # of t1; level 4
              ??(lambda ??: # of m1; level 5
                ??(lambda ??: # of r1; level 6
                  ??(lambda ??: # of a1; level 7
                    ??(lambda ??: # of n_2; level 8
                      (??.a1, ??.r1 + ??.a1[??.n_2]) # <~~~ quick exit
                      if ??.n_2 in ??.a1 else 
                      ??(lambda ??: # of t_2; level 9
                        ??(lambda ??: # of m_2; level 10
                          ??(lambda ??: # of r_2; level 11
                            ??(lambda ??: # of a_2; level 12
                              (??.a2, ??.r1 + ??.r2), # <~~~ the money line
                              ['a2'], ??)(??.m2 | {??.n_2: ??.r2}), # <~~~ update memo
                            ['r2'], ??)(??.t2[1]), # nupaci
                          ['m2'], ??)(??.t2[0]), # unpack
                        ['t2'], ??)(??.f(??.a1, ??.n_2)), # <~~~ recurse
                      ['n_2'], ??)(??.n - 2), # dry
                    ['a1'], ??)(??.m1 | {??.n_1: ??.r1}), # <~~~ update memo
                  ['r1'], ??)(??.t1[1]), # unpac
                ['m1'], ??)(??.t1[0]), # unpack
              ['t1'], ??)(??.f(??.a, ??.n_1)), # <~~~ recurse
            ['n_1'], ??)(??.n - 1), # DRY
          ['a', 'n'], ??), # busines parameters
        ['f'])); # domain-code signature
```

Now need only an ordinary $\Upsilon2$ to call it:

```python
DEFINE('??2', 
       ??(lambda ??: # of d, the domain code ...
         ??(lambda ??: ??.g(??.g), ['g'], ??)(
             # of business code of two parameters
             ??(lambda ??: 
               ??.d(??(lambda ??: ??.sf(??.sf)(??.m, ??.c), 
                     ['m', 'c'], ??)), 
               ['sf'], ??)), 
         ['d']));
```

The recursion limit is a little higher, but we don't want any of that.

```python
try:
    print(????.??2(????.fib_fast_uncurried)({}, 200)[1])
except RecursionError as e:
    print(e.args)
```

```python
try:
    print(????.??2(????.fib_fast_uncurried)({}, 250)[1])
except RecursionError as e:
    print(e.args)
```

This code is not tail-recursive, because it sums the results of two tail-recursive calls. An attempt to call it through a specialization of `LOOP` produces incorrect results, as warned above:

```python
def LOOP2(d: Procedure) -> Procedure:  # domain code
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    nyms = ['a', '??']
    # in the global environment, ????,
    DEFINE('??2',
           ??(lambda ??:
             RECUR(*[??[nym] for nym in nyms]),
             nyms))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(????.??2)(*args)
            except TailCall as e:
                args = e.args

    ?? = ??(lambda ??:
          looper(*[??[nym] for nym in nyms]),
          nyms,
          ??=d.??)

    return ??
```

```python
try:
    print(LOOP2(????.fib_fast_uncurried)({}, 250))
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
DEFINE('??5', 
       ??(lambda ??: # of d, the domain code ...
         ??(lambda ??: ??.g(??.g), ['g'], ??)(
             # of business code of five parameters
             ??(lambda ??: ??.d(
                 ??(lambda ??: ??.sf(??.sf)(??.m, ??.c, ??.x, ??.a, ??.b), 
                   ['m', 'c', 'x', 'a', 'b'], ??)), 
               ['sf'], ??)), 
         ['d']));
```

```python
DEFINE('fib_tc_memo',
      ??(lambda ??: 
        ??(lambda ??:
          (??.a | {??.x: ??.r2}, ??.r2) if ??.n < 1 else \
          ??.f(??.r2, ??.r1 + ??.r2, 
              ??.a | {??.x-??.n: ??.r2},
              ??.n - 1,
              ??.x),
         ['r1', 'r2', 'a', 'n', 'x'], ??), 
        ['f']));
```

```python
????.??5(????.fib_tc_memo)(0, 1, {}, 23, 23)
```

<!-- #region tags=[] -->
### Tail-Recusive
<!-- #endregion -->

```python
def LOOP5(d: Procedure) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    DEFINE('??5', ??(lambda ??: 
                   RECUR(??.m, ??.c, ??.x, ??.a, ??.b), 
                   ['m', 'c', 'x', 'a', 'b']));
    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try: 
                return d(????.??5)(*args)
            except TailCall as e:
                args = e.args
    ?? = ??(lambda ??: 
               looper(??.m, ??.c, ??.x, ??.a, ??.b), 
               ['m', 'c', 'x', 'a', 'b'], ??=d.??)
    return ??
```

```python
LOOP5(????.fib_tc_memo)(0, 1, {}, 23, 23)
```

### Test the Limits

```python
try:
    print(????.??5(????.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)    
```

```python
try:
    print(LOOP5(????.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)
```

# `LOOP` and $\Upsilon$ of $N$ Business Parameters


We finish off [PART 1: BASICS](#basics) with straightforward generalizations of `LOOP` and $\Upsilon$ to any number of business parameters, without undue explanation. We test them extensively below in [the section on `DO`](#do).

```python
def LOOPN(
        d: Procedure, 
        vars_: List[str]  # <~~~ NOTA BENE, extra parameters to LOOPN
) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    DEFINE('??N',
           ??(lambda ??:
             RECUR(*[??[var] for var in vars_]),
             vars_))

    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try:
                return d(????.??N)(*args)
            except TailCall as e:
                args = e.args

    ?? = ??(lambda ??:
          looper(*[??[var] for var in vars_]),
          vars_,
          ??=d.??)

    return ??

LOOPN(????.fact_iter, ['m', 'c', 'x'])(1, 1, 6)
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
DEFINE('??N',
       ??(lambda ??d:  # of d, the domain code and vars ...
         ??(lambda ??g: ??g.g(??g.g), ['g'], ??d)(
             # of business code of N parameters
             ??(lambda ??sf:
               ??d.d(??(lambda ??vs:
                      ??vs.sf(??vs.sf)(
                          *[??vs[var] for var in ??vs.vars_]),
                      ??sf.vars_,  # <~~~ list of parameters
                      ??sf)),
               ['sf'], ??d)),
         ['d', 'vars_']));  # <~~~ extra parameters

????.??N(????.fact_iter, ['m', 'c', 'x'])(1, 1, 6)
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
        ??: Environment = ????
) -> None:
    ee = EVAL(val, ??)
    """recursive lookup"""
    while ?? is not None:
        # Find the right ??; ...
        try:
            getattr(??.??, sym)  # ... don't recurse via ??[sym]
            break
        except AttributeError as _:
            if ??.?? is None:
                raise NameError(f'Set!: Name {sym} is unbound.')
            else:  # recurse
                ?? = ??.??
    setattr(??.??, sym, ee)
    return None  # following Gambit Scheme
```

# BLOCK / BEGIN<a id="block"></a>


In the following implementation, every $\lambda$ must be a [_thunk_](#thunk): a procedure of no arguments (our $\lambda$s always have the conventional parameter $\pi$; our thunks do not use any parameters bound in $\pi$). All but the last thunk are for side-effect; all thunks are evaluated; all but the last value are discarded.


Steele's paper calls this form `BLOCK`. Scheme calls it `BEGIN`. Common Lisp calls it `PROGN`.

```python
def BLOCK(
        *thunks: "Procedure | Procedure",  # <~~~ PEP 438 type notation
        ??: Environment = ????
) -> Any:
    ?? = None
    for thunk in thunks:
        ?? = APPLY(thunk, [], ??=??)  # <~~~ thunks take no args
    return ??
```

## Examples:


### Set-and-Test


This block first sets `x` in $\Gamma\Pi$ to 6 and then accesses that variable:

```python
DEFINE('x', 0)  # <~~~ Binding must preexist in ???? for SET_BANG
BLOCK(
    ??(lambda ??: SET_BANG('x', 6, ??)), 
    ??(lambda ??: ??.x * 7))
```

Python equivalent (using [Greek](#greek) to avoid collisions with user names). Python $\lambda$s can't have sequences of "statements." We packages sequences of statements in `def`s with made-up names:


As a general habit, the return value of any Python equivalent is called $\rho$. We imclude it specifically so that t a compiler can notice the beginning and the end of a construction. It's also a convenient place to hang a debugger breakpoint.

```python
x = 0
def ??000():
    global x
    x = 6  # <~~~ ?? thunks in BLOCK are automatically called
    ?? = x * 7
    return ??
??000()
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
DEFINE('y', 42)  # <~~~ in ????
BLOCK(
    ??(lambda ??: SET_BANG('x', 6, ??)), 
    ??(lambda ??: SET_BANG('x', ??.x * 7, ??)),
    ??(lambda ??: ??.x * ??.y))
```

Python equivalent

```python
y = 42
def ??001():
    global x, y
    x = 6
    x = x * 7
    ?? = x * y
    return ??
??001()
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
        ??(lambda ??: SET_BANG('x', 6, ??)), 
        ??(lambda ??: SET_BANG('x', ??.x * 7, ??)),
        ??(lambda ??: print({'expect 1764': ??.x * ??.y})),
        ??(lambda ??: ??.z))  # <~~~ no binding
except NameError as e:
    print({'from Schemulator': e.args})
```

Python equivalent:

```python
def ??002():
    global x, y, z
    x = 6
    x = x * 7
    print({'expect 1764': x * y})
    ?? = z
    return ??
try:
    ??002()
except Exception as e:
    print({'from Python': e.args})
```

Test BLOCK in a non-global environment $\pi$:

```python
??(lambda ??0:  # <~~~ Make a non-global ??.
  print(
  BLOCK(
      ??(lambda ??: SET_BANG('x1', 7, ECHO('??', ??)), ??=??0),
      ??(lambda ??: SET_BANG('y1', 6, ??), ??=??0),
      ??(lambda ??: ??.x1 * ??.y1, ??=??0),
      ??=ECHO('??0', ??0)
  )),
  ['x1', 'y1'])(0, 0)
```

Python equivalent; the only way to combine local variables with squencing is via `def`:

```python
def ??003():
    x1 = 0
    y1 = 0
    def ??004():
        nonlocal x1, y1
        x1 = 7
        y1 = 6
        ?? = x1 * y1
        return ??
    ?? = ??004()
    return ??
??003()
```

BLOCK automatically propagates the environments, so the $\pi_0$ arguments to the individual $\lambda$s are overkill:

```python
??(lambda ??0:  # <~~~ Make a non-global ??.
  print(
  BLOCK(
      ??(lambda ??: SET_BANG('x1', 7, ECHO('??', ??))),  # <~~~ overkill
      ??(lambda ??: SET_BANG('y1', 6, ??)),  # <~~~ overkill
      ??(lambda ??: ??.x1 * ??.y1),           # <~~~ overkill
      ??=ECHO('??0', ??0)  # <~~~ necessary
  )),
  ['x1', 'y1'])(0, 0)
```

Names `x1` and `y1` are not bound in the global environment, even though they were in the non-global environment. The following example proves no "binding leakage."

```python
try:
    ????.x1
except NameError as e:
    print(e.args)
    
try:
    ????.y1
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
    ??(lambda ??: SET_BANG('x', 6, ??)), 
    ??(lambda ??: SET_BANG('x', ??.x * 7, ??)),
    ??(lambda ??: ??.x * ??.y),
    ??(lambda ??: ??.x * ??.y))
```

Check nested `BLOCK`s. Don't forget to wrap the nested `BLOCK` in a [thunk](#thunk)!

```python
BLOCK(
    ??(lambda ??: SET_BANG('x', 6, ECHO('??', ??))), 
    ??(lambda ??:  # <~~~ Don't forget to wrap it!
      BLOCK(??(lambda ??: SET_BANG('x', ??.x * 7, ??)),
            ??(lambda ??: ??.x * ??.y))))
```

# Clear the Global Environment


Get rid of `x` and `y` and other gaseous bindings lest they cause trouble below. We shouldn't just make a new global environment because some system procedures, like [`DEFINE`](#define), are closed over the old one and we'd just have to specify `????` everywhere explicitly rather than using the convenient defaults.

```python
del ????.??.???????
del ????.??.saxpy
del ????.??.sum_of_squares
del ????.??.factorial
del ????.??.fact_iter
del ????.??.fact_recursive
del ????.??.fib_slow
del ????.??.fib_iter
del ????.??.fib_fast
del ????.??.fib_fast_uncurried
del ????.??.fib_tc_memo
del ????.??.x
del ????.??.y
????
```

`PN` does not appear because we have not yet called `LOOPN`.


Define the Scheme-like synonym `BEGIN` for `BLOCK`:

```python
BEGIN = BLOCK
```

# LET*, LET, LETREC<a id="the-lets"></a>


`LET_STAR` is sequential binding of locals, syntactically like assignment, but purely with $\lambda$ expressions. Later bindings may depend on earlier ones. `LET` is parallel binding, where bindings are independent and unordered, but can't depend on one another. `LETREC` is mutually recursive `LET`, where any bindings may depend on any and all other bindings.


We face a design choice: should the binding pairs be a list of tuples, as in Scheme, or a flat list of pairs, as in Clojure? The former introduces a level of collections, the latter simplifies implementation via slicinng, but introduces more checks for types and for element counts. We choose the Scheme-like option because the Schemulator is supposed to be like Scheme on the surface unless there is a profound reason not to be.


Remember that $\Xi$ is a [Greek](#greek) shortcut for [`Application`](#application). Body of any of the _Lets_ must be an `Application` so that the environment $\pi$ can be propagated at the point where it is known.


> __WARNING__: ***Results of `LET*`, `LET`, and `LETREC` are undefined if the body is not an [`Application`](#application) or $\Xi$.***


## LET_STAR<a id="let-star"></a>

```python
def LET_STAR(
        binding_pairs: List[Tuple[str, Expression]],
        body: Expression, 
        ??: Environment = ????
) -> Any:
    if len(binding_pairs) == 0:  # <~~~ Empty bindings are allowed.
        ?? = EVAL(body, ??)
        return ??
    key, val = binding_pairs[0]
    E1 = Environment(lambda: None, ??)
    setattr(E1.??, key, EVAL(val, ??))
    if len(binding_pairs) == 1:
        return EVAL(body, E1)
    else:
        return LET_STAR(binding_pairs[1:], body, E1)
```

### Examples:


Test depth 0, no bindings. The final expression -- the returned value -- can be a `Procedure`:

```python
LET_STAR([], 
         ??(lambda ??: 43 * ??.x, ['x']))
```

Python equivalent:

```python
lambda x: 43 * x
```

Which can be called:

```python
LET_STAR([], 
         ??(lambda ??: 43 * ??.x, ['x']))(42)
```

Python equivalent:

```python
(lambda x: 43 * x)(42)
```

Applications are evaluated in place:

```python
LET_STAR([], 
         ??(??(lambda ??: 43 * 42)))
```

Python equivalent:

```python
(lambda: 43 * 42)()
```

The `LET_STAR` chains bindings in environments and evaluates applications in the chain. Procedures in applications enjoy free variables from any environments in the chain.

```python
LET_STAR([('z', 42)],
        ??(??(lambda ??: ECHO('??', ??).z * ??.z)))
```

Python does not have `LET`. Local variables must be defined in `def`s with made-up names (we use [Greek](#greek) to avoid collision with user names. Equivalent to the above:

```python
def ??004():
    z = 42  # <~~~ Local variables must be args or defined in defs.
    ?? = (lambda: z * z)()  # <~~~ Evaluating ?? calls its ??.
    return ??
??004()
```

Alternative:

```python
def ??005(z):  # <~~~ Local vars can be args.
    ?? = (lambda: z * z)()  # <~~~ ?? call
    return ??
??005(42)
```

Later bindings can acces and can shadow earlier ones:

```python
LET_STAR([('z', 42),
          ('z', ??(??(lambda ??: ??.z + 1)))],
        ??(??(lambda ??: ECHO('??', ??).z * ??.z)))
```

Python equivalent:

```python
def ??006():
    z = 42
    def ??007():  # <~~~ Cust have a def; can't sequence 'nonlocal' stmt in a ??
        nonlocal z  # <~~~ necessary; refers to outer z.
        z = (lambda: z + 1)()  # <~~~ ?? call
        return z
    z = ??007()  # <~~~ Old z is shadowed.
    ?? = (lambda: z * z)()  # <~~~ ?? call
    return ??
??006()
```

Returned `Procedure` is a closure over local variables:

```python
LET_STAR([('z', 42)],
        ??(lambda ??: ??.z * ??.z))
```

Python equivalent:

```python
def ??008():
    z = 42
    ?? = lambda: z * z
    return ??
??008()
```

As always, the returned procedure may be called externally:

```python
LET_STAR([('z', 42)],
        ??(lambda ??: ??.z * ??.z))()
```

Python equivalent:

```python
def ??009():
    z = 42
    ?? = lambda: z * z
    return ??
??009()()
```

Test depth 1 (don't forget the `Var` in $\Xi$:

```python
LET_STAR([('z', 42)], 
         ??(????.square, [Var('z')]))
```

Python equivalent; `Var('z')` is like `eval('z')`:

```python
def square(x):
    ?? = x * x
    return ??
def ??010():
    z = 42
    ?? = square(eval('z')) 
    return ??
??010()
```

### Free Variables<a id="free-variables"></a>


> __DEFINITION__: A ___free variable___ in the body of a lambda is a variable _NOT_ in the parameter list.


`z`, `y`, and `y` are free in the $\lambda$s below:


Test depth 2 with free variables:

```python
LET_STAR([('z', 42), 
          ('y', 43)], 
         ??(??(lambda ??: ??.z * ??.y)))
```

Python equivalent:

```python
def ??011():  # <~~~ Environment here will have all bindings.
    z = 42
    y = None  # <~~~ inserted in second pass after 'y' is discovered
    def ??012():
        nonlocal y, z
        y = 43
    ??012()
    ?? = z * y
    return ??
??011()
```

Inspect bindings with an `ECHO` around the environment variable:

```python
LET_STAR([('z', 42), 
          ('y', 43)], 
         ??(??(lambda ??: ECHO('??', ??).z * ??.y)))
```

Test depth 3 with free varialbes:

```python
LET_STAR([('z', 42), 
          ('y', ??(??(lambda ??: ??.z + 1))), 
          ('w', ??(??(lambda ??: ??.z * ??.y)))],
         body=??(??(lambda ??: ECHO('??', ??).w)))
```

Python equivalent:

```python
def ??013():
    z = 42
    y = None
    w = None
    def ??014():
        nonlocal y, z  # <~~~ Build up the nonlocals.
        y = z + 1
        def ??015():
            nonlocal w, y, z  # <~~~ Build up the nonlocals.
            w = z * y
        ??015()
    ??014()
    ?? = w
    return ??
??013()
```

### Bound Variables<a id="bound-variables"></a>


> __DEFINITION__: A ___bound variable___ in the body of a $\lambda$ is a variable that appears in the parameter list.


Remember that [this definition of bound variables is confusing](#confusing).


### Closed Term<a id="closed-term"></a>


> __DEFINITION__: A ___closed procedure___ or ___closed term___ is a procedure with no free variables (see [this web page](https://web.mat.bham.ac.uk/R.W.Kaye/logic/freevar.html)).


Test depth 3 with bound variables. To access earlier bindings, write an [`Application` or $\Xi$](#application) that, when evaluated, [accesses earlier bindings as `Var`s](#var). Notice the bound vars shadow free vars with the same names.

```python
LET_STAR([('z', 42), 
          ('y', ??(??(lambda ??: ??.z + 1, ['z']),  # <~~~ z shadows ...
                  [Var('z')])),  # <~~~ ... this Var.
          # Shadowing occurs here, too.
          ('w', ??(??(lambda ??: ECHO('??', ??).z * ??.y, ['z', 'y']), 
                  [Var('z'), Var('y')])
          )], 
         body=??(??(lambda ??0: ECHO('??0', ??0).w)))
```

Python equivalent:

```python
def ??016():
    z = 42
    y = None
    w = None
    def ??017():
        nonlocal y, z
        y = (lambda z: z+1)(eval('z'))
        def ??018():
            nonlocal w, y, z
            w = (lambda z, y: z * y)(eval('z'), eval('y'))
        ??018()
    ??017()
    ?? = w
    return ??
??016()
```

The names of the bound variables do not matter. In this case, we avoid shadowing, so the names `z`, `y`, and `w` are available in the body of `w`'s procedure:

```python
LET_STAR([('z', 42), 
          ('y', ??(??(lambda ??: ??.zz + 1, ['zz']),
                  [Var('z')])), 
          # Use parameters:
          ('w', ??(??(lambda ??: ECHO('??', ??).zzz * ??.yy, ['zzz', 'yy']), 
                  [Var('z'), Var('y')])
          )], 
         body=??(??(lambda ??: ??.w)))
```

Python equivalent:

```python
def ??019():
    z = 42
    y = None
    w = None
    def ??020():
        nonlocal y, z
        y = (lambda zz: zz+1)(eval('z'))
        def ??021():
            nonlocal w, y, z
            w = (lambda zzz, yy: zzz * yy)(eval('z'), eval('y'))
        ??021()
    ??020()
    ?? = w
    return ??
??019()
```

Ignore parameters; use free variables:

```python
LET_STAR([('z', 42), 
          ('y', ??(??(lambda ??: ??.zz + 1, ['zz']),
                  [Var('z')])), 
          # Ignore parameters; use free variables:
          ('w', ??(??(lambda ??: ECHO('??', ??).z * ??.y, ['zzz', 'yy']), 
                  [Var('z'), Var('y')])
          )], 
         body=??(??(lambda ??: ??.w)))
```

Python equivalent:

```python
def ??019():
    z = 42
    y = None
    w = None
    def ??020():
        nonlocal y, z
        y = (lambda zz: zz+1)(eval('z'))
        # Ignore parameters; use free variables:
        def ??021():
            nonlocal w, y, z
            w = (lambda zzz, yy: z * y)(eval('z'), eval('y'))
        ??021()
    ??020()
    ?? = w
    return ??
??019()
```

### Closures<a id="closures"></a>


The body of this next example evaluates `g` without returning a closure:

```python
LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
        body=??(??(lambda ??: ECHO('??', ??).g(42))))
```

Python equivalent:

```python
def ??022():
    g = lambda x: x * x
    ?? = g
    return ??
??022()(42)
```

But, we may return a closure, meaning that its environment chain is still alive. We may invoke it outside the `LET_STAR`.The application $\Xi$ pulls the closure out of the environment created by `LET_STAR`.

```python
?? = LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
             body=??(??(lambda ??: ECHO('??', ??).g)))
??(42)
```

Python equivalent:

```python
def ??023():
    g = lambda x: x * x
    ?? = g
    return ??
?? = ??023()
??(42)
```

Without he application `??`, the value is an unevaluated procedure returning a closure:

```python
LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
        body=??(lambda ??: ??.g))
```

We can evaluate the returned closure with an extra, explicit call:

```python
foo = LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
               body=??(lambda ??: ??.g))
foo()(42)
```

The Python equivalents were demonstrated above.


Ensure no leakage:

```python
try:
    ????.g
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
LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
         body=??(??(lambda ??: ??.gg(42), ['gg']), [Var('g')]))
```

Python equivalent:

```python
def ??024():
    g = lambda x: x * x
    ?? = (lambda gg: gg(42))(eval('g'))
    return ??
??024()
```

Unevaluated, it's still a closure:

```python
LET_STAR([('g', ??(lambda ??: ??.x * ??.x, ['x']))],
         body=??(??(lambda ??: ??.gg, ['gg']), [Var('g')]))(
42)
```

Python equivalent:

```python
def ??025():
    g = lambda x: x * x
    ?? = (lambda gg: gg)(eval('g'))
    return ??
??025()(42)
```

## LET<a id="let"></a>


`LET` is parallel "assignment." All "local variables" must be bound in the enclosing environment and may not depend on one another. This implementation is not curried. `body` is usually an [`Application`](#application) that automatically receives the local variables in its environment. The $\Lambda$ argument of the `Application` receives this environment as its $\pi$ parameter, so it may access the local variables as free variables in its body.

```python
def LET(
        pairs: List[Tuple[str, Expression]],
        body: Expression,
        ??: Environment = ????
) -> Any:
    if len(pairs) == 0:
        ?? = EVAL(body, ??)
        return ??
    E1 = Environment(lambda: None, ??)
    for p in pairs:
        if isinstance(p[1], Procedure):
            p[1].?? = E1
    _ = [setattr(E1.??, p[0], EVAL(p[1], ??))
         for p in pairs]
    ?? = EVAL(body, E1)
    return ?? 
```

### Examples:


Test depth 0; embedded $\Lambda$ is a thunk (no parameters):

```python
LET([], 
    ??(??(lambda ??: print(43 * 42))))
```

Test depth 1; embedded `Procedure` takes one argument, supplied as a `Var` in the environment of the `LET`:

```python
LET([('z', 42)], 
    ??(????.square, [Var('z')]))
```

Test depth 2; embedded `Procedure` receives an environment with bindings for the local variables:

```python
LET([('z', 42), 
     ('y', 43)], 
    ??(??(lambda ??: ECHO('??', ??).z * ??.y)))
```

Depth 2 with external invocation / evaluation; body -- return value -- of the `LET` is an un-called `Procedure`. The `Procedure` is evaluated, but not called, to propagate the environment, before it is returned:

```python
LET([('z', 42), 
     ('y', 43)], 
    ??(lambda ??: ??.z * ??.y))()
```

Reversed; order does not matter in `LET`:

```python
LET([('y', 42), 
     ('z', 43)], 
    ??(??(lambda ??: ??.z * ??.y)))
```

With applications as values for local variables, the inner `y` is evaluated in the local environment, not leaking down from the global environment $\Gamma\Pi$, where `y` is 0:

```python
DEFINE('y', 0)
LET([('y', 42), 
     ('z', ??(??(lambda ??: ??.y + 1)))],  # Outer y = 0, not inner y = 42
    ??(??(lambda ??: print(??.z * ??.y))))  # Inner y = 42 * inner z = 1
```

Order does not matter:

```python
LET([('z', ??(??(lambda ??: ??.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)], 
    ??(??(lambda ??: print(??.z * ??.y)))) # Inner y = 42 * inner z = 1
```

Print the environment to check that all symbols are bound in it:

```python
LET([('z', ??(??(lambda ??: ??.y + 1))),  # Outer y = 0, not inner y = 42
     ('y', 42)], 
    ??(??(lambda ??: print(ECHO('??', ??).z * ??.y)))) # Inner y = 42 * inner z = 1
```

Prove global `y` is unchanged and that `z` is bound only in local environment, not global.

```python
print({'expect y = 0':  ????.y})
try:
    print(????.z)
except NameError as e:
    print(e.args)
```

Test nested `LET`. Don't forget to chain the environments! The default is $\Gamma\Pi$.

```python
LET([('z0', 42)],
    ??(??(lambda ??0:
        LET([('y0', ??(??(lambda ??1: ??1.z0 + 1)))],
            ??(??(lambda ??2: ??2.z0 * ??2.y0)),
            ??=??0))))  # <~~~ Don't forget to chain!
```

The free variable `x` in the $\lambda$ below is looked up in the local environment established by `LET`.


First, delete `x` again, just in case, so we can check that it does not get bound accidentally.

```python
try: 
    del ????.??.x
except:
    pass
```

```python
LET([('x', 42)],
   ??(??(lambda ??: ??.x * ??.x)))
```

The variable `x` did not leak out of the local environment:

```python
try:
    ????.x
except Exception as e:
    print(e.args)
```

We can get the same result as above when the internal $\lambda$ does not have free variables. When evaluating the application $\Xi$, grab the local value of `x` as an actual argument and substitute it for the bound variable `y` in the body of the $\lambda$:

```python
LET([('x', 42)],
   ??(??(lambda ??: ??.y * ??.y, ['y']), [Var('x')]))
```

### Test `EVAL` on Collections<a id="test-collections"></a>


[Notice that `EVAL` recurses into Dicts, Tuples, Lists, and numpy arrays](#eval). `LET_STAR` and `LET` gives us good tools for testing that.


Lists, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', ??(??(lambda ??: [??.forty_two, ??.forty_two + 1]))),
    ('y', 3)],
   ??(??(lambda ??: ??.x * ??.y)))
```

With bound variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', ??(??(lambda ??: [??.forty_two, ??.forty_two + 1]))),
    ('y', 3)],
   ??(??(lambda ??: ??.xx * ??.yy, ['xx', 'yy']), [Var('x'), Var('y')]))
```

Tuples, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', ??(??(lambda ??: (??.forty_two, ??.forty_two + 1)))),
    ('y', 3)],
   ??(??(lambda ??: ??.x * ??.y)))
```

Dictionaries, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', ??(??(lambda ??: {'??.forty_two': ??.forty_two, 
                         'forty-three': ??.forty_two + 1}))),
    ('y', 3)],
   ??(??(lambda ??: [??.x['??.forty_two'], 
                  ??.x['forty-three']] * ??.y)))
```

Numpy arrays, with free variables:

```python
LET_STAR([
    ('forty_two', 42),
    ('x', ??(??(lambda ??: numpy.array(
        [??.forty_two,
         ??.forty_two + 1])))),
    ('y', 3)],
    ??(??(lambda ??: ??.x * ??.y)))
```

## LETREC<a id="letrec"></a>


`LETREC` binds codependent values in a new environment _before_ evaluating them. `LET` evaluates values before binding them.


`LETREC` patches the environments of any contained procedures to ensure they have access to new bindings. [Lengthy code in `EVAL` does the monkey-patching for us](#eval) later. `LETREC` differs from `LET` only in the absence of `EVAL`s on the value side. Those `EVAL`s are done later.

```python
def LETREC(
        pairs: List[Tuple[str, Any]],
        body: Expression,
        ??: Environment = ????
) -> Any:
    if len(pairs) == 0:
        ?? = EVAL(body, ??)  
        return ??
    E1 = Environment(lambda: None, ??)
    for p in pairs:
        if isinstance(p[1], Procedure):
            p[1].?? = E1
    _ = [setattr(E1.??, p[0], p[1]) for p in pairs]  # <~~~ DON'T EVAL!
    ?? = EVAL(body, E1)
    return ??
```

### Examples


Factorial, familiar by now, accesses itself through the patched environment; `fact` is a free variable in the body of `fact`:

```python
LETREC([('fact', 
         ??(lambda ??: 
           (??.a
            if ??.m <= 0 
            else ??.fact(??.m - 1, ??.m * ??.a)),
           ['m', 'a']))],
       ??(??(lambda ??: ??.fact(6, 1))))
```

Here is a version that fails if the patching is not done correctly:

```python
??(lambda ??o: 
  LETREC([('fact', 
           ??(lambda ??: 
             (??.a
              if ??.m <= ??.m0  # <~~~ Watch out!
              else ??.fact(??.m - 1, ??.m * ??.a)),
             ['m', 'a']))],
         ??(??(lambda ??: ??.fact(6, 1))),
         ??o),  # <~~~ access 'm0'
 ['m0'])(0)
```

Now fully parameterized:

```python
??(lambda ??o: 
  LETREC([('fact', 
           ??(lambda ??: 
             (??.a
              if ??.m <= ??.m0  # <~~~ Watch out!
              else ??.fact(??.m - 1, ??.m * ??.a)),
             ['m', 'a']))],
         ??(??(lambda ??: ??.fact(??.n, 1))),
         ??o),  # <~~~ access 'm0' and 'n'
 ['m0', 'n'])(0, 6)
```

The final application $\Xi$ is necessary to actually evaluate the final $\Lambda$, lest it be simply returned unevaluated:

```python
LETREC([('fact', 
         ??(lambda ??: 
           (??.a
            if ??.m <= 0 
            else ??.fact(??.m - 1, ??.m * ??.a)),
           ['m', 'a']))],
       ??(lambda ??: ??.fact(6, 1)))
```

But we can evaluate it:

```python
LETREC([('fact', 
         ??(lambda ??: 
           (??.a
            if ??.m <= 0 
            else ??.fact(??.m - 1, ??.m * ??.a)),
           ['m', 'a']))],
       ??(lambda ??: ??.fact(6, 1)))()  # <~~~ extra evaluation!
```

One can unroll the final application into formal parameters and actual arguments:

```python
LETREC([('fact', 
         ??(lambda ??: 
           (??.a
            if ??.m <= 0 
            else ??.fact(??.m - 1, ??.m * ??.a)),
           ['m', 'a']))],
       ??(??(lambda ??: ??.fact(??.n, ??.b), 
           ['n', 'b']),  # <~~~ formal parameters
         [6, 1]))  # <~~~ actual arguments
```

Mutually codependent procedures are OK:

```python
LETREC([('z0', ??(lambda ??: 1 + ??.y0(), ['y0'])),
        ('y0', ??(lambda ??: 42))],
      ??(??(lambda ??: ??.y0() * ??.z0(??.y0))))
```

Check that `y0` does not leak into the global environment:

```python
try:
    print(????.y0)
except NameError as e:
    print(e.args)
```

The following shows that `z0` also does not leak from `LETREC`:

```python
try:
    print(????.z0)
except NameError as e:
    print(e.args)
```

The following example is [borrowed from the Racket documentation](https://docs.racket-lang.org/reference/let.html).

```python
LETREC([('is_even',
         ??(lambda ??: True if ??.n == 0 else (not ??.is_odd(??.n)), 
           ['n'])),
        ('is_odd', 
         ??(lambda ??: ??.n != 0 and ??.is_even(abs(??.n) - 1),
           ['n']))],
      ??(??(lambda ??: (
          ??.is_even( 42),
          ??.is_even(-43),
          ??.is_odd (-42),
          ??.is_odd ( 43),
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


Here is a functional flavoring of the instrumentation:

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
       ??(lambda ??o:
         LET([('itakx', ??(lambda ??: ????.irtak(??.c, ??.x - 1, ??.y, ??.z), ['c'])),
              ('itaky', ??(lambda ??: ????.irtak(??.c, ??.y - 1, ??.z, ??.x), ['c'])),
              ('itakz', ??(lambda ??: ????.irtak(??.c, ??.z - 1, ??.x, ??.y), ['c']))],
             ??(??(lambda ??i:
                 LET_STAR(
                     [('lx', ??(??(lambda ??: ??.itakx(??.c + 1)))),
                      ('ly', ??(??(lambda ??: ??.itaky(??.lx[0] + 1)))),
                      ('lz', ??(??(lambda ??: ??.itakz(??.ly[0] + 1))))],
                     ??(??(lambda ??ii:
                         ????.irtak(
                             ??ii.lz[0] + 1,
                             ??ii.lx[1],
                             ??ii.ly[1],
                             ??ii.lz[1]))),
                     ??=??i  # <~~~ don't forget me!
                 ) if ??i.y < ??i.x
                 else
                 (??i.c, ??i.z)
                )),
             ??=??o),
         ['c', 'x', 'y', 'z']))
```

```python
????.irtak(1, 3, 2, 1)  # expect (5, 2)
```

```python
????.irtak(1, 12, 8, 4)  # expect(1733, 5)  # 72msec
```

```python
# assert ????.irtak(1, 18, 12, 6) == (63609, 7)  # 2sec 521msec
# assert ????.irtak(1, 28, 20, 12) == (2493349, 13)  # 1min 36sec
```

#### Tak with $\Upsilon$

```python
DEFINE('tak',
      ??(lambda ??t:
        ??(lambda ??:
          ??.tak(
              ??.tak(??.x - 1, ??.y, ??.z),
              ??.tak(??.y - 1, ??.z, ??.x),
              ??.tak(??.z - 1, ??.x, ??.y))
          if ??.y < ??.x else
          ??.z, ['x', 'y', 'z'], ??t),
        ['tak']))
????.??N(????.tak, ['xt', 'yt', 'zt'])(18, 12, 6)
```

Too slow: 

```python
# ????.??3(????.tak)(28, 20, 12)
```

# LABELS<a id="labels"></a>


`LABELS` is a special case of `LETREC` where all the values are mutually codependent procedures.

```python
def LABELS(
        binding_pairs: List[Tuple[str, Any]], 
        body: Application, 
        ??: Environment = ????
) -> Any:
    for pair in binding_pairs:
        if not isinstance(pair[1], Procedure):
            raise IllegalArgumentsError(
                f'all values in labels must be Procedures; '
                f'this value {pair[1]} is not')
    result = LETREC(binding_pairs, body, ??)
    return result  # <~~~ Hang breakpoint here.
```

Our old friend, factorial:

```python
LABELS([('fact_iter_nom',
        ??(lambda ??: 
        (??.a 
         if ??.m <= 0 
         else ??.fact_iter_nom(??.m - 1, ??.a * ??.m)),
       ['m', 'a']))],
      ??(??(lambda ??: ??.fact_iter_nom(6, 1))))
```

Test monkey patching again:

```python
??(lambda ??o: 
  LABELS([('fact_iter_nom',
           ??(lambda ??: 
             (??.a 
              if ??.m <= ??.m0  # <~~~ Watch out!
              else ??.fact_iter_nom(??.m - 1, ??.a * ??.m)),
             ['m', 'a']))],
         ??(??(lambda ??: ??.fact_iter_nom(6, 1))),
         ??o),
  ['m0'])(1)  # <~~~ Works with 1, also.
```

# DO<a id="do"></a>


Here is the specification of `DO` from Steele's paper, without further explanation here:


```
(DO ((<var1> <init1> <step1>)
     (<var2> <init2> <step2>)
     ...
     (<varN> <initN> <stepN>))
    (<pred> <value>) 
    <optional body>)
```


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
        ??: Environment = ????
) -> Any:
    """(DO ((<var1> <init1> <??step1>)
            (<var2> <init2> <??step2>
            . . .
            (<var??> <init??> <??step??))
            (<??pred> <??value>)
            <??body>
            <env=None>).
    Steps are evaluated sequentially.
    Tail-recursive version requires a LOOPN.
    """
    vars = [CHECK_TYPE(t[0], str) for t in triples]
    inits = [t[1] for t in triples]
    steps = [CHECK_TYPE(t[2], Procedure) for t in triples]
    E1 = Environment(lambda: None, ??)
    _ = [setattr(E1.??, f'??teps_{i}', step)
         for i, step in enumerate(steps)]
    setattr(E1.??, '??red', CHECK_TYPE(pred, Procedure))
    setattr(E1.??, 'va??ue', CHECK_TYPE(value, Procedure))
    setattr(E1.??, '??ody', CHECK_TYPE(body, Procedure))
    r = LABELS([(
        '??oop',
        ??(lambda ??b:
          (EVAL(??('va??ue'), ??b)
           if EVAL(??('??red'), ??b)
           else ??b.??oop(  # <~~~ non-tail recursion
              EVAL(??('??ody'), ??b),
              *[EVAL(??(f'??teps_{i}'), ??b)
                for i in range(len(steps))])),
          ['??ody_result', *vars]))],
        ??('??oop',
          [None, *[EVAL(init, E1) for init in inits]]),
        E1)
    return r
```

Our old friend, factorial, as non-tail-recursive `DO`:

```python
??(lambda ??o:
  DO_NTC([('m', ??o.m, ??(lambda ??: ??.m - 1)),
          ('a', 1, ??(lambda ??: ??.a * ??.m))],
         pred=??(lambda ??: ??.m <= 1),
         value=??(lambda ??: ??.a),
         body=??(lambda ??: None),
         ??=??o),
  ['m'])(6)
```

This blows recursion, naturally:

```python
try:
    ??(lambda ??o:
      DO_NTC([('m', ??o.m, ??(lambda ??: ??.m - 1)),
              ('a', 1, ??(lambda ??: ??.a * ??.m))],
             pred=??(lambda ??: ??.m <= 1),
             value=??(lambda ??: ??.a),
             body=??(lambda ??: None),
             ??=??o),
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
        ??: Environment = ????):
    """(DO ((<var1> <init1> <??step1>)
            (<var2> <init2> <??step2>
            . . .
            (<var??> <init??> <??step??))
            (<??pred> <??value>)
            <??body>
            <env=None>).
    Steps are evaluated sequentially.
    Tail-recursive version requires a LOOPN.
    """
    vars = [CHECK_TYPE(t[0], str) for t in triples]
    inits = [t[1] for t in triples]
    steps = [CHECK_TYPE(t[2], Procedure) for t in triples]
    E1 = Environment(lambda: None, ??)
    _ = [setattr(E1.??, f'??teps_{i}', step)
         for i, step in enumerate(steps)]
    setattr(E1.??, '??red', CHECK_TYPE(pred, Procedure))
    setattr(E1.??, 'va??ue', CHECK_TYPE(value, Procedure))
    setattr(E1.??, '??ody', CHECK_TYPE(body, Procedure))
    r = LABELS([(
        '??oop',
        ??(lambda ??d:  # Domain code is a functino of '??f', ...
          ??(lambda ??b:  # ... which is busines code of N params.
            (EVAL(??('va??ue'), ??b)
             if EVAL(??('??red'), ??b)
             else ??b.??f(  # <~~~ tail recursion
                EVAL(??('??ody'), ??b),
                *[EVAL(??(f'??teps_{i}'), ??b)
                  for i in range(len(steps))])),
            ['??ody_result', *vars], ??=??d),
          ['??f'], ??=E1))],
        ??(??(lambda ??l:
            LOOPN(??l.??oop, ['??ody_result', *vars])  # <~~~ Tail Recursion
            (None, *[EVAL(init, E1) for init in inits]))),
        E1)
    return r
```

```python
??(lambda ??o:
  DO([('m', ??o.m, ??(lambda ??: ??.m - 1)),
      ('a', 1, ??(lambda ??: ??.a * ??.m))],
     pred=??(lambda ??: ??.m <= 1),
     value=??(lambda ??: ??.a),
     body=??(lambda ??: None),
     ??=??o),
  ['m'])(6)
```

```python
??(lambda ??o:
  DO([('m', ??o.m, ??(lambda ??: ??.m - 1)),
      ('a', 1, ??(lambda ??: ??.a * ??.m))],
     pred=??(lambda ??: ??.m <= 1),
     value=??(lambda ??: ??.a),
     body=??(lambda ??: None),
     ??=??o),
  ['m'])(800)
```

# GOTO


TODO


# COND


TODO


# Junkyard / Workshop


Ignore everything below. It's saved in case we need it someday.


## Named Let


## Partial Evaluation and Automatic Currying


## QUOTE, QUASIQUOTE, UNQUOTE, MACROS<a id="quote"></a>


## Normal-Order (Lazy) Evaluation


## Streams (scons, scar, scdr, ...)


## Tanai


## Laziest Tanai
