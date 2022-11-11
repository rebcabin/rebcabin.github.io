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

# Lambda, the Pynultimate Imperative

+++

#### Version 2
#### Brian Beckman
#### 9 Nov 2022

+++

# Introduction

+++

In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative constructs with just _lambda_:

+++

> We demonstrate how to model the following common [imperative] programming constructs in ... an applicative-order language similar to LISP: Simple Recursion, Iteration, Compound Statements and Expressions, GO TO and Assignment, Continuation-Passing, Escape Expressions, Fluid Variables, Call by Name, Call by Need, and Call by Reference. The models require only (possibly self-referent) lambda application, conditionals, and (rarely) assignment.

+++

It's useful to recap this paper in Python, which has most of the listed imperative constructs. Imagine compiling Python into an intermediate language in which the semantics, even those with side-effects, are laid bare as trees of $\lambda$ expressions. In such a representation, optimizations are 
1. easy to write as tree-to-tree transforms
1. easy to extend via just function composition
2. independent of surface syntax, thus easy to share with other imperative languages like Fortran, C, Java
3. independent of back ends, thus easy to run interactively; or to translate into LLVM, x86, ARM64, C, for execution; or to transpile into other surface languages

+++

The use-cases above are similar to those for a SQL algebraizer. Many SQL implementations 
1. translate the surface language into bare-bones expressions in a closed relational algebra, free of original syntax
2. run the algebraic expressions through symbolic optimizers, which often rearrange the expressions completely
2. incrementally extend the system by composing new optimization
3. translate optimized expressions into commands for local and distributed servers

+++

We follow the paper more-or-less directly, with gleanings from [SICP](https://sarabander.github.io/sicp/).

+++

## _Schemulation:_ Python Semantics in Python

+++

Ideally, we'd compile Python into Scheme or Clojure or Common Lisp, then write transformations, translations, interpreters, debuggers, etc. in Scheme or Clojure or Common Lisp. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in a Scheme-like applicative-order $\lambda$ calculus embedded in basic Python.

+++

Some people may find this weird. Why not just implement Scheme in Python instead of emulating Scheme in Python? It's an engineering judgment call. Most authors of compilers and interpreters spend undue time on syntax before even getting to semantics. Compiler textbooks tell you to do this! Semantics becomes a hidden "implementation detail," but then developers forgive themselves for making an ungodly mess of it. We prefer the other way around. Get the semantics clean, composable, extendable, maintainable, and correct, _first_, then spend time on syntax, if you even care any more. Or just let someone else do it. From the development point of view, syntax is a distraction. It's been solved for decades, but it makes young developers and professors feel powerful. The decision to spend time on syntax before semantics is not engineering, it's self-indulgence.

+++

## Orthogonality as a Design Principle

+++

We prefer designs that minimize cross-talk. Each facility -- transformation layer or module -- should have the least possible dependency on other facilities. For example, tranformations that affect control flow need not necessarily depend on transformations that affect numerics.

+++

Contrast to a braided design, where each facility explicitly accounts for every other.

+++

# Environment and Frame

+++

We find it necessary to model Scheme's environments and frames explicitly. We tried multiple short-cut alternatives and found that none of them compose well.

+++

[SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) has some apparent contradictions in the definition of environment and frame. It says that "an environment is a sequence of frames," but the rest of the text and figures clearly imply that an environment has just one frame.

+++

The best resolution appears to be:

+++

> An ___environment___ is a frame $\phi$ and a pointer $\pi$ to an enclosing environment. A ___frame___ is a mathematical function from variable names to values; no variable name may appear more than once in a frame.

+++

We note in passing that this works only for a single thread. [Clojure, for instance, solves that problem with _Vars_](https://clojure.org/reference/vars).

+++

## Greek and ALL CAPS<a id="greek"></a>

+++

$\pi$ is a nice pun: it evokes $\pi\eta\rho\iota$, a Greek prefix meaning "surrounding."

+++

We use Greek letters for system attributes. User code should avoid Greek to avoid clobbering system attributes.

+++

We use ALL CAPS for system functions that implement Schemulator.

+++

## Bindings

+++

> A ___binding___ is an association from a variable name to a value, that is, an entry in a frame.

+++

We might model a binding as a pair, or as a row in a table, an element of a relation (subset of a Cartesian product), an element of a Python dictionary, or as an attribute of a Python object. We prefer the attribute model because it affords _dot_ notation for lookup, that is, `o.foo` rather than the dictionary's syntax `o['foo']`.

+++

If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a virtual sequence of frames via the chain of $\pi\eta\rho\iota$ pointers to enclosing environments.

+++

> The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`.

+++

> A frame $\phi$ belongs to a virtual sequence of environments implied by the unidirectional pointer-chain of enclosing environments rooted in $\phi$ and ending at the unique global environment. The ___value of a variable in an environment___ is the value in the first binding in any frame of that sequence. Bindings lower in the chain may ___shadow___ bindings higher in the chain, rendering them inaccessible. If no frame in a chain specifies a binding for the variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another.

+++

Thanks to [divs1210](https://gist.github.com/divs1210?page=3) for the idea of modeling frames as dummy lambda functions, and assigning attributes to them.

+++

We override `__getattr__` to avoid a separate function for recursive lookup. We can't also override `__setaddr__` to call `setattr(self.$\phi$ ...)` because `self.$\phi$` diverges on `getattr(self.$\phi$ ...)`.

+++

There is a unique global environment, $\Gamma\Pi$, for each session.

+++

TODO: Coalesce $\Gamma\Pi$ with Python's global environment.

```{code-cell} ipython3
from dataclasses import dataclass, field
from types import FunctionType
@dataclass
class Environment:
    """Set attributes via settattr(env.ϕ, key, val). When getting
    attributes, it's ok to omit the ϕ."""
    ϕ: FunctionType   # "frame," also nice place to hang attributes via 'setattr'
    π: "Environment"  # via Greek πηρι, short name for 'enclosing'
    def __getattr__(self, key):
        """recursive lookup"""
        try:
            result = getattr(self.ϕ, key)
        except AttributeError as _:
            if self.π is None:
                raise NameError(f'Name {key} is unbound.')
            else:  # recurse
                result = self.π.__getattr__(key)
        return result
# diverges because it calls __getattr__ for 'self.ϕ'
#    def __setattr__(self, key, val):
#        setattr(getattr(self, 'ϕ'), key, val)
#        setattr(self.ϕ, key, val)
# The ugliness of 'setattr' is hidden in DEFINE and DEFINE_PROC.
    
ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ
```

# Procedure

+++

A ___procedure___ is a pair of code and environment. ___Code___ is a dictionary of parameter names and a $\lambda$ expression. The $\lambda$, by convention, takes a sigle argument, $\pi$, the environment in which its formal parameters are bound to actual arguments via procedure application, as described in SICP 3.2. This convention seems to be the best we can do for composable $\lambda$s in Schemulator. For now, we support only positional arguments, one-to-one with the argument list. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the application has too many or too few arguments.

+++

By default, procedures are defined in the unique global environment, $\Gamma\Pi$.

+++

Experimental: `__getitem__` call syntax.

```{code-cell} ipython3
from typing import Dict, List, Tuple
Parameters = List[str]  # positional arguments only
def APPLY(proc, args, π=ΓΠ):
    """forward reference; Will be redefined."""
    print(args)
@dataclass
class Procedure:
    code: Dict
    π: Environment=ΓΠ  # Defined in global environment by default.
    def __getitem__(self, keys):
        if isinstance(keys, List) or isinstance(keys, Tuple):
            return APPLY(self, keys, self.π)
        else:
            return APPLY(self, [keys], self.π)
# experimental:
#    def __call__(self, *args):
#        return APPLY(self, args, self.π)
```

Following the example for _square_ in SICP 3.2.1, let's define it in the global environment and test the invocation of `APPLY`:

```{code-cell} ipython3
setattr(
    ΓΠ.ϕ, 
    "square", 
    Procedure(
        {"body": lambda π: π.x * π.x,  # ugly, I know; sorry :(
         "parameters": ['x']}))
ΓΠ.square[5]
ΓΠ.square[5, 6]
#ΓΠ.square(5)
#ΓΠ.square(5, 6)
```

## $\Lambda$

+++

Some syntactical help for anonymous procedures:

```{code-cell} ipython3
def Λ(body, parameters, π=ΓΠ):
    return Procedure(code={
        "body": body, "parameters": parameters}, 
              π=π)
```

```{code-cell} ipython3
setattr(
    ΓΠ.ϕ,
    "square",
    Λ(lambda π: π.x * π.x, ['x']))
ΓΠ.square[5]
ΓΠ.square[5, 6]
```

## EVAL

+++

work in progress

```{code-cell} ipython3
from typing import Any
def EVAL(expr: Any, π: Environment=ΓΠ) -> Any:
    """Python does almost all this for us."""
    if isinstance(expr, int) or \
       isinstance(expr, float) or \
       isinstance(expr, str) or \
       isinstance(expr, bool):
        result = expr
    elif isinstance(expr, Procedure):
        result = expr
    else:
        raise ValueError
    return result
```

## APPLY

+++

By default, procedures are applied in the unique global environment, $\Gamma\Pi$.

```{code-cell} ipython3
class IllegalArgumentsError(ValueError):
    pass

def APPLY(proc: Procedure, args: List[Any], π:Environment=ΓΠ) -> Any:
    """How about a nice __getitem__ overload on 'Procedure' for this."""
    if len(proc.code['parameters']) != len(args):
        raise IllegalArgumentsError(
            f"Wrong number of arguments passed to procedure {proc}")
    evaled_args = [EVAL(arg, π) for arg in args]
    E1 = Environment(lambda: None, π)
    for k, v in zip(proc.code['parameters'], evaled_args):
        setattr(E1.ϕ, k, v)
    result = proc.code['body'](E1)
    return result
    
print(APPLY(ΓΠ.square, [5]))
```

Square-bracket notation (like Wolfram / Mathematica) works:

```{code-cell} ipython3
ΓΠ.square[5]
```

Works on anonymous procedures, too:

```{code-cell} ipython3
Λ(lambda π: π.x * π.x, ['x'])[5]
```

## DEFINE

+++

Package up the "defining" boilerplate.

+++

By default, symbols are defined in the unique global environment, $\Gamma\Pi$.

```{code-cell} ipython3
def DEFINE(sym: str, val: Any, π: Environment=ΓΠ) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
    return val

DEFINE('saxpy', 
       Λ(lambda π: π.a * π.x + π.y, ['a', 'x', 'y']))

ΓΠ.saxpy[4, 10, 2]
```

### SICP 3.2.2

```{code-cell} ipython3
DEFINE('square', 
       Λ(lambda π: π.x * π.x, ['x']))

DEFINE('sum_of_squares',
       Λ(lambda π: π.square[π.x] + π.square[π.y], ['x', 'y']))

DEFINE('f',
       Λ(lambda π: π.sum_of_squares[1 + π.a, 2 * π.a], ['a']))

ΓΠ.f[5]
```

### Exercise 3.9

```{code-cell} ipython3
DEFINE('factorial',
       Λ(lambda π: 1 if π.n < 2 else \
         π.n * π.factorial[π.n - 1], ['n']))

ΓΠ.factorial[6]
```

This doesn't tail-recurse because Python does not tail-recurse. See [Tail Recursion](#tail-recursion) for mitigation work-in-progress.

```{code-cell} ipython3
DEFINE('fact_iter',
       Λ(lambda π: π.product if π.counter > π.max_count else \
         π.fact_iter[
           π.counter * π.product,
           π.counter + 1,
           π.max_count
           ], ['product', 'counter', 'max_count']))

ΓΠ.fact_iter[1, 1, 6]
```

## Procedures that Apply Procedures

```{code-cell} ipython3
Λ(lambda π: π.f[π.x],['f', 'x'])[ΓΠ.square, 42]
```

Anonymously; shadowing is no problem, here:

```{code-cell} ipython3
Λ(lambda π: Λ(lambda π: π.n * π.n, ['n'], π)[π.n], ['n'])[42]
```

But you had better include the non-default $\pi$ on the inner if you don't want accidentally the correct answer.

```{code-cell} ipython3
Λ(lambda π: Λ(lambda π: π.x * π.n, ['x'], π)[π.n], ['n'])[42]
```

## Procedures that Return Procedures

```{code-cell} ipython3
Λ(lambda π: π.f, ['f'])[ΓΠ.square][42]
```

Anonymously:

```{code-cell} ipython3
Λ(lambda π: π.f, ['f'])[Λ(lambda π: π.x * π.x, ['x'])][42]
```

## Square Roots of Functions

+++

[See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md). We follow its derivations step-by-step from first principles.

+++

Don't forget non-default $\pi$ on the inner lest `sf` be undefined.

```{code-cell} ipython3
Λ(lambda π: 
  Λ(lambda π: 1 if π.n < 1 else π.n * π.sf[π.sf][π.n - 1], ['n'], π), ['sf'])[
    Λ(lambda π: 
      Λ(lambda π: 1 if π.n < 1 else π.n * π.sf[π.sf][π.n - 1], ['n'], π), ['sf'])][6]
```

Abstract into a $\lambda$ of `m` a $\lambda$-delayed self-application of `sf`:

```{code-cell} ipython3
Λ(lambda π: 
  Λ(lambda π: 
    Λ(lambda π: 1 if π.n < 1 else π.n * π.f[π.n - 1], ['n'], π), 
    ['f'], π)[Λ(lambda π: π.sf[π.sf][π.m], ['m'], π)], 
  ['sf'])[
Λ(lambda π: 
  Λ(lambda π: 
    Λ(lambda π: 1 if π.n < 1 else π.n * π.f[π.n - 1], ['n'], π), 
    ['f'], π)[Λ(lambda π: π.sf[π.sf][π.m], ['m'], π)], 
  ['sf'])][6]
```

Abstract into `d` the _domain code_, a function of `f`, the _business code_, a function of `n`, the _busieness parameter_.

```{code-cell} ipython3
Λ(lambda π: # of d
  Λ(lambda π: π.d[Λ(lambda π: π.sf[π.sf][π.m], ['m'], π)],
    ['sf'], π)[
      Λ(lambda π: π.d[Λ(lambda π: π.sf[π.sf][π.m], ['m'], π)],
        ['sf'], π)], 
  ['d'])[
    Λ(lambda π: # of f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f[π.n - 1], ['n'], π), 
      ['f'])
][6]
```

Abstract into `g` the self-application of the function of the square root, the function of `sf`.

```{code-cell} ipython3
Λ(lambda π: # of d
  Λ(lambda π: π.g[π.g], ['g'], π)[
      Λ(lambda π: π.d[Λ(lambda π: π.sf[π.sf][π.m], ['m'], π)],
        ['sf'], π)], 
  ['d'])[
    Λ(lambda π: # of f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f[π.n - 1], ['n'], π), 
      ['f'])
][6]
```

## Iterative Factorial via $\Upsilon$

+++

The thing that looks like "Y" below is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). Names in user code should not collide with it if users remember that they should [avoid Greek in user code](#greek).

```{code-cell} ipython3
# λ d: (λ g: g[g])(λ sf: d[λ m, c, x: sf[sf][m, c, x]]) 
DEFINE('Υ3', 
       Λ(lambda π: # of d, the domain code
         Λ(lambda π: π.g[π.g], ['g'], π)[
           Λ(lambda π: π.d[Λ(lambda π: π.sf[π.sf][π.m, π.c, π.x], 
                   ['m', 'c', 'x'], π)], 
             ['sf'], π)], 
         ['d']));
```

$\Upsilon$ must be tailored for a given number of arguments. This one is for 3 arguments. We could write a Python-AST hack to handle any number of arguments, but that's Python macrology, a rabbit hole we'd like to sidestep for now. An alternative is a tower of general-purpose multi-pargument $\Upsilon$s, and that's OK for now (TODO: reconsider).

+++

User-level domain code, redefining `fact_iter`. Any domain code is a function of `f`, recursive _business code_. In this case, `f` is a function of 3 _business parameters_. This will get us to a tail-recursive solution in the [section on tail recursion](#tail-recursion). 

```{code-cell} ipython3
# λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter', # domain code ...
       Λ(lambda π: # ... function of f, which is business code.
         Λ(lambda π: π.m if π.c > π.x else 
           π.f[π.m * π.c, π.c + 1, π.x], # business code
           ['m', 'c', 'x'], π), # business parameters
         ['f']));
```

```{code-cell} ipython3
ΓΠ.Υ3[ΓΠ.fact_iter][1, 1, 6]
```

# Tail Recursion<a id="tail-recursion"></a>

+++

Thanks to [Thomas Baruchel for this idea on tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). If users are aware that their domain code is tail-recursive, then they may call it as follows instead of through $\Upsilon$. In Scheme, tail recursion is the default, but in Python and Schemulator, users must manage it explicitly. This isn't a terrible issue, tail-calls are lexically obvious. Clojure does likewise, where users must explicitly write `loop` and `recur`.

+++

The thing that looks like "P" below is Greek Capital Rho for "recur" and "B" is Greek Capital Beta for "Baruchel." B has the same signature as $\Upsilon$; it takes a _domain code_ as its sole argument. Names in user code should not collide with P and B if users remember that they should [avoid Greek in user code](#greek). As with $\Upsilon$, Rho must know its number of arguments. That's OK for now (TODO: reconsider).

```{code-cell} ipython3
class TailCall(Exception):  # αναδρομική_κλήση
    def __init__(self, *args):
        self.args = args

def RECUR(*args):  # υψώνω
    raise TailCall(*args)

DEFINE('Ρ3', Λ(lambda π: 0, ['m', 'c', 'x']))
        
def bet0(func):
    def recursor(*args):
        raise RecursiveCall(*args)
    
    def wrapper(*args):
        while True:
            try:
                return func(RECUR)(*args)
            except TailCall as e:
                args = e.args
    return wrapper

bet0(lambda f: lambda m, c, x: \
     m if c > x else \
     f(c * m, c + 1, x))(1, 1, 6)
```

```{code-cell} ipython3
class TailCall(Exception):
    def __init__(self, *args):
        self.args = args
        
def RECUR(*args):
    raise TailCall(*args)

def bet0(func):
    def wrapper(*args):
        while True:
            try:
                return func(RECUR)(*args)
            except TailCall as e:
                args = e.args
    return wrapper

bet0(lambda f: lambda product, counter, max_count: \
     product if counter > max_count else \
     f(counter * product, counter + 1, max_count))(1, 1, 6)    
```

```{code-cell} ipython3
def bet0(func):
    class RecursiveCall(Exception):
        def __init__(self, *args):
            self.args = args

    def recursor(*args):
        raise RecursiveCall(*args)
    
    def wrapper(*args):
        while True:
            try:
                return func(recursor)(*args)
            except RecursiveCall as e:
                args = e.args
    return wrapper

bet0(lambda f: lambda product, counter, max_count: \
     product if counter > max_count else \
     f(counter * product, counter + 1, max_count))(1, 1, 6)
```

# Junkyard

+++

Ignore everything below. It's saved in case we need it someday.

```{code-cell} ipython3

```
