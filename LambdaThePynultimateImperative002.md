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
#### 12 Nov 2022

+++

# Introduction

+++

In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative constructs with just _lambda_:

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

We follow the paper more-or-less directly, with gleanings from [SICP](https://sarabander.github.io/sicp/).

+++

## _Schemulation:_ Python Semantics in Python

+++

Ideally, we'd compile Python into Scheme or Clojure or Common Lisp, then write transformations, translations, interpreters, debuggers, etc. in Scheme or Clojure or Common Lisp. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in a Scheme-like applicative-order $\lambda$-calculus embedded in basic Python.

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
from typing import Any
@dataclass
class Environment:
    """Set attributes via settattr(env.ϕ, key, val). When getting
    attributes, it's ok to omit the ϕ because of overloaded 
    __getattr__."""
    ϕ: FunctionType   # "frame," also nice place to hang attributes via 'setattr'
    π: "Environment"  # via Greek πηρι, short name for 'enclosing'
    def _get_it(self, key: str) -> Any:
        "recursive lookup"
        try:
            ρ = getattr(self.ϕ, key)
        except AttributeError as _:
            if self.π is None:
                raise NameError(f'Name {key} is unbound.')
            else:  # recurse
                ρ = self.π.__getattr__(key)
        return ρ
    def __getattr__(self, key: str) -> Any:
        """recursive lookup by dot notation"""
        return self._get_it(key)
    def __getitem__(self, key: str) -> Any:
        """recursive lookup by bracket notation"""
        return self._get_it(key)
# diverges because it calls __getattr__ for 'self.ϕ'
#    def __setattr__(self, key, val):
#        setattr(getattr(self, 'ϕ'), key, val)
#        setattr(self.ϕ, key, val)
# The ugliness of 'setattr' is hidden in DEFINE.    
```

# Unique Global Environment $\Gamma\Pi$

```{code-cell} ipython3
ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ
```

# Procedure

+++

A ___procedure___ is a pair of code and environment. ___Code___ is a dictionary of parameter names and a $\lambda$ expression. The $\lambda$, by convention, takes a sigle argument, $\pi$, the environment in which its formal parameters are bound to actual arguments via procedure application, as described in SICP 3.2. This convention seems to be the best we can do for composable $\lambda$s in Schemulator. For now, we support only positional arguments, one-to-one with the argument list. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the application has too many or too few arguments.

+++

By default, procedures are defined in the unique global environment, $\Gamma\Pi$.

```{code-cell} ipython3
from typing import Dict, List, Tuple, Any
Parameters = List[str]  # positional arguments only
class Procedure: 
    """forward reference; will be corrected."""
    pass
def APPLY(proc: Procedure, 
          args: List[Any], 
          π: Environment = ΓΠ) -> Any:
    """forward reference; will be corrected."""
    print(args)
@dataclass
class Procedure:
    code: Dict  # TODO: Check for duplicated symbols in `parameters`.
    π: Environment=ΓΠ  # Defined in global environment by default.
    def __call__(self, *args):
        return APPLY(self, args, self.π)
```

Following the example for _square_ in SICP 3.2.1, let's define it in the global environment and test the invocation of `APPLY`:

```{code-cell} ipython3
setattr(
    ΓΠ.ϕ, 
    "square", 
    Procedure(
        {"body": lambda π: π.x * π.x,  # ugly, I know; sorry :(
         "parameters": ['x']}))
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

## $\Lambda$($\lambda$, params, $\pi$)

+++

Some syntactical help for anonymous procedures. Default parameter list is empty and default parent environment is the unique global environment $\Gamma{}\Pi$.

```{code-cell} ipython3
def Λ(body: FunctionType, 
      parameters: List[str] = [],  # default empty
      π = ΓΠ) -> Procedure:        # default global
    ρ = Procedure(
        code={"body": body, "parameters": parameters}, 
        π=π)
    return ρ
```

```{code-cell} ipython3
setattr(
    ΓΠ.ϕ,
    "square",
    Λ(lambda π: π.x * π.x, ['x']))
ΓΠ.square(5)
ΓΠ.square(5, 6)
```

# Application

+++

In $\lambda$-calculus, an _Application_ is an unevaluated list with a Procedure or symbol (`str`) in the first slot and unevaluated actual arguments in the remaining positions. 

```{code-cell} ipython3
from typing import Union, Any
@dataclass
class Application():
    head: Union[str, Procedure]
    args: List[Any]
    π: Environment = ΓΠ
```

# QUOTE, QUASIQUOTE, UNQUOTE

+++

TODO

+++

## EVAL(expr, $\pi$)

+++

work in progress

+++

`EVAL` calls `APPLY` for Applications. But `APPLY` calls `EVAL` on all arguments, so we must refer to the earlier forward reference for `APPLY` before defining `EVAL`. We get to test this after [`APPLY` is corrected, below](#apply).

```{code-cell} ipython3
def EVAL_APPLICATION(expr: Application, π: Environment = ΓΠ) -> Any:
    if isinstance(expr.head, str):
        head = EVAL(expr.π[expr.head], expr.π)
        assert isinstance(head, Procedure), \
            f'The head of {expr} must be a string or a Procedure, ' \
            f'not a {expr.head}'
    elif isinstance(expr.head, Procedure): 
        head = expr.head
    else:
        raise ValueError(
            f'The head of {expr} must be a string or a Procedure, '
            f'not a {expr.head}')
    eargs = [EVAL(arg, π) for arg in expr.args]
    ρ = APPLY(head, eargs, π)
    return ρ
```

```{code-cell} ipython3
from typing import Any, Dict, Tuple, List

def EVAL(expr: Any, π: Environment = ΓΠ) -> Any:
    """Python does a lot of this for us."""
    if isinstance(expr, int) or \
       isinstance(expr, float) or \
       isinstance(expr, str) or \
       isinstance(expr, bool):
        ρ = expr
    elif isinstance(expr, Procedure):
        ρ = expr
    elif isinstance(expr, Dict):
        # for memo tables:
        ρ = expr
    elif isinstance(expr, Tuple):
        # for memoized, curried functions
        ρ = expr
    elif expr is None:
        ρ = None
    elif isinstance(expr, Application):
        ρ = EVAL_APPLICATION(expr, π)
    else:
        raise ValueError(f'{expr} has unsupported type {type(expr)}')
    return ρ
```

## APPLY(proc, args, $\pi$)<a id="apply"></a>

+++

By default, procedures are applied in the unique global environment, $\Gamma\Pi$.

```{code-cell} ipython3
class IllegalArgumentsError(ValueError):
    pass

def APPLY(proc: Procedure, 
          args: List[Any] = [], # default empty argument list
          π: Environment = ΓΠ) -> Any:
    """How about a nice __getitem__ overload on 'Procedure' for this."""
    if len(proc.code['parameters']) != len(args):
        raise IllegalArgumentsError(
            f"Wrong number of arguments passed to procedure {proc}")
    evaled_args = [EVAL(arg, π) for arg in args]
    E1 = Environment(lambda: None, π)
    for k, v in zip(proc.code['parameters'], evaled_args):
        setattr(E1.ϕ, k, v)
    ρ = proc.code['body'](E1)
    return ρ
    
print(APPLY(ΓΠ.square, [5]))
```

Call via "Pythonic" round brackets:

```{code-cell} ipython3
ΓΠ.square(5)
```

Works on anonymous procedures, too:

```{code-cell} ipython3
Λ(lambda π: π.x * π.x, ['x'])(5)
```

### Test Application

```{code-cell} ipython3
EVAL(Application(ΓΠ.square, [5]))
```

```{code-cell} ipython3
EVAL(Application('square', [42]))
```

## DEFINE(sym, val, $\pi$)

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

ΓΠ.saxpy(4, 10, 2)
```

### SICP 3.2.2

```{code-cell} ipython3
DEFINE('square', 
       Λ(lambda π: π.x * π.x, ['x']))

DEFINE('sum_of_squares',
       Λ(lambda π: π.square(π.x) + π.square(π.y), ['x', 'y']))

DEFINE('f',
       Λ(lambda π: π.sum_of_squares(1 + π.a, 2 * π.a), ['a']))

ΓΠ.f(5)
```

### Exercise 3.9

```{code-cell} ipython3
DEFINE('factorial',
       Λ(lambda π: 1 if π.n < 2 else \
         π.n * π.factorial(π.n - 1), ['n']))

ΓΠ.factorial(6)
```

This doesn't tail-recurse because Python does not tail-recurse. See [Tail Recursion](#tail-recursion) for mitigation work-in-progress.

```{code-cell} ipython3
DEFINE('fact_iter',
       Λ(lambda π: π.product if π.counter > π.max_count else \
         π.fact_iter(
           π.counter * π.product,
           π.counter + 1,
           π.max_count
           ), ['product', 'counter', 'max_count']))

ΓΠ.fact_iter(1, 1, 6)
```

## Procedures that Apply Procedures

+++

Here is a procedure of `f` and `x` that applies `f` to `x`:

```{code-cell} ipython3
# Outer parens necessary to break lines for comments (Python).
(Λ(lambda π:      # Create environment E1 in ΓΠ
  π.f(π.x),       # Apply E1.f to E1.x.
  ['f', 'x'])     # parameters
(ΓΠ.square, 42))  # <~~~ Bind f to square, x to 42.
```

### Anonymous Sibling

+++

Here is a procedure that applies an internal procedure; shadowing is no problem, here:

```{code-cell} ipython3
Λ(lambda π:     # Create environment E1 in ΓΠ.
  Λ(lambda π:   # Create environment E2 in ΓΠ.
    π.n * π.n,  # <~~~ n is bound in E2.
    ['n']       #      (E2 is sibling to E1)
   )            # Parent environment implicitly ΓΠ.
  (π.n),        # <~~~ Look up in E1, bind in E2.
  ['n'])(42)    # <~~~ Bind in E1.
```

### Anonymous Child

+++

Include the non-default $\pi$ on the inner to chain the environments:

```{code-cell} ipython3
Λ(lambda π:     # Create environment E1 in ΓΠ.
  Λ(lambda π:   # Create environment E2 in E1 !!!! 
    π.x * π.n,  # <~~~ n in E1, x in E2.
    ['x'],      #      (E2 is child of E1)
    π)          # Parent environment *explicitly* E1.
  (π.n),        # <~~~ Look up in E1, bind in E2
  ['n'])(42)    # <~~~ Bind in E1
```

## Procedures that Return Procedures

+++

The $\lambda$ below is just the identityf function: it returns its argument.

+++

### Known to Parent

```{code-cell} ipython3
Λ(lambda π:  # Create environment E1 in ΓΠ.
  π.f,       # Just return the value of parameter f.
  ['f'])     # Parent environment implicitly ΓΠ.
(ΓΠ.square)  # <~~~ Procedure bound in ΓΠ
(42)         # Apply the returned procedure.
```

### Anonymous

+++

Return a fresh anonymous procedure rather than one bound to a global symbol as above:

```{code-cell} ipython3
Λ(lambda π:                      # Identity function as above.
  π.f, 
  ['f'])
(Λ(lambda π: π.x * π.x, ['x']))  # <~~~ Anonymous procedure
(42)                             # Apply the returned procedure.
```

## THUNK: Procedure of No Arguments

```{code-cell} ipython3
Λ(lambda π: 42)()
```

```{code-cell} ipython3
def THUNK(body: Any,
          π: Environment = ΓΠ) -> Procedure:
    ρ = Λ(lambda π: 
          EVAL(body, π=π), 
          [], 
          π=π)
    return ρ
```

```{code-cell} ipython3
THUNK(42)()
```

```{code-cell} ipython3
APPLY(THUNK(42), [])
```

## $\Upsilon$: Squaring Square Roots of Functions

+++

[See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md). We follow its derivations step-by-step from first principles.

+++

The running example is recursive factorial.

+++

Don't forget non-default $\pi$ on the inner lest `sf` be undefined.

```{code-cell} ipython3
Λ(lambda π: 
  Λ(lambda π: 1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1), ['n'], π), ['sf'])(
    Λ(lambda π: 
      Λ(lambda π: 1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1), ['n'], π), ['sf']))(6)
```

Abstract `sf(sf)(m)` into a delayed $\lambda$ of `m`:

```{code-cell} ipython3
Λ(lambda π: 
  Λ(lambda π: 
    Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), ['n'], π), 
    ['f'], π)(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)), 
  ['sf'])(
Λ(lambda π: 
  Λ(lambda π: 
    Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), ['n'], π), 
    ['f'], π)(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)), 
  ['sf']))(6)
```

Abstract the _domain code_ into `d`, a function of `f`, the _business code_, is a function of `n`, the _business parameter_.

```{code-cell} ipython3
Λ(lambda π: # of d
  Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
    ['sf'], π)(
      Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
        ['sf'], π)), 
  ['d'])(
    Λ(lambda π: # of f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), ['n'], π), 
      ['f'])
    )(6)
```

Abstract the self-applied $\lambda$ of `sf` into `g`:

```{code-cell} ipython3
Λ(lambda π: # function of domain code, d
  Λ(lambda π: π.g(π.g), ['g'], π)(
      Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
        ['sf'], π)), 
  ['d'])(
    Λ(lambda π: # domain code; function of business code, f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), 
        ['n'], π), # business parameter, n
      ['f']) # recursive function
)(6)
```

## Recursive Factorial via $\Upsilon{}1$

+++

The glyph that looks like "Y" is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). Names in user code should not collide with it if users remember that they should [avoid Greek in user code](#greek).

+++

Package into a system function, $\Upsilon{}1$, for later use. The "1" in the name means that this is for domain codes that return business codes of one parameter:

```{code-cell} ipython3
DEFINE('Υ1', 
       Λ(lambda π: # function of domain code, d
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of one parameter
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m), 
                   ['m'], π)),
               ['sf'], π)), 
         ['d']))

DEFINE('fact_recursive',
      Λ(lambda π: # domain code; function of business code, f
        Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), 
          ['n'], π), # business parameter, n
        ['f'])) # recursive function

ΓΠ.Υ1(ΓΠ.fact_recursive)(6)
```

## Iterative Factorial via $\Upsilon{}3$

+++

$\Upsilon$ must be tailored for a given number of business parameters. This one is for three. We could write a Python-AST hack to handle any number of business parameters, but that's Python macrology, a rabbit hole to sidestep for now (TODO: reconsider).

```{code-cell} ipython3
# λ d: (λ g: g[g])(λ sf: d[λ m, c, x: sf[sf][m, c, x]]) 
DEFINE('Υ3', 
       Λ(lambda π: # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of three parameters
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m, π.c, π.x), 
                   ['m', 'c', 'x'], π)), 
               ['sf'], π)), 
         ['d']));
```

Here is user-level domain code, redefining `fact_iter` in domain-code form. Any domain code is a function of `f`, recursive _business code_. In this case, `f` is a function of 3 business parameters. This will get us to a tail-recursive solution in the [section on tail recursion](#tail-recursion).

```{code-cell} ipython3
# λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter', # domain code is a function of f ...
       Λ(lambda π: # ... which is business code.
         Λ(lambda π: π.m if π.c > π.x else 
           π.f(π.m * π.c, π.c + 1, π.x), # business code
           ['m', 'c', 'x'], π), # business parameters
         ['f']));
```

```{code-cell} ipython3
ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 6)
```

# Tail Recursion<a id="tail-recursion"></a>

+++

Thanks to [Thomas Baruchel for this idea on tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). If users are aware that their domain code is tail-recursive, then they may call it via `LOOP` instead of via $\Upsilon$. In Scheme, detection of tail recursion is automatic. In Python and Schemulator, users must manage tail recursion explicitly. This isn't a terrible issue. Tail-calls are lexically obvious, so users should always know. In Clojure, there is precedent. Users must explicitly write `loop` and `recur`. In any event, domain code can always be called via the proper $\Upsilon$, the one that knows the count of business parameters.

+++

We imitate Clojure's names `loop` and `recur`. The glyph that looks like "P" below is Greek Capital Rho for "recur." `LOOP3` has the same signature as $\Upsilon3$; it takes domain code as its sole argument. Names in user code should not collide with P B if users remember that they should [avoid Greek in user code](#greek). As with $\Upsilon$, Rho and `LOOP` must know their argument counts. That's OK for now (TODO: reconsider).

```{code-cell} ipython3
class TailCall(Exception):  
    """αναδρομική κλήση"""
    def __init__(self, *args):
        """Overwrite old args with new."""
        self.args = args

def RECUR(*args):  
    """υψώνω: in sincere flattery of Clojure"""
    raise TailCall(*args)

def LOOP3(d: Procedure) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
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

LOOP3(ΓΠ.fact_iter)(1, 1, 20)
```

> ***Results are undefined if you call any `LOOP` function with non-tail-recursive domain code.***

+++

## Prove It

+++

The recursive version blows Python's recursion limit.

```{code-cell} ipython3
try:
    print(ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 371))  # smallest that blows the limit
except RecursionError as e:
    print(e.args)
```

The tail-call version does not. Notice the domain code `fact_iter` is EXACTLY the same as in the recursive version above.

```{code-cell} ipython3
try:
    print(LOOP3(ΓΠ.fact_iter)(1, 1, 371))
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

This is miserable even for $n=23$. You won't want to call it for bigger arguments.

```{code-cell} ipython3
ΓΠ.Υ1(ΓΠ.fib_slow)(23)
```

```{code-cell} ipython3
timeit(ΓΠ.Υ1(ΓΠ.fib_slow)(23))
```

Without linearization, Fibonacci 500 would not complete in $10^{30}$ times the Age of the Universe. One way to linearize is tail recursion. Another way is [memoization](#memoization) (not _memorization_). Tail-recursive memoization is possible but not necessary. A tail-recursive Fibonacci easy and blazingly fast:

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

Time it:

```{code-cell} ipython3
timeit(LOOP3(ΓΠ.fib_iter)(0, 1, 23))
```

4000 times faster. Stress it:

```{code-cell} ipython3
LOOP3(ΓΠ.fib_iter)(0, 1, 5000)
```

# Memoized [sic] Fibonacci<a id="memoization"></a>

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

The domain code for a memoized, Curried Fibonacci follows. The parameter `a` is the _accumulator_, _associator_, or memo, whatever word you like best. This is easiest to read (and to write) from the bottom up. It looks horrendous, but it isn't really.

```{code-cell} ipython3
DEFINE('fib_fast',
       Λ(lambda π: # of f; level 1
         Λ(lambda π: # of a; level 2
           Λ(lambda π: # of n; level 3
             (π.a, 1) if π.n < 2 else
             Λ(lambda π: # of n_1; level 4
               (π.a, π.a[π.n_1]) # optimizer should remove these two lines
               if π.n_1 in π.a else # ^^^
               Λ(lambda π: # of fim1; level 5
                 Λ(lambda π: # of m1; level 6
                   Λ(lambda π: # of r1; level 7
                     Λ(lambda π: # of a1; level 8
                       Λ(lambda π: # of n_2; level 9
                         (π.a1, π.r1 + π.a1[π.n_2]) # <~~~ a quick exit
                         if π.n_2 in π.a1 else 
                         Λ(lambda π: # of fim2; level 10
                           Λ(lambda π: # of m2; level 11
                             Λ(lambda π: # of r2; level 12
                               Λ(lambda π: # of a2; level 13
                                 (π.a2, π.r1 + π.r2), # <~~~ the money line
                                 ['a2'], π)(π.m2[0] | {π.n_2: π.r2}),  # <~~~ update memo
                               ['r2'], π)(π.m2[1]), # unpack
                             ['m2'], π)(π.fim2(π.n_2)), # unpack
                           ['fim2'], π)(π.f(π.a1)), # <~~~ recurse
                         ['n_2'], π)(π.n - 2), # DRY
                       ['a1'], π)(π.m1[0] | {π.n_1: π.r1}), # <~~~ update memo
                     ['r1'], π)(π.m1[1]), # unpack
                   ['m1'], π)(π.fim1(π.n_1)), # unpack
                 ['fim1'], π)(π.f(π.a)), # <~~~ recurse
               ['n_1'], π)(π.n - 1), # DRY
             ['n'], π), # business parameter
           ['a'], π), # curried memo
         ['f'])) # domain code 
ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1]
```

1,000 times faster

```{code-cell} ipython3
timeit(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1])
```

But still blows the recursion limit:

```{code-cell} ipython3
try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(185)[1])
except RecursionError as e:
    print(e.args)

try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(184)[1])
except RecursionError as e:
    print(e.args)
```

## Memo Table as Business Parameter

+++

Currying is useful in general, but complicates this case. Get rid of it.

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

Need $\Upsilon2$ to call it:

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
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 246)[1])
except RecursionError as e:
    print(e.args)
    
try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 245)[1])
except RecursionError as e:
    print(e.args)
```

## Recursive With Memo

+++

Section [Tail-Recursive Fibonacci](#tail-recursive-fibonacci) exhibits a very short solution without a memo table. Could we write a tail-recursive version with a memo table? Is it worth the effort? Perhaps as a mental exercise.

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

### Tail-Recusive

```{code-cell} ipython3
def LOOP5(d: Procedure) -> Procedure:
    """in sincere flattery of Clojure, and thanks to Thomas Baruchel."""
    DEFINE('Ρ3', Λ(lambda π: 
                   RECUR(π.m, π.c, π.x, π.a, π.b), 
                   ['m', 'c', 'x', 'a', 'b']));
    def looper(*args):
        """Expression form of a while-loop statement."""
        while True:
            try: 
                return d(ΓΠ.Ρ3)(*args)
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
    
try:
    print(LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)
```

# DO

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

# LABELS

+++

# SET_BANG

+++

Tested below in [block](#block). Like Gambit and unlike Common Lisp and Emacs Lisp, we can only `set!` symbols that are already `define`d.

```{code-cell} ipython3
def SET_BANG(
        sym: str, 
        val: Any, 
        π: Environment = ΓΠ) -> None:
    ee = EVAL(val, π)
    """recursive lookup"""
    while π is not None:
        try:
            getattr(π.ϕ, sym)
            break
        except AttributeError as _:
            if π.π is None:
                raise NameError(f'Name {sym} is unbound.')
            else:  # recurse
                π = π.π
    setattr(π.ϕ, sym, ee)
    return None  # following Gambit Scheme
```

# BLOCK / BEGIN<a id="block"></a>

+++

Sequencing of statements and expressions is not fundamental. Instead, we must chain $\lambda$s. The paper calls the form `BLOCK`. Scheme calls it `BEGIN`. Common Lisp calls it `PROGN`. 

+++

## 1-THUNK Convention: Statements are 1-Thunks

+++

In the logician's $\lambda$-calculus, all $\lambda$-forms take one parameter. In Schemulator, procedures of one parameter are ***1-Thunks***, just as procedures of no parameters are ***thunks***. We need to pass the output of one 1-thunk to the next 1-thunk so as to force time-order of execution to follow dependency order of argument evaluation by function application, i.e, to follow applicative-order semantics. Haskell calls such forcing "scheduling." That might be a bit of a heavy word for the forcing because there is no run-time scheduler: it's all done at compile time.

```{code-cell} ipython3
def THUNK1(body: Any,
           π: Environment = ΓΠ):
    ρ = Λ(lambda π: body, ['_'], π=π)
    return ρ
```

Use `_` as a dummy parameter, as does Python.

+++

First, following the paper, we define a block of two statements.

```{code-cell} ipython3
def BLOCK2(s1: Procedure, 
           s2: Procedure,
           π: Environment = ΓΠ) -> Any:
    """Sequentially execute two statement-expressions."""
    APPLY(
        s2,                      #      By applicative order:
        [APPLY(s1, [None], π)],  # <~~~ inner is eval'ed first.
        π)
    
DEFINE('x', 0)    
BLOCK2(
    Λ(lambda π: SET_BANG('x', 6, π), ['_']), 
    Λ(lambda π: print(π.x * 7), ['_']))

try:
    BLOCK2(
        Λ(lambda π: SET_BANG('y', 6, π), ['_']), 
        Λ(lambda π: print(π.z * 7), ['_']))             
except NameError as e:
    print(e.args)
```

```{code-cell} ipython3
def BLOCK(*ss: List[Procedure], 
         π: Environment = ΓΠ) -> Any:
    intermediate = None
    for s in ss:
        if intermediate is not None:
            intermediate = APPLY(s, 
                                 [APPLY(
                                     intermediate,
                                     [None],
                                     π=π)],
                                 π=π)
        intermediate = APPLY(s, [None], π)

BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π), ['_']), 
    Λ(lambda π: print(π.x * 7), ['_']))

DEFINE('x', 0)
DEFINE('y', 42)
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π), ['_']), 
    Λ(lambda π: SET_BANG('x', π.x * 7, π), ['_']),
    Λ(lambda π: print(π.x * π.y), ['_']))

try:
    BLOCK(
        Λ(lambda π: SET_BANG('x', 6, π), ['_']), 
        Λ(lambda π: SET_BANG('x', π.x * 7, π), ['_']),
        Λ(lambda π: print(π.x * π.y), ['_']),
        Λ(lambda π: π.z, ['_']))
except NameError as e:
    print(e.args)
```

```{code-cell} ipython3
BEGIN = BLOCK
```

# LET*, LET, LETREC

+++

`LET_STAR` is sequential binding of locals. `LET` is parallel binding. `LETREC` is mutually recursive `LET`.

+++

To start, let's say that `body` must be a thunk. 

+++

TODO: This choice may lead to redesign of `EVAL`.

```{code-cell} ipython3
def LET_STAR(
        binding_pairs: List[Tuple[str, Any]], 
        body: Any, 
        π: Environment = ΓΠ) -> Any:
    if len(binding_pairs) == 1:
        k, v = binding_pairs[0]
        ρ = APPLY(
            Λ(lambda π: body,
              [k],
              π=π),
            [v], 
            π=π)
        return ρ
    else:
        pass
    
LET_STAR([('z', 42)], 
           Λ(lambda π:
             ΓΠ.square(π.z)))()
```

# COND

+++

# Junkyard

+++

Ignore everything below. It's saved in case we need it someday.
