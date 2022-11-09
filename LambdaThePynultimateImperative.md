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

#### Brian Beckman
#### 7 Nov 2022

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

Each facility in our work must address three concerns:
1. What constructions in the $\lambda$ calculus are we talking about? This is a theoretical level, translated into Python $\lambda$ expressions as directly as possible.
2. What imperative constructs, i.e., Simple Recursion, Iteration, Compound Statements and Expressions, etc., of the ***object-language***, here Python, are we talking about? 
3. How do we represent and implement those constructs in the Schemulator? 

+++

## Orthogonality as a Design Principle

+++

We prefer designs that minimize cross-talk. Each facility -- transformation layer or module -- should have the least possible dependency on other facilities. For example, tranformations that affect control flow need not necessarily depend on transformations that affect numerics.

+++

Contrast to a braided design, where each facility explicitly accounts for every other.

+++

# Definitions

+++

## Functions and $\lambda$ Expressions

+++

All functions in $\lambda$ calculus are anonymous. Python functions can be _named_ or _anonymous_. In Python, named functions are usually introduced via `def`, as follows:

```{code-cell} ipython3
def foo(x):
    result = x * (x + 1)
    return result
```

> Aside: The local variable `result` is a habit: a convenient place to hang a debugger breakpoint, as an alternative to direct returns, which some debuggers won't report to the user easily:

```{code-cell} ipython3
def foo(x):
    return x * (x + 1)
```

Invoke named functions as follows:

```{code-cell} ipython3
foo(6)
```

Python and Schemulator anonymous functions are introduced via $\lambda$ expressions, as follows:

+++

`(lambda x: x * (x + 1))`

+++

and invoked as follows:

```{code-cell} ipython3
(lambda x: x * (x + 1))(6)
```

Notice that we must invoke the $\lambda$ literally, by copy-paste, because there is no name by which to invoke it.

+++

However, we can name anonymous functions by assigning them to Python variables. Although the $\lambda$ expression below is valid Schemulator, the assignment and the name `foo` are not. We say that the name `foo` is ___notional___ in Schemulator, meaning convenient for discussion or implementation in Python but not representable in the Schemulator language.

```{code-cell} ipython3
foo = (lambda x: x * (x + 1))
foo(6)
```

The bodies of Python $\lambda$ expressions contain exactly one Python _expression_. There is no place to hang a debugger breakpoint in a Python $\lambda$ expression. We cannot write assignment _statements_ inside Python $\lambda$s. Python $\lambda$s are hard to debug.

+++

## Bound and Free Variables<a id="bound-and-free"></a>

+++

This is a confusing topic because the parlances of Python and of $\lambda$ calculus differ. Schemulator is a simulation of $\lambda$ calculus in Python, and the parlance of Schemulator must split the difference.

+++

In $\lambda$ calculus, Python, and Schemulator, we say a variable is ___bound___ if it has a value, and ___unbound___ otherwise. Don't confuse this meaning of _bound_ with the meanings of _$\lambda$-bound_ and _P-bound_, defined below.

+++

## Bindings

+++

The pair of a variable and its current value is its ___binding___. We say that looking up the value of a variable is ___consulting___ its binding. 

+++

## Contexts

+++

Bindings depend on context. A binding can change depending on where its variable appears in the program text, and depending on when its binding is consulted. The ___static context___ of a binding is its ___lexical closure___ -- the lexical or textual body of a function where the variable appears -- plus the static contexts of all the enclosing functions. The ___dynamic context___ of a binding consists of the run-time stack frames in force when the binding is consulted. The word _context_ by itself means _static context_. We may loosely say _context_ and mean only one set of bindings, not the entire tower.

+++

## Parameters and Non-Parameters

+++

In the body of a function, named or not, a variable is either a ___parameter___ or a ___non-parameter___. The body of the following contains parameter `x` and non-parameters `a`, `w`, `y`, and `z`:

```{code-cell} ipython3
foo = (lambda x: (a * x + y) / (w * z))
```

### $\lambda$-Bound, $\lambda$-Free

+++

In $\lambda$ calculus, parameters of a function are ___bound variables___ and non-parameters are ___free variables___. Because Python parlance differs, we'll say that parameters are ___$\lambda$-bound___ and non-parameters are ___$\lambda$-free___. The only $\lambda$-bound variable in `foo` above is `x`; `a`, `w`, `y`, and `z` are $\lambda$-free.

+++

A variable may be $\lambda$-free in one context $\lambda$-bound in another. For example, in

+++

`lambda w: lambda x: (a * x + y) / (w * z)`

+++

`w` is $\lambda$-bound in the outer context `lambda w: ...` and $\lambda$-free in the inner context `lambda a: ...`

+++

### P-Bound, P-Unbound, P-Global, P-Free

+++

In Python parlance, 

1. Parameters are ___P-bound___.

2. ___P-unbound:___ Non-parameters may be _P-closure-unbound_ and _P-instruction_global_, like `a`. Functions with P-unbound variables can be compiled -- Python will generate instructions to consult their bindings -- but they won't run unless and until all P-unbounds have bindings in some context. P-unbound variables are unbound: they don't have values.

3. ___P-global:___ Non-parameters may be _P-closure_global_ and _P-instruction_global_, like `z`. P-globals are bound: they have values.

4. ___P-free:___ Non-parameters may be _P-closure-nonlocal_ and _P-freevars-free_, like `w` and `y`. P-free variables are always $\lambda$-bound somewhere in their context. P-free variables are bound: they have values. This is the most confusing nomenclature.

+++

The following example motivates the adjectives by examining the disassembly instructions, the closure data, and the `co_freevars` attribute of the Python compiler's `__code__` attribute:

```{code-cell} ipython3
z = 43
def bar():
    y = 37
    foo = (lambda x: (a * x + y) / (w * z))
    import dis
    dis.dis(foo)
    import inspect
    from pprint import pprint
    pprint({"parameters(foo)": inspect.signature(foo).parameters})
    pprint({"ClosureVars(foo)": inspect.getclosurevars(foo)})
    pprint({"freevars(foo)": foo.__code__.co_freevars})
bar()
```

The following paragraphs explain the output above.

+++

### Parameters

+++

#### P-Bound

+++

Python reckons `x` a parameter via the `LOAD_FAST` instruction. The non-parameter categories do not pertain. `inspect.signature(...).parameters` corroborates this reckoning. Such variables are both _P-bound_ and bound: they have values.

+++

### Non-Parameters

+++

#### P-Unbound

+++

Python reckons `a`, `w`, and `z` _P-instruction-global_ via `LOAD_GLOBAL`. Because `inspect.getclosurevars(foo)` reveals that `a` and `w` are also P-closure-unbound, we'll get an error if we run the lambda. Unboundedness is the dominating property, so we call this category of variables simply _P-unbound_. They are also simply unbound: they don't have values in this context. 

+++

#### P-Global

+++

Python reckons `z` as P-instruction-global by `dis` and P-closure-global by `inspect.getclosurevars(foo)`. I believe that P-closure-global implies P-instruction-global, so we call this category just _P-global_. P-globals are bound: they have values.

+++

#### P-Free

+++

Python reckons `y` P-closure-nonlocal and P-freevars-free. I believe these are synonyms, so we call this category of variables _P-free_. P-frees are bound: they have values somewhere in their context. 

+++

## Change of Context

+++

Let's give a parameter `w` to the enclosing context `bar`. `w` becomes P-free in the inner context of `lambda x ...`; it was previously P-unbound.

```{code-cell} ipython3
z = 43
def bar(w):
    y = 37
    foo = (lambda x: (a * x + y) / (w * z))
    import dis
    dis.dis(foo)
    import inspect
    from pprint import pprint
    pprint({"parameters(foo)": inspect.signature(foo).parameters})
    pprint({"ClosureVars(foo)": inspect.getclosurevars(foo)})
    pprint({"freevars(foo)": foo.__code__.co_freevars})

bar(19)
```

### Enclosing $\lambda$

+++

We can't assign to -- and thus can't create -- local variables in an enclosing $\lambda$ expression. Assignment is a statement and statements are not allowed in Python $\lambda$ expressions. However, we can have parameters, which show up in the inner lambda as P-free.

+++

Replace the first outer `def bar` above with a $\lambda$ expression. `y`, once P-free within the inner context, defined by assignment in the outer context, becomes P-unbound. `w`, once P-unbound, becomes P-free. `z` remains P-global and `a` remains P-unbound.

```{code-cell} ipython3
z = 43
import dis, inspect
from pprint import pprint
(lambda w:
    dis.dis((lambda x: (a * x + y) / (w * z))))(19)
(lambda w:
    pprint({"ClosureVars(λ)": 
            inspect.getclosurevars(
                (lambda x: (a * x + y) / (w * z)))}))(19)
(lambda w:
    pprint({"freevars(λ)":
            (lambda x: (a * x + y) / (w * z)).__code__.co_freevars}))(19)
```

### Summary

+++

> $\lambda$-free variables are P-free iff they are non-local and not global, either defined in an outer context by assignment, or parameters of an outer context. 

> $\lambda$-free variables are P-global iff they are defined by assignment at top level and not shadowed by P-locals with the same names in deeper enclosing contexts. 

> $\lambda$-free variables are P-unbound iff they are not defined and are not parameters in any enclosing context. 

> $\lambda$-bound variables are P-bound. 

+++

### Analyzer

+++

We have enough tooling now to write a small Python library for analyzing functions. Please make sure you understand and accept the following results:

```{code-cell} ipython3
from types import FunctionType
from typing import Dict, List
def variable_analysis(f: FunctionType) -> Dict[str, List[str]]:
    p_bounds = list(inspect.signature(f).parameters.keys())
    closurevars = inspect.getclosurevars(f)
    p_frees = list(closurevars.nonlocals.keys())
    p_unbounds = list(closurevars.unbound)
    p_globals = list(closurevars.globals.keys())
    all_vars = p_bounds + p_frees + p_unbounds + p_globals
    assert len(set(all_vars)) == len(all_vars)
    result = {'p_bounds': p_bounds,
              'p_frees': p_frees,
              'p_unbounds': p_unbounds,
              'p_globals': p_globals,
              'all_vars(set)': set(all_vars),
              'all_vars(list)': all_vars,
             }
    return result
pprint((lambda w: 
 variable_analysis(lambda x: (a * x + y) / (w * z)))(19))
def bar(w):
    y = 37
    result = variable_analysis(lambda x: (a * x + y) / (w * z))
    return result
pprint(bar(19))
```

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

> A ___binding___ is an association from a variable name to a value, that is, an entry in a frame. 

+++

We might model a binding as a pair, or as a row in a table, an element of a relation (subset of a Cartesian product), an element of a Python dictionary, or as an attribute of a Python object. We prefer the attribute model because it affords _dot_ notation for lookup, that is, `o.foo` rather than the dictionay's syntax `o['foo']`.

+++

If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a virtual sequence of frames via the chain of pointers to enclosing environments.

+++

> The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`. 

+++

> A frame $\phi$ belongs to a virtual sequence of environments implied by the unidirectional pointer-chain of enclosing environments rooted in $\phi$ and ending at the unique global environment. The ___value of a variable in an environment___ is the value in the first binding in any frame of that sequence. Bindings lower in the chain may ___shadow___ bindings higher in the chain, rendering them inaccessible. If no frame in a chain specifies a binding for the variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another.

+++

User code should avoid Greek to avoid clobbering system stuff.

```{code-cell} ipython3
from dataclasses import dataclass, field
from types import FunctionType
@dataclass
class Environment:
    """Set attributes via settattr(env.ϕ, key, val). When getting
    attributes, it's ok to omit the ϕ."""
    ϕ: FunctionType   # "frame," also nice place to hang attributes via 'setattr'
    π: "Environment"  # via Greek πηρι, short name for 'enclosing'
    # Recursive lookup
    def __getattr__(self, key):
        try:
            result = getattr(self.ϕ, key)
        except AttributeError as _:
            if self.π is None:
                raise NameError(f'Name {key} is unbound.')
            else:  # recurse
                result = self.π.__getattr__(key)
        return result
# The ugliness of 'setattr' is hidden in DEFINE and DEFINE_PROC.
    
ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ
```

# Procedure

+++

A ___procedure___ is a pair of code and environment. ___Code___ is a dictionary of parameter names and a $\lambda$ expression. The $\lambda$, by convention, takes a sigle argument, the environment in which its formal parameters are bound via some application of the procedure, as described in SICP 3.2. This convention seems to be the best we can do for composable $\lambda$s in Schemulator. For now, we support only positional arguments, one-to-one with the argument list. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the application has too many or too few arguments.

```{code-cell} ipython3
from typing import Dict, Set
Parameters = List[str]  # positional arguments only
@dataclass
class Procedure:
    code: Dict
    π: Environment
```

Following the example for _square_ in SICP 3.2.1, let's define it in the global environment:

```{code-cell} ipython3
setattr(
    ΓΠ.ϕ, 
    "square", 
    Procedure(
        {"body": lambda π: π.x * π.x,  # ugly, I know; sorry :(
         "parameters": ['x']}, 
        ΓΠ))
```

Define the recursive lookup algorithm, raising an exception for unbound. Catch it if you care.

+++

## EVAL

+++

work in progress

```{code-cell} ipython3
def EVAL(expr: Any, π: Environment) -> Any:
    if isinstance(expr, int) or \
       isinstance(expr, float) or \
       isinstance(expr, str) or \
       isinstance(expr, bool):
        result = expr
    else:
        raise ValueError
    return result
```

Apply "square"

+++

## APPLY

```{code-cell} ipython3
class IllegalArgumentsError(ValueError):
    pass

def APPLY(proc: Procedure, args: List[Any], π:Environment) -> Any:
    if len(proc.code['parameters']) != len(args):
        raise IllegalArgumentsError(
            f"Wrong number of arguments passed to procedure {proc_str}")
    evaled_args = [EVAL(arg, π) for arg in args]
    E1 = Environment(lambda: None, π)
    for k, v in zip(proc.code['parameters'], evaled_args):
        setattr(E1.ϕ, k, v)
    result = proc.code['body'](E1)
    return result
    
APPLY(ΓΠ.square, [5], ΓΠ)
```

## DEFINE

+++

Package up the "defining" boilerplate.

```{code-cell} ipython3
def DEFINE(sym: str, val: Any, π: Environment) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
def DEFINE_PROC(sym: str, 
                parameters: List[str], 
                body: FunctionType, 
                π: Environment) -> None:
    """unofficial convenience (alternative syntax in Scheme)"""
    DEFINE(
        sym,
        Procedure(
            {"body": body, "parameters": parameters}, 
            π),
        π)
    
DEFINE_PROC('saxpy', ['a', 'x', 'y'],
            lambda π: π.a * π.x + π.y, 
            ΓΠ)  # ugly, i know; sorry :(

APPLY(ΓΠ.saxpy, [4, 10, 2], ΓΠ)
```

SICP 3.2.2

```{code-cell} ipython3
DEFINE_PROC('square', ['x'], 
            lambda π: π.x * π.x, 
            ΓΠ)

DEFINE_PROC('sum_of_squares', ['x', 'y'],
           lambda π: \
            APPLY(π.square, [π.x], π) + \
            APPLY(π.square, [π.y], π),
           ΓΠ)

DEFINE_PROC('f', ['a'],
           lambda π: \
            APPLY(π.sum_of_squares, 
                  [1 + π.a, 2 * π.a], π), 
                  ΓΠ)

APPLY(ΓΠ.f, [5], ΓΠ)
```

# Junkyard

+++

Everything below this line best ignored. It is saved for later but discarded for now.

+++

# LET_STAR

+++

Install bindings in a given environment, possibly destroying prior bindings with the same variable names. Return a function that expects to be called with that environment.

```{code-cell} ipython3
from typing import Sequence, Callable, Any, Union, List, Tuple
from pprint import pprint
class IllegalArgumentError(ValueError):
    pass
def LET_STAR(bindings: Union[List, Tuple], fn, e):
    if len(bindings) == 0:
        result = fn
    else:
        k, v = bindings[:2]
        if isinstance(v, Callable):
            v = v(e)
        setattr(e, k, v)
        result = LET_STAR(bindings[2:], fn, e)
    return result
_ge = lambda: None
setattr(_ge, 'z', 43)
pprint(LET_STAR(['y', 6, 'a', 29, 'w', 39, 'x', 17],
                lambda e: (e.a * e.x + e.y) / (e.w * e.z),
                _ge)(_ge))
_ge = lambda: None
setattr(_ge, 'z', 43)
pprint(LET_STAR(['y', 6, 'a', 29],
                LET_STAR(['w', 39, 'x', 17],
                         lambda e: (e.a * e.x + e.y) / (e.w * e.z),
                         _ge),
                _ge)(_ge))
_ge = lambda: None
setattr(_ge, 'z', 43)
pprint(LET_STAR(['y', 6, 'a', lambda e: e.y * e.y - 7],
                LET_STAR(['w', lambda e: e.a - 10, 'x', 17],
                         lambda e: (e.a * e.x + e.y) / (e.w * e.z),
                         _ge),
                _ge)(_ge))
```

# BLOCK

```{code-cell} ipython3
from typing import Sequence, Callable, Any, Union, List, Tuple
from pprint import pprint
class IllegalArgumentError(ValueError):
    pass
def BLOCK(stmts: Union[List[Callable], Tuple[Callable]], 
          e=lambda: None):
    """
    ex: LET_STAR(['x', 0],
             BLOCK([lambda e: setattr(e, 'x', 6), lambda e: e.x * (e.x + 1)]))
    gives: 42
    """
    if len(stmts) < 1:
        raise IllegalArgumentError("A BLOCK must have at least one statement.")

    results = [stmt(e) for stmt in stmts]
    return lambda e: results[-1]
BLOCK([lambda e: setattr(e, 'x', 6), lambda e: e.x * (e.x + 1)])(lambda: None)
```

The first schemulation is `LET*`, an expression that "defines" local variables by binding their values to parameters of nested $\lambda$ expressions. Later variables may refer to earlier ones in the nesting. 

+++

We want to write a function that wraps $\lambda$ expressions in other $\lambda$ expressions. The desired effect is for

+++

`LET_STAR(['y', 6, 'a', 29], lambda w: lambda x: (a * x + y) / (w * z))`

+++

to produce

+++

`(lambda y: (lambda a: lambda w: lambda x: (a * x + y) / (w * z))(29))(6)`

+++

We consider two implementation strategies. Neither is great, so we accept a compromise. 

+++

## Explicit Environments

+++

[divs1210](https://gist.github.com/divs1210/d218d4b747b08751b2a232260321cdeb) presents a method for composing $\lambda$s by inserting bindings via `setattr` into an explicit `env` $\lambda$. Every function must have an explicit `env` parameter. The method invites an explicit simulation of Scheme's _frames and environments_ from [SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2). The required depth of this simulation is not clear.

+++

We pursued this method at some length in [another development](#frames-and-environments) and found it oppressive. Because Python already has an adequate, if idiosyncratic, environment model (see [Bound and Free Variables](#bound-and-free) above), we choose that alternative, with its downsides.

+++

## Pythonic Environments

+++

We find that the best, though compromised, alternative is to operate on strings:

+++

`eval(LET_STAR(['y', 6, 'a', 29], 'lambda w: lambda x: (a * x + y) / (w * z)'))`

+++

This strategy is similar to macros in Lisp, just using strings instead of the AST. We might pursue the AST route in another development (see [macropy](https://github.com/lihaoyi/macropy), but we go with this compromise for now. 

+++

We arrived at the compromise after examining altenatives via `inspect` and `eval` inside the implementation of `LET_STAR`. Nothing was easy. Skip the following section if you don't care why not.

+++

### Eliminating Alternatives

+++

Whereas the notebook's `??` operator recovers the source for a $\lambda$ expression at top level:

```{code-cell} ipython3
??lambda w: lambda x: (a * x + y) / (w * z)
```

it's not so easy to do when the $\lambda$ is a function argument

```{code-cell} ipython3
def foo(fn: FunctionType):
    return ??fn
```

The standard `inspect` module is no help, as it seems to chase definitions outward:

```{code-cell} ipython3
import inspect
from typing import Any
def foo(noise_argument, fn: FunctionType):
    print(inspect.getsource(fn))
foo(Any, lambda w: lambda x: (a * x + y) / (w * z))
```

Worse, `eval`, anywhere in the function body, seems to percolate inward, prematurely evaluating the `fn` argument, making it impossible to compose functions. We elide the investigatory notes and invite the reader to refute us.

+++

## Implementation

+++



```{code-cell} ipython3
from typing import List, Sequence, Callable, Any
class IllegalArgumentError(ValueError):
    pass
def LET_STAR(bindings: Sequence, fn: str) -> str:
    if len(bindings) % 2 == 1:
        raise IllegalArgumentError("Bindings must have k, v pairs.")
    if not isinstance(fn, str):
        raise IllegalArgumentError("fn must be a string containing a λ expression.")

    if len(bindings) == 0:
        result = fn
    else:
        k, v = bindings[:2]
        result = f'(lambda {k}: {LET_STAR(bindings[2:], fn)})({v})'
    
    return result
eval(LET_STAR(['y', 6, 'a', 29], 'lambda w: lambda x: (a * x + y) / (w * z)'))(39)(17)
```

# COND

```{code-cell} ipython3
from typing import Union
def COND(clauses, fn: Callable) -> str:
    if len(clauses) %2 == 1:
        raise IllegalArgumentError("Clauses must be pred, fn pairs")
    pass
```

## Frames and Environments<a id="frames-and-environments"></a>

+++

See [SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) for Scheme's documentation on frames and environments.

+++

A ***frame*** is a lookup table -- a mathematical function -- from symbols to values. A symbol may appear no more than once in a frame. Symbols and values are not defined yet. Take them as primitive notions for now with their intuitive meanings.

+++

An element of a frame is a ***key-value pair***, also called a ***binding*** of the symbol. The term "_binding_" also pertains to the value of a parameter in a context where it represents a bound variable -- a function parameter that has an actual argument as a value. Don't conflate these two meanings of "_binding_."

+++

Logically, a frame is a Python dictionary, and that's a good model for it because we shall have to test for existence and values of keys. We considered attributes of lambdas, but they're difficult to chain.

+++

An ***environment*** is a sequence of frames. If a symbol appears in more than one frame in an environment, the symbol in the earlier frame ***shadows*** the symbol in a later frame. This structure supports ***lexical binding***.

+++

## Variable Bindings as Object Attributes

+++

Python's built-in `setattr` gives us a pleasing notation for variable bindings as attributes of $\lambda$ expressions. Consider:

```{code-cell} ipython3
import types

def obj_attribute():
    obj = lambda: None  # <~~~ We want lambdas, lots of them!
    if isinstance(obj, types.FunctionType):
        setattr(obj, 'foo', 42)
    return obj.foo  # <~~~ pretty
obj_attribute()
```

versus

```{code-cell} ipython3
def dict_attribute():
    dict = {}
    dict['foo'] = 42
    return dict['foo']  # <~~~ ugly
dict_attribute()
```

Because we work mostly with $\lambda$ expressions, we choose `obj_attribute` as a pattern, as suggested by [divs1210](https://gist.github.com/divs1210/d218d4b747b08751b2a232260321cdeb) for this idea. However, environments are sequences of frames, and we can't easily design a `LOOKUP` function that takes a symbol as a parameter and finds its binding in a sequence of frames. `frame.x` is a beautiful notation, but it doesn't extend to `env.x`.

+++

# Global Environment

+++

## SICP 1.1.2: Naming and the Environment

+++

The first step in representing Python in applicative-order $\lambda$ calculus is to have a global environment in which to install global names.

```{code-cell} ipython3
from collections import deque
G_FRM = {}  # would prefer lambda: None but it doesn't chain.
G_ENV = deque([G_FRM])  # for efficient appendleft
```

## DEFINE: SICP 3.2.1

+++

Quoting:

+++

> In the environment model of evaluation, a procedure is always a pair consisting of some code and a pointer to an environment. Procedures are created in one way only: by evaluating a λ-expression. This produces a procedure whose code is obtained from the text of the λ-expression and whose environment is the environment in which the λ-expression was evaluated to produce the procedure.

+++

Rather than implicit ambient environments, we make them explicit with the following convention:

+++

> Every $\lambda$ expression in the schemulator has an explicit environment (list of frames) in its last parameter slot. The environment defaults to the global environment

+++

Pursuing the example in SICP 3.2.1, consider the procedure definition

+++

```
(define (square x)
  (* x x))
```

```{code-cell} ipython3
from typing import Deque, Any, Union
from types import FunctionType
from copy import deepcopy
ENV = List[FunctionType]
```

```{code-cell} ipython3
def DEFINE(sym: str, 
           params: Union[None, List[str]],
           val: Any, 
           env: ENV) -> ENV:
    """Deposit definition in frame 0 of the env. Works for 
    local and global envs. Make new frame when defining 
    functions. If defining a function, params must be a list,
    even an empty one. If defining a symbol ...
    Raise exception on any error."""

    # If params are None, there is no new frame. If params
    # are empty
    if 
    params_frame = {}
    for param in params:
        params_frame[param] = None
    
    frame = env[0]
    if sym in frame:  # overwrite old definition
        old_env = frame[sym][1]
        _ = old_env.popleft()
        old_env.appendleft(new_frame)
        
    frame[sym] = val
    return env
```

```{code-cell} ipython3
DEFINE("square",
       ["x"],
       lambda env: LOOKUP('x', env) * LOOKUP(env, "x"),
       G_ENV)
```

```{code-cell} ipython3
def LOOKUP(env: ENV, sym: str) -> Any:
    for frame in env:
        if sym in frame:
            return frame[sym]
        else:
            raise KeyNotFound(f"{sym}")
```

```{code-cell} ipython3
G_FRM['square'](42)
```

From SICP 1.2.1, we can also define constants.

```{code-cell} ipython3
DEFINE("size",  # short alias for g_schemu_env
      2, G_ENV)
```

```{code-cell} ipython3
G_FRM.square(G_FRM.size)
```

## COND: SICP 1.1.6

+++

Thanks again to [divs1210](https://gist.github.com/divs1210/d218d4b747b08751b2a232260321cdeb) for inspiration.

```{code-cell} ipython3
DEFINE("_else", lambda env: True, G_ENV)

def COND(pred_val_pairs, _else=lambda env: None, env=_ge) -> FunctionType: 
    """Functional if-elif-...-else expression
    ex: COND([lambda env: 1 == 0, lambda env: 'a',
              lambda env: 2 == 0, lambda env: 'b',
              lambda env: 3 == 0, lambda env: 'c')
              _gf._else,          lambda env: 'd'])
    gives: 'd'.
    """
    if len(pred_val_pairs) == 0:
        return _else(env)

    pred, val = pred_val_pairs[:2]
    if pred(env):
        result = val(env)
    else:
        result = COND(pred_val_pairs[2:], _else, env)
    return result
```

```{code-cell} ipython3
COND([lambda env: 1 == 0, lambda env: 'a',
      lambda env: 2 == 0, lambda env: 'b',
      lambda env: 3 == 0, lambda env: 'c',
      G_FRM._else,        lambda env: 'd'])
```

Show that lambdas after the first True are not evaluated.

```{code-cell} ipython3
COND([lambda env: 1 == 0, lambda env: 'a',
      lambda env: 1 == 1, lambda env: 'b',
      lambda env: 1 == 1, lambda env: 1 / 0])
```

Example: `abs`. This is user-defined, so we won't put it in all caps.

```{code-cell} ipython3
DEFINE("abs",
       COND([lambda env: env.x <  0, lambda env: - env.x,
             lambda env: env.x == 0, lambda env: 0,
             lambda env: env.x >  0, lambda env: env.x]))
```

# Expressions

```{code-cell} ipython3

```
