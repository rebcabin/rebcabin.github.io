#!/usr/bin/env python
# coding: utf-8

# # Lambda, the Pynultimate Imperative

# #### Version 2
# #### Brian Beckman
# #### 12 Nov 2022

# # Introduction

# In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative constructs with just _lambda_:

# > We demonstrate how to model the following common [imperative] programming constructs in ... an applicative-order language similar to LISP: Simple Recursion, Iteration, Compound Statements and Expressions, GO TO and Assignment, Continuation-Passing, Escape Expressions, Fluid Variables, Call by Name, Call by Need, and Call by Reference. The models require only (possibly self-referent) lambda application, conditionals, and (rarely) assignment.

# It's useful to recap this paper in Python, which has most of the listed imperative constructs. Imagine compiling Python into an intermediate language in which the semantics, even those with side-effects, are laid bare as trees of $\lambda$ expressions. In such a representation, optimizations are
# 1. easy to write as tree-to-tree transforms
# 1. easy to extend via just function composition (even Kleisli-monadic)
# 2. independent of surface syntax, thus easy to share with other imperative languages like Fortran, C, Java
# 3. independent of back ends, thus easy to run interactively; or to translate into LLVM, x86, ARM64, C, for execution; or to transpile into other surface languages

# The use-cases above are similar to those for a SQL algebraizer. Many SQL implementations
# 1. translate the surface language into bare-bones expressions in a closed relational algebra, free of original syntax
# 2. run the algebraic expressions through symbolic optimizers, which often rearrange the expressions completely
# 2. incrementally extend the system by composing new optimizations
# 3. translate optimized expressions into commands for local and distributed servers

# We follow the paper more-or-less directly, with gleanings from [SICP](https://sarabander.github.io/sicp/).

# ## _Schemulation:_ Python Semantics in Python

# Ideally, we'd compile Python into Scheme or Clojure or Common Lisp, then write transformations, translations, interpreters, debuggers, etc. in Scheme or Clojure or Common Lisp. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in a Scheme-like applicative-order $\lambda$-calculus embedded in basic Python.

# Some people may find this weird. Why not just implement Scheme in Python instead of emulating Scheme in Python? It's an engineering judgment call. Most authors of compilers and interpreters spend undue time on syntax before even getting to semantics. Compiler textbooks tell you to do this! Semantics becomes a hidden "implementation detail," but then developers forgive themselves for making an ungodly mess of it. We prefer the other way around. Get the semantics clean, composable, extendable, maintainable, and correct, _first_, then spend time on syntax, if you even care any more. Or just let someone else do it. From the development point of view, syntax is a distraction. It's been solved for decades, but it makes young developers and professors feel powerful. The decision to spend time on syntax before semantics is not engineering, it's self-indulgence.

# ## Orthogonality as a Design Principle

# We prefer designs that minimize cross-talk. Each facility -- transformation layer or module -- should have the least possible dependency on other facilities. For example, tranformations that affect control flow need not necessarily depend on transformations that affect numerics.

# Contrast to a braided design, where each facility explicitly accounts for every other.

# # Environment and Frame

# We find it necessary to model Scheme's environments and frames explicitly. We tried multiple short-cut alternatives and found that none of them compose well.

# [SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) has some apparent contradictions in the definition of environment and frame. It says that "an environment is a sequence of frames," but the rest of the text and figures clearly imply that an environment has just one frame.

# The best resolution appears to be:

# > An ___environment___ is a frame $\phi$ and a pointer $\pi$ to an enclosing environment. A ___frame___ is a mathematical function from variable names to values; no variable name may appear more than once in a frame.

# We note in passing that this works only for a single thread. [Clojure, for instance, solves that problem with _Vars_](https://clojure.org/reference/vars).

# ## Greek and ALL CAPS<a id="greek"></a>

# $\pi$ is a nice pun: it evokes $\pi\eta\rho\iota$, a Greek prefix meaning "surrounding."

# We use Greek letters for system attributes. User code should avoid Greek to avoid clobbering system attributes.

# We use ALL CAPS for system functions that implement Schemulator.

# ## Bindings

# > A ___binding___ is an association from a variable name to a value, that is, an entry in a frame.

# We might model a binding as a pair, or as a row in a table, an element of a relation (subset of a Cartesian product), an element of a Python dictionary, or as an attribute of a Python object. We prefer the attribute model because it affords _dot_ notation for lookup, that is, `o.foo` rather than the dictionary's syntax `o['foo']`.

# If the definitions above are acceptable, the apparent contradiction in SICP is resolved. SICP says that an environment _is_ a sequence of frames. Rather, I'd say that any environment _implies_ a virtual sequence of frames via the chain of $\pi\eta\rho\iota$ pointers to enclosing environments.

# > The system maintains a unique ___global environment___, whose _pointer-to-enclosing-environment_ is `None`.

# > A frame $\phi$ belongs to a virtual sequence of environments implied by the unidirectional pointer-chain of enclosing environments rooted in $\phi$ and ending at the unique global environment. The ___value of a variable in an environment___ is the value in the first binding in any frame of that sequence. Bindings lower in the chain may ___shadow___ bindings higher in the chain, rendering them inaccessible. If no frame in a chain specifies a binding for the variable, then the variable is ___unbound___ in the environment. A variable may be ___bound___ in one environment and unbound in another.

# Thanks to [divs1210](https://gist.github.com/divs1210?page=3) for the idea of modeling frames as dummy lambda functions, and assigning attributes to them.

# We override `__getattr__` to avoid a separate function for recursive lookup. We can't also override `__setaddr__` to call `setattr(self.$\phi$ ...)` because `self.$\phi$` diverges on `getattr(self.$\phi$ ...)`.

# There is a unique global environment, $\Gamma\Pi$, for each session.

# TODO: Coalesce $\Gamma\Pi$ with Python's global environment.

# In[1]:


from dataclasses import dataclass, field
from types import FunctionType


@dataclass
class Environment:
    """Set attributes via settattr(env.ϕ, key, val). When getting
    attributes, it's ok to omit the ϕ because of overloaded
    __getattr__."""
    ϕ: FunctionType  # "frame," also nice place to hang attributes via 'setattr'
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
# The ugliness of 'setattr' is hidden in DEFINE.

ΓΠ = Environment(lambda: None, None)  # Γ for "global," Π for "environment"
setattr(ΓΠ.ϕ, 'γόὂ', 43)
ΓΠ.γόὂ

# # Procedure

# A ___procedure___ is a pair of code and environment. ___Code___ is a dictionary of parameter names and a $\lambda$ expression. The $\lambda$, by convention, takes a sigle argument, $\pi$, the environment in which its formal parameters are bound to actual arguments via procedure application, as described in SICP 3.2. This convention seems to be the best we can do for composable $\lambda$s in Schemulator. For now, we support only positional arguments, one-to-one with the argument list. That's consistent with [Gambit Scheme](https://github.com/gambit/gambit), which reports "Wrong number of arguments ..." if the application has too many or too few arguments.

# By default, procedures are defined in the unique global environment, $\Gamma\Pi$.

# In[2]:


from typing import Dict, List, Tuple

Parameters = List[str]  # positional arguments only


def APPLY(proc, args, π=ΓΠ):
    """forward reference; Will be redefined."""
    print(args)


@dataclass
class Procedure:
    code: Dict  # TODO: Check for duplicated symbols in `parameters`.
    π: Environment = ΓΠ  # Defined in global environment by default.

    def __call__(self, *args):
        """experimental"""
        return APPLY(self, args, self.π)


# Following the example for _square_ in SICP 3.2.1, let's define it in the global environment and test the invocation of `APPLY`:

# In[3]:


setattr(
    ΓΠ.ϕ,
    "square",
    Procedure(
        {"body": lambda π: π.x * π.x,  # ugly, I know; sorry :(
         "parameters": ['x']}))
ΓΠ.square(5)
ΓΠ.square(5, 6)


# ## $\Lambda$

# Some syntactical help for anonymous procedures:

# In[4]:


def Λ(body, parameters, π=ΓΠ):
    result = Procedure(
        code={"body": body, "parameters": parameters},
        π=π)
    return result


# In[5]:


setattr(
    ΓΠ.ϕ,
    "square",
    Λ(lambda π: π.x * π.x, ['x']))
ΓΠ.square(5)
ΓΠ.square(5, 6)

# ## EVAL

# work in progress

# In[69]:


from typing import Any, Dict, Tuple, List


def EVAL(expr: Any, π: Environment = ΓΠ) -> Any:
    """Python does almost all this for us."""
    if isinstance(expr, int) or isinstance(expr, float) or isinstance(expr, str) or isinstance(expr, bool):
        result = expr
    elif isinstance(expr, Procedure):
        result = expr
    elif isinstance(expr, Dict):
        # for memo tables:
        result = expr
    elif isinstance(expr, Tuple):
        # for memoized, curried functions
        result = expr
    elif expr is None:
        result = None
    else:
        raise ValueError(f'{expr} has unknown type {type(expr)}')
    return result


# ## APPLY

# By default, procedures are applied in the unique global environment, $\Gamma\Pi$.

# In[7]:


class IllegalArgumentsError(ValueError):
    pass


def APPLY(proc: Procedure, args: List[Any], π: Environment = ΓΠ) -> Any:
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

# Call via "Pythonic" round brackets:

# In[8]:


ΓΠ.square(5)

# Works on anonymous procedures, too:

# In[9]:


Λ(lambda π: π.x * π.x, ['x'])(5)


# ## DEFINE

# Package up the "defining" boilerplate.

# By default, symbols are defined in the unique global environment, $\Gamma\Pi$.

# In[10]:


def DEFINE(sym: str, val: Any, π: Environment = ΓΠ) -> None:
    """official Scheme"""
    setattr(π.ϕ, sym, val)
    return val


DEFINE('saxpy',
       Λ(lambda π: π.a * π.x + π.y, ['a', 'x', 'y']))

ΓΠ.saxpy(4, 10, 2)

# ### SICP 3.2.2

# In[11]:


DEFINE('square',
       Λ(lambda π: π.x * π.x, ['x']))

DEFINE('sum_of_squares',
       Λ(lambda π: π.square(π.x) + π.square(π.y), ['x', 'y']))

DEFINE('f',
       Λ(lambda π: π.sum_of_squares(1 + π.a, 2 * π.a), ['a']))

ΓΠ.f(5)

# ### Exercise 3.9

# In[12]:


DEFINE('factorial',
       Λ(lambda π: 1 if π.n < 2 else \
           π.n * π.factorial(π.n - 1), ['n']))

ΓΠ.factorial(6)

# This doesn't tail-recurse because Python does not tail-recurse. See [Tail Recursion](#tail-recursion) for mitigation work-in-progress.

# In[13]:


DEFINE('fact_iter',
       Λ(lambda π: π.product if π.counter > π.max_count else \
           π.fact_iter(
               π.counter * π.product,
               π.counter + 1,
               π.max_count
           ), ['product', 'counter', 'max_count']))

ΓΠ.fact_iter(1, 1, 6)

# ## Procedures that Apply Procedures

# In[14]:


Λ(lambda π: π.f(π.x), ['f', 'x'])(ΓΠ.square, 42)

# Anonymously; shadowing is no problem, here:

# In[15]:


Λ(lambda π: Λ(lambda π: π.n * π.n, ['n'], π)(π.n), ['n'])(42)

# But you had better include the non-default $\pi$ on the inner if you don't want accidentally the correct answer.

# In[16]:


Λ(lambda π: Λ(lambda π: π.x * π.n, ['x'], π)(π.n), ['n'])(42)

# ## Procedures that Return Procedures

# In[17]:


Λ(lambda π: π.f, ['f'])(ΓΠ.square)(42)

# Anonymously:

# In[18]:


Λ(lambda π: π.f, ['f'])(Λ(lambda π: π.x * π.x, ['x']))(42)

# ## $\Upsilon$: Squaring Square Roots of Functions

# [See this other noteobook](https://github.com/rebcabin/rebcabin.github.io/blob/main/PythonYCombinators.md). We follow its derivations step-by-step from first principles.

# The running example is recursive factorial.

# Don't forget non-default $\pi$ on the inner lest `sf` be undefined.

# In[19]:


Λ(lambda π:
  Λ(lambda π: 1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1), ['n'], π), ['sf'])(
    Λ(lambda π:
      Λ(lambda π: 1 if π.n < 1 else π.n * π.sf(π.sf)(π.n - 1), ['n'], π), ['sf']))(6)

# Abstract `sf(sf)(m)` into a delayed $\lambda$ of `m`:

# In[20]:


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

# Abstract the _domain code_ into `d`, a function of `f`, the _business code_, is a function of `n`, the _business parameter_.

# In[21]:


Λ(lambda π:  # of d
  Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
    ['sf'], π)(
      Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
        ['sf'], π)),
  ['d'])(
    Λ(lambda π:  # of f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1), ['n'], π),
      ['f'])
)(6)

# Abstract the self-applied $\lambda$ of `sf` into `g`:

# In[22]:


Λ(lambda π:  # function of domain code, d
  Λ(lambda π: π.g(π.g), ['g'], π)(
      Λ(lambda π: π.d(Λ(lambda π: π.sf(π.sf)(π.m), ['m'], π)),
        ['sf'], π)),
  ['d'])(
    Λ(lambda π:  # domain code; function of business code, f
      Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1),
        ['n'], π),  # business parameter, n
      ['f'])  # recursive function
)(6)

# ## Recursive Factorial via $\Upsilon{}1$

# The glyph that looks like "Y" is actually capital Upsilon ($\Upsilon$ in $\LaTeX$). Names in user code should not collide with it if users remember that they should [avoid Greek in user code](#greek).

# Package into a system function, $\Upsilon{}1$, for later use. The "1" in the name means that this is for domain codes that return business codes of one parameter:

# In[23]:


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
         Λ(lambda π: 1 if π.n < 1 else π.n * π.f(π.n - 1),
           ['n'], π),  # business parameter, n
         ['f']))  # recursive function

ΓΠ.Υ1(ΓΠ.fact_recursive)(6)

# ## Iterative Factorial via $\Upsilon{}3$

# $\Upsilon$ must be tailored for a given number of business parameters. This one is for three. We could write a Python-AST hack to handle any number of business parameters, but that's Python macrology, a rabbit hole to sidestep for now (TODO: reconsider).

# In[24]:


# λ d: (λ g: g[g])(λ sf: d[λ m, c, x: sf[sf][m, c, x]])
DEFINE('Υ3',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of three parameters
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m, π.c, π.x),
                   ['m', 'c', 'x'], π)),
               ['sf'], π)),
         ['d']));

# Here is user-level domain code, redefining `fact_iter` in domain-code form. Any domain code is a function of `f`, recursive _business code_. In this case, `f` is a function of 3 business parameters. This will get us to a tail-recursive solution in the [section on tail recursion](#tail-recursion).

# In[25]:


# λ f: λ m, c, x: m if c > x else f(m*c, c+1, x)
DEFINE('fact_iter',  # domain code is a function of f ...
       Λ(lambda π:  # ... which is business code.
         Λ(lambda π: π.m if π.c > π.x else
         π.f(π.m * π.c, π.c + 1, π.x),  # business code
           ['m', 'c', 'x'], π),  # business parameters
         ['f']));

# In[26]:


ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 6)


# # Tail Recursion<a id="tail-recursion"></a>

# Thanks to [Thomas Baruchel for this idea on tail recursion](https://stackoverflow.com/questions/13591970/does-python-optimize-tail-recursion). If users are aware that their domain code is tail-recursive, then they may call it via `LOOP` instead of via $\Upsilon$. In Scheme, detection of tail recursion is automatic. In Python and Schemulator, users must manage tail recursion explicitly. This isn't a terrible issue. Tail-calls are lexically obvious, so users should always know. In Clojure, there is precedent. Users must explicitly write `loop` and `recur`. In any event, domain code can always be called via the proper $\Upsilon$, the one that knows the count of business parameters.

# We imitate Clojure's names `loop` and `recur`. The glyph that looks like "P" below is Greek Capital Rho for "recur." `LOOP3` has the same signature as $\Upsilon3$; it takes domain code as its sole argument. Names in user code should not collide with P B if users remember that they should [avoid Greek in user code](#greek). As with $\Upsilon$, Rho and `LOOP` must know their argument counts. That's OK for now (TODO: reconsider).

# In[27]:


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

    result = Λ(lambda π: looper(π.m, π.c, π.x), ['m', 'c', 'x'], π=d.π)
    return result


LOOP3(ΓΠ.fact_iter)(1, 1, 20)

# > ***Results are undefined if you call any `LOOP` function with non-tail-recursive domain code.***

# ## Prove It

# The recursive version blows Python's recursion limit.

# In[28]:


try:
    print(ΓΠ.Υ3(ΓΠ.fact_iter)(1, 1, 3))  # smallest that blows the limit
except RecursionError as e:
    print(e.args)

# The tail-call version does not. Notice the domain code `fact_iter` is EXACTLY the same as in the recursive version above.

# In[29]:


try:
    print(LOOP3(ΓΠ.fact_iter)(1, 1, 3))
except RecursionError as e:
    print(e.args)

# ## Tail-Recursive Fibonacci<a id="tail-recursive-fibonacci"></a>

# Write domain code for catastropically slow, non-tail-recursive, exponentially diverging Fibonacci:

# In[30]:


DEFINE('fib_slow',
       Λ(lambda π:
         Λ(lambda π: 1 if π.n < 2 else
         π.f(π.n - 1) + π.f(π.n - 2), ['n'], π),
         ['f']))

ΓΠ.Υ1(ΓΠ.fib_slow)(6)

# This is miserable even for $n=23$. You won't want to call it for bigger arguments.

# In[31]:


ΓΠ.Υ1(ΓΠ.fib_slow)(3)

# In[32]:


#from timeit import timeit
#timeit('ΓΠ.Υ1(ΓΠ.fib_slow)(23)', 'from LambdaThePynultimateImperative002 import ΓΠ')

# Without linearization, Fibonacci 500 would not complete in $10^{30}$ times the Age of the Universe. One way to linearize is tail recursion. Another way is [memoization](#memoization) (not _memorization_). Tail-recursive memoization is possible but not necessary. A tail-recursive Fibonacci easy and blazingly fast:

# In[33]:


DEFINE('fib_iter',
       Λ(lambda π:
         Λ(lambda π: π.b if π.n < 1 else
         π.f(π.b, π.a + π.b, π.n - 1),
           ['a', 'b', 'n'], π),
         ['f']));

# Check it:

# In[34]:


LOOP3(ΓΠ.fib_iter)(0, 1, 23)

# Time it:

# In[35]:


# timeit(LOOP3(ΓΠ.fib_iter)(0, 1, 23))

# 4000 times faster. Stress it:

# In[36]:


LOOP3(ΓΠ.fib_iter)(0, 1, 5000)

# # Memoized [sic] Fibonacci<a id="memoization"></a>

# ## Curried Memo Table

# One way to pass a memo table is through a Curried second argument. We'll need $\Upsilon2C$, generic for 2-parameter, Curried business code:

# In[37]:


DEFINE('Υ2C',
       Λ(lambda π:  # function of domain code, d ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # with business code of 2 parameters, curried
             Λ(lambda π:
               π.d(Λ(lambda π:
                     Λ(lambda π: π.sf(π.sf)(π.m)(π.n),
                       ['n'], π), ['m'], π)),
               ['sf'], π)),
         ['d']));

# The domain code for a memoized, Curried Fibonacci follows. The parameter `a` is the _accumulator_, _associator_, or memo, whatever word you like best. This is easiest to read (and to write) from the bottom up. It looks horrendous, but it isn't really.

# In[38]:


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
ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1]

# 1,000 times faster

# In[39]:


# timeit(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(23)[1])

# But still blows the recursion limit:

# In[40]:


try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(185)[1])
except RecursionError as e:
    print(e.args)

try:
    print(ΓΠ.Υ2C(ΓΠ.fib_fast)({})(184)[1])
except RecursionError as e:
    print(e.args)

# ## Memo Table as Business Parameter

# Currying is useful in general, but complicates this case. Get rid of it.

# In[41]:


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
         ['f']));  # domain-code signature

# Need $\Upsilon2$ to call it:

# In[42]:


DEFINE('Υ2',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of two parameters
             Λ(lambda π:
               π.d(Λ(lambda π: π.sf(π.sf)(π.m, π.c),
                     ['m', 'c'], π)),
               ['sf'], π)),
         ['d']));

# The recursion limit is a little higher, but we don't want any of that.

# In[43]:


try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 246)[1])
except RecursionError as e:
    print(e.args)

try:
    print(ΓΠ.Υ2(ΓΠ.fib_fast_uncurried)({}, 245)[1])
except RecursionError as e:
    print(e.args)

# ## Recursive With Memo

# Section [Tail-Recursive Fibonacci](#tail-recursive-fibonacci) exhibits a very short solution without a memo table. Could we write a tail-recursive version with a memo table? Is it worth the effort? Perhaps as a mental exercise.

# Pseudocode:

# ```
# f(r1, r2, a, n, x):
#     (a, 1) if n < 1 else
#     f(r2, r1 + r2, a | {x - (n-1): r1, x - (n-2): r2}, x)
# ```

# ### Non-Tail-Recursive

# In[44]:


DEFINE('Υ5',
       Λ(lambda π:  # of d, the domain code ...
         Λ(lambda π: π.g(π.g), ['g'], π)(
             # of business code of five parameters
             Λ(lambda π: π.d(
                 Λ(lambda π: π.sf(π.sf)(π.m, π.c, π.x, π.a, π.b),
                   ['m', 'c', 'x', 'a', 'b'], π)),
               ['sf'], π)),
         ['d']));

# In[45]:


DEFINE('fib_tc_memo',
       Λ(lambda π:
         Λ(lambda π:
           (π.a | {π.x: π.r2}, π.r2) if π.n < 1 else \
               π.f(π.r2, π.r1 + π.r2,
                   π.a | {π.x - π.n: π.r2},
                   π.n - 1,
                   π.x),
           ['r1', 'r2', 'a', 'n', 'x'], π),
         ['f']));

# In[46]:


ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)


# ### Tail-Recusive

# In[47]:


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

    result = Λ(lambda π:
               looper(π.m, π.c, π.x, π.a, π.b),
               ['m', 'c', 'x', 'a', 'b'], π=d.π)
    return result


# In[48]:


LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 23, 23)

# ### Test the Limits

# In[49]:


try:
    print(ΓΠ.Υ5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)

try:
    print(LOOP5(ΓΠ.fib_tc_memo)(0, 1, {}, 500, 500)[1])
except RecursionError as e:
    print(e.args)


# # DO

# ```
# (DO ((<var1> <init1> <step1>)
#      (<var2> <init2> <step2>)
#      ...
#      (<varN> <initN> <stepN>))
#     (<pred> <value>)
#     <optional body>)
# ```

# # LABELS

# # SET_BANG

# Tested below in [block](#block). Like Gambit and unlike Common Lisp and Emacs Lisp, we can only `set!` symbols that are already `define`d.

# In[101]:


def SET_BANG(sym: str, val: Any, π: Environment) -> None:
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


# # BLOCK / BEGIN<a id="block"></a>

# Sequencing of statements and expressions is not fundamental. Instead, we must chain $\lambda$s. The paper calls the form `BLOCK`. Scheme calls it `BEGIN`. Common Lisp calls it `PROGN`. `ανδρϵικϵλο` means "dummy." It's Greek so user symbols won't clobber it.

# First, following the paper, we define a block of two statements.

# In[112]:


def BLOCK2(s1: Procedure, s2: Procedure) -> Any:
    """Sequentially execute two statement-expressions."""
    s2(s1(_))


BEGIN2 = BLOCK2

# Λ(lambda π: ASSIGN('x', 6, π), ['ανδϵικϵλο'])(_)
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


# In[115]:


def BLOCK(*ss: List[Procedure]) -> Any:
    intermediate = None
    for s in ss:
        if intermediate is not None:
            intermediate = s(intermediate(_))
        intermediate = s(_)


DEFINE('x', 0)
DEFINE('y', 42)
BLOCK(
    Λ(lambda π: SET_BANG('x', 6, π), ['_']),
    Λ(lambda π: SET_BANG('x', π.x * 7, π), ['_']),
    Λ(lambda π: print(π.x * π.y), ['_']))


# # LET*, LET, LETREC

# `LET_STAR` is sequential binding of locals. `LET` is parallel binding. `LETREC` is mutually recursive `LET`.

# In[ ]:


def LET():
    pass

# # COND

# # Junkyard

# Ignore everything below. It's saved in case we need it someday.
