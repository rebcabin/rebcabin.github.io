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
#### 5 Nov 2022

+++

# Introduction

+++

In a classic paper, [_Lambda, the Ultimate Imperative_](https://www.researchgate.net/publication/37596655_Lambda_The_Ultimate_Imperative), Steele and Sussman show how to model most imperative control structures with just _lambda_:

+++

> We demonstrate how to model the following common programming constructs in terms of an applicative order language similar to LISP: Simple Recursion, Iteration, Compound Statements and Expressions, GO TO and Assignment, Continuation-Passing, Escape Expressions, Fluid Variables, Call by Name, Call by Need, and Call by Reference. The models require only (possibly self-referent) lambda application, conditionals, and (rarely) assignment. No complex data structures such as stacks are used. The models are transparent, involving only local syntactic transformations. This paper is partly tutorial in intent, gathering all the models together for purposes of context.

+++

It's useful to recapitulate this development in Python. Imagine compiling Python into an intermediate language in which the semantics, even those with side-effects, are laid bare as trees of lambda expressions. In such a representation, optimizations are 
1. easy to write as tree-to-tree transforms
2. independent of syntactical details of the surface language, thus easy to share with other surface languages like Fortran, C, Java
3. independent of back ends, thus easy to run in an interactive debugger; or to translate into LLVM, x86, ARM64, C, for execution; or to transpile into other surface languages

+++

The use-cases above are similar to those for a SQL algebraizer. Many SQL implementations 
1. translate the surface language into bare-bones expressions in the relational algebra, free of syntactical concerns
2. run the algebraic expressions through a symbolic optimizer, which often rearranges them completely
3. translate the optimized expressions into commands for a hardware back-end, or a distributed back-end, or other mechanization

+++

We follow the paper more-or-less directly, with gleanings from [SICP](https://sarabander.github.io/sicp/).

+++

## _Schemulation:_ Emulating Python in Python

+++

Ideally, we'd compile Python into Scheme or Clojure, then write our transformers etc. in Scheme or Clojure. However, to maintain a convenient notebook structure and to avoid creeping dependencies, we'll just model Python imperatives in Python. We'll build a Scheme-like applicative-order lambda calculus in basic Python so that we can follow Steele and Sussman's paper. We may introduce some normal-order evaluation as discussed in SICP 1.1.5 ff.

+++

# Definitions

+++

## Functions and Lambda Expressions

+++

Functions can be _named_ or _anonymous_. A named function is usually introduced Python with a `def`, as follows:

```{code-cell} ipython3
def foo(x):
    result = x * (x + 1)
    return result
```

We have the habit of defining local variables for results and then returning the values of those variables, as an alternative to direct returns:

```{code-cell} ipython3
def foo(x):
    return x * (x + 1)
```

This gives us a convenient place to hang a debugger breakpoint. Some debuggers are a bit shy about reporting return values.

+++

Invoke named functions as follows:

```{code-cell} ipython3
foo(6)
```

Anonymous functions are introduced via lambda expressions, as follows:

```{code-cell} ipython3
(lambda x: x * (x + 1))
```

and invoked as follows:

```{code-cell} ipython3
(lambda x: x * (x + 1))(6)
```

We can also give names to functions by assigning lambda expressions to Python variables:

```{code-cell} ipython3
foo = (lambda x: x * (x + 1))
foo(6)
```

Python lambda expressions may contain exactly one Python _expression_ in their bodies. We cannot assign a return value to a local variable in a lambda expression as we can with named functions. An assignment is a Python _statement_, not an expression. We solve this problem later with a `BLOCK` expression that lets us sequence multiple expressions in the schemulation. Until then, lambda expressions are difficult to debug. 

+++

### Use Cases

+++

In the ***object language*** of the schemulator, that is, the language being schemulated, we only use lambda expressions, no named functions. In the ***implementation language***, Python, we use both lambda expressions and named functions, plus any other features of Python we want. 

+++

## Bound and Free Variables

+++

The bodies of Python's lambda expressions can refer to parameters and to non-parameters. Non-parameters are ***free variables***. In Python, they're looked up in a lexical environment rooted at top-level. Example: In the body of the innermost lambda in the following example, `x` is a parameter or bound variable, and `foo` is a free variable. In the body of the outermost lambda, `foo` is a bound variable. A given symbol may stand for a free variable in one place and for a bound variable in another place.

```{code-cell} ipython3
(lambda foo: 
 (lambda x: foo * x)(foo + 1))(6)
```

We need to model that lookup in the schemulator.

+++

## Frames and Environments

+++

See [SICP 3.2](https://sarabander.github.io/sicp/html/3_002e2.xhtml#g_t3_002e2) for Scheme's documentation on frames and environments.

+++

A ***frame*** is a lookup table -- a mathematical function -- from symbols to values. Symbols and values are not defined yet. Take them as primitive notions for now with their intuitive meanings.

+++

A symbol may appear no more than once in a frame.  An element of a frame is a ***key-value pair***, also called a ***binding*** of the symbol. The term "_binding_" also pertains to the value of a parameter in a context where it represents a bound variable -- a function parameter that has an actual argument as a value. Don't conflate these two meanings of "_binding_."

+++

Logically, a frame is a Python dictionary. However, we choose a different implementation, as explained below.

+++

An ***environment*** is a sequence of frames. If a symbol appears in more than one frame in an environment, the symbol in the earlier frame ***shadows*** the symbol in a later frame. This structure supports ***lexical binding***.

+++

## Variable Bindings as Object Attributes

+++

Python's built-in `setattr` gives us a pleasing notation for variable bindings as attributes of lambda expressions. Consider:

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

Because we work mostly with lambda expressions, we chose `obj_attribute` as a pattern. Thanks to [divs1210](https://gist.github.com/divs1210/d218d4b747b08751b2a232260321cdeb) for this idea.

+++

# Global Environment

+++

## SICP 1.1.2: Naming and the Environment

+++

The first challenge in "compiling" Python into applicative-order is to have a global environment in which to install global names. 

```{code-cell} ipython3
g_schemu_frame = lambda: None
g_schemu_env = [g_schemu_frame]
```

## SICP 3.2.1

+++

Quoting:

+++

> In the environment model of evaluation, a procedure is always a pair consisting of some code and a pointer to an environment. Procedures are created in one way only: by evaluating a λ-expression. This produces a procedure whose code is obtained from the text of the λ-expression and whose environment is the environment in which the λ-expression was evaluated to produce the procedure. 

+++

Rather than implicit ambient environments, we make them explicit with the following convention:

+++

> Every lambda expression has an explicit environment (list of frames) in its last parameter slot)

+++

Pursuing the example in SICP 3.2.1, consider the procedure definition

+++

```
(define (square x)
  (* x x))
```

```{code-cell} ipython3
(lambda x, env: x * x)
```

# Expressions

```{code-cell} ipython3

```
