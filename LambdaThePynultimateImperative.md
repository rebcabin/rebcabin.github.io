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

```{code-cell} ipython3

```
