+++
title = "LISP 模式"
date = 2025-03-18

[extra]
comment = true
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "计算机", "离散", "抽象"]
+++

<style>
  code::before, code::after {
    content: "" !important;
  }

  p code, li code {
    text-decoration: 3px gold underline;
  }
</style>

## 说明
在 1960 年，John McCarthy 提出了一个划时代的理论。其对编程的贡献有如欧几里德对几何的贡献。[^1]

本文参考了[其原始论文](https://www-formal.stanford.edu/jmc/recursive.pdf)。

部分参考了 [Paul Graham](http://www.paulgraham.com/) 所作 [The Roots of Lisp](https://blog.freecloud.dev/img/the-roots-of-lisp.pdf)。找到了其一个 2003 年的译本 [Lisp 之根源](http://daiyuwen.freeshell.org/gb/rol/roots_of_lisp.html)，但该翻译多有谬误。

## Lambda 演算
让我们从 Alonzo Church 发明的 `Lambda Calculus` 开始。[^2]

本部分参考了[一篇 2014 年的文章](https://liujiacai.net/blog/2014/10/12/lambda-calculus-introduction/)以减少材料组织的工作。其按 [CC BY-NC-ND 4.0](https://creativecommons.org/licenses/by-nc-nd/4.0/) 许可。

### 语法
λ演算的语法形式极其简单。一种可理解的形式文法如下：
```c
E = x           // variables
  | λx. E       // function creation (abstraction)
  | E1 E2       // function application
```

上面的 $E$ 称为 λ-表达式或 `λ-terms`，它的值有三种形式：
* 变量。
* 函数声明或抽象。函数**有且仅有**一个参数。在 $\lambda x.\ E$ 中，$x$ 是参数，$E$ 是函数体。
* 函数应用。即函数调用。

一些简单的例子：
* 恒等函数 $\lambda x.\ x$
* 返回恒等函数的函数 $\lambda y.\ (\lambda x.\ x)$ 这里的 $y$ 参数被忽略了。

在书写时，使用的惯例是：
* 函数声明时，函数体尽可能向右扩展。
* 函数应用时，遵循左结合。

例如，$\lambda x.\ x\ \lambda y.\ x\ y\ z$ 应被理解为 $\lambda x.\ (x\ \lambda y.\ ((x\ y)\ z))$。

### Currying
尽管在定义中，函数必须恰有一个参数，多个参数的函数仍然可以通过 `currying` 技术间接地表示。

例如，不合法的 $\lambda x\ y.\ x+y$ 可以被表达为 $\lambda x.\ (\lambda y.\ x+y)$。后世的闭包思想也来自于此。

巧妙的是，你可以传入少于全部参数个数的参数。例如代入 3，可以得到 $\lambda y.\ 3+y$。

### 求值
有两条求值规则：
* `Alpha equivalence` (or conversion)\
  α-重命名意为，可以任意改变变量名。如有歧义的 $\lambda x.\ x(\lambda x.\ x)$ 可改为 $\lambda x.\ x(\lambda y.\ y)$。
* `Beta reduction`\
  β-规约意为，在应用时将声明展开。例如 $(\lambda x.\ x)(\lambda y.\ y)$ 被展开为 $\lambda y.\ y$。

### 求值顺序
考虑函数应用 $(\lambda y.\ (\lambda x.\ x)\ y) E$。它有两种计算方法：
* 先求内层，得到 $(\lambda y.\ y) E$
* 先求外层，得到 $(\lambda x.\ x) E$

根据 Church–Rosser 定理，这两种方法是等价的，最终会得到相等的结果。

但我们在计算时必须作出选择。因而产生了两种方式。
* `Call by Value`；`Eager Evaluation` 紧迫求值：在函数应用前，就计算函数参数的值。
* `Call by Name`；`Lazy Evaluation` 懒惰求值：在函数应用前，不计算函数参数的值，直到需要时才求值。

### 数据
λ演算中只有函数而没有纯粹的 `boolean`，`number`，`list` 等实践中关心的类型。不过我们可以用函数来间接的表达它们。

#### Boolean
注意到，其最核心的功能是条件判断。简单地定义 `true` 为 $\lambda x.\ \lambda y.\ x$，`false` 为 $\lambda x.\ \lambda y.\ y$。

这样，`if e then u else v` 就可被重写为 $e\ u\ v$。

#### Natural Number
见后文[数字](#lambda-natural-number)一节。

## Lisp 之根源
### S-表达式
一个**表达式**（**expression**）可以是：
- 一个**原子**（**atom**），在这里是一个连续的字母序列，如 `foo`。
- 一个**表**（**list**），表达方式形如 `()`，`(foo)`，`(foo bar)`，其中含有零个、一个、两个或多个表达式。

可见 `(foo (bar) baz)` 是表达式。

在 Lisp 中，合法的表达式有**值**（**value**），求值往往是默认进行的。[^evaluate]

### 原始操作符
只有七个原始操作符：`quote`，`atom`，`eq`，`car`，`cdr`，`cons` 和 `cond`。

`(quote x)` 会返回未求值的 `x`，整个 `(quote x)` 也被简记为 `'x`。
```lisp
> (quote a)
a
> 'a
a
> (quote (a b c))
(a b c)
```

`(atom x)` 会在 `x` 是原子或空表时返回原子 `t`，否则返回 `()`。
按惯例用原子 `t` 表示真，用空表表示假。
```lisp
> (atom 'a)
t
> (atom '(a b c))
()
> (atom '())
t
```

这时，通过使用 `quote` 可以避免表达式被求值。
```lisp
> (atom (atom 'a))
t
> (atom '(atom 'a))
()
```

第一例实质上是 `(atom t)`，因此返回真，而第二例中 `atom` 后则是一个表，因此返回假。

`quote` 的作用即是**引用**，这一奇怪特性来自于 Lisp 最与众不同的特征：代码和数据由相同的结构构成。[^feature]

`(eq x y)` 会在 `x` 和 `y` 的值是同一个原子或都是空表时返回 `t`，否则返回 `()`。
```lisp
> (eq 'a 'a)
t
> (eq 'a 'b)
()
> (eq '() '())
t
```

`(car x)` 期望 `x` 的值是一个表，并且返回 `x` 的第一个元素。
```lisp
> (car '(a b c))
a
```

`(cdr x)` 期望 `x` 的值是一个表，并且返回 `x` 的第一个元素之后的所有元素。
```lisp
> (cdr '(a b c))
(b c)
```

`(cons x y)` 期望 `y` 的值是一个表，并且返回一个新表，其第一个元素是 `x` 的值，之后是 `y` 的值的各个元素。
```lisp
> (cons 'a '(b c))
(a b c)
> (cons 'a (cons 'b (cons 'c '())))
(a b c)
> (car (cons 'a '(b c)))
a
> (cdr (cons 'a '(b c)))
(b c)
```

`(cond (p1 e1) ... (pn en))` 依次对每个 `p` 表达式求值直到有一个返回 `t`，如果能找到这样的 `p` 表达式，相应的 `e` 表达式的值作为整个 `cond` 表达式的返回值。此命名即 `condition` 简称。
```lisp
> (cond ((eq 'a 'b) 'first)
        ((atom 'a)  'second))
second
```

易发现 `(cond (x y) ('t z))` 即为一些语言中的 `if x then y else z`。

[^evaluate]: 当表达式以七个原始操作符中 `quote`，`cond` 以外的表达式进行时，它的自变量总是要求值的。其中 `cond` 不被触及的表达式可以不被求值。

称这样的操作符为函数。

### 函数
函数表示为 `(lambda (p1 ... pn) e)`，其中 $p_i$ 是原子，称为参数，`e` 是表达式。

如果表达式形如 `((lambda (p1 ... pn) e) a1 ... an)` 则称为函数调用。
它的值计算如下：每一个表达式 $a_i$ 先求值，然后 `e` 再求值。在 `e` 的求值过程中，每个出现在 `e` 中的 $p_i$ 的值是相应的 $a_i$ 在最近一次的函数调用中的值。
```lisp
> ((lambda (x) (cons x '(b))) 'a)
(a b)
> ((lambda (x y) (cons x (cdr y)))
   'z
   '(a b c))
(z b c)
```

如果一个表达式的第一个元素 `f` 是原子且 `f` 不是原始操作符，形如 `(f a1 ... an)` 且 `f` 的值是一个函数`(lambda (p1 ... pn) e)`，则以上表达式的值就是 `((lambda (p1 ... pn) e) a1 ... an)` 的值。

这意味着，参数在表达式中可以作为操作符使用。
```lisp
> ((lambda (f) (f '(b c)))
   '(lambda (x) (cons 'a x)))
(a b c)
```

有另外一个函数记号使得函数能提及它本身，使我们能方便地定义递归函数。

### 递归
理论上说，引入新的记号是可以通过 `Y combinator` 避免的。可以参阅：[Y组合子的一个启发式推导](https://zhuanlan.zhihu.com/p/547191928)。

在 Lambda 演算中，其定义为：

$$Y = \lambda f. (\lambda x. f(x x))(\lambda x. f(x x))$$

这使得

$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f)$$

不管怎么说，让我们引入记号 `label`。

用 `(label f (lambda (p1 ... pn) e))` 表示形如 `(lambda (p1 ... pn) e)` 的函数，并允许表达式 `e` 中的原子 `f` 对应整个函数 `f`。

假设我们要定义函数 `subst` 使 `(subst x y z)`，其中 `x` 为表达式，`y` 为原子，`z` 为表，得到一个基于 `z` 的表，但其中所有（任意深度的）原子 `y` 被替换为 `x` 的值。如：
```lisp
> (subst 'm 'b '(a b (a b c) d))
(a m (a m c) d)
```

这个函数可以被定义为：
```lisp
(label subst (lambda (x y z)
               (cond ((atom z)
                      (cond ((eq z y) x)
                            ('t z)))
                     ('t (cons (subst x y (car z))
                               (subst x y (cdr z)))))))
```

让我们缩写 `(label f (lambda (p1 ... pn) e))` 为 `(defun f (p1 ... pn) e)`，并且允许其定义的函数被保留下来。就有：
```lisp
(defun subst (x y z)
  (cond ((atom z)
         (cond ((eq z y) x)
               ('t z)))
        ('t (cons (subst x y (car z))
                  (subst x y (cdr z))))))
```

### 一些函数
让我们定义一些新的函数。

首先我们用形如 `cxr` 的序列定义 `car` 和 `cdr` 的组合，如 `(cadr e)` 为 `(car (cdr e))` 的缩写。
```lisp
> (cadr '((a b) (c d) e))
(c d)
> (caddr '((a b) (c d) e))
e
> (cdar '((a b) (c d) e))
(b)
```

再用 `(list e1 ... en)` 代替 `(cons e1 ... (cons en '()) ... )`。
```lisp
> (cons 'a (cons 'b (cons 'c '())))
(a b c)
> (list 'a 'b 'c)
(a b c)
```

接下来定义一些常见的逻辑工具。在原文中为防和 Common Lisp 存在的函数重复而在函数名后加 `.`，这里我们不关心任何 Lisp 方言，因此不作此操作。

`(null x)` 检测 `x` 是否为空表。
```lisp
(defun null (x)
  (eq x '()))

> (null 'a)
()
> (null '())
t
```

与。
```lisp
(defun and (x y)
  (cond (x (cond (y 't) ('t '())))
        ('t '())))
```

非。
```lisp
(defun not (x)
 (cond (x '())
       ('t 't)))
```

`(append x y)` 将表拼接。
```lisp
(defun append (x y)
 (cond ((null x) y)
       ('t (cons (car x) (append (cdr x) y)))))
```

`(pair x y)` 将表 `(x y z)` 和 `(a b c)` 变为 `((x a) (y b) (z c))`。
```lisp
(defun pair (x y)
 (cond ((and (null x) (null y)) '())
       ((and (not (atom x)) (not (atom y)))
        (cons (list (car x) (car y))
              (pair (cdr x) (cdr y))))))
```

`(assoc x y)` 取原子 `x` 和形如 `pair` 返回值的表 `y`，然后返回 `y` 中每一对中第一个值是 `x` 所对应的第二个值。
```lisp
(defun assoc (x y)
 (cond ((eq (caar y) x) (cadar y))
       ('t (assoc x (cdr y)))))
```

### 计算
现在我们已有了定义 `eval` 的所有材料。它可以接受任意 Lisp 表达式然后返回它的值。

```lisp
(defun eval (e a)
  (cond 
    ((atom e) (assoc e a))
    ((atom (car e))
     (cond 
       ((eq (car e) 'quote) (cadr e))
       ((eq (car e) 'atom)  (atom   (eval (cadr e) a)))
       ((eq (car e) 'eq)    (eq     (eval (cadr e) a)
                                    (eval (caddr e) a)))
       ((eq (car e) 'car)   (car    (eval (cadr e) a)))
       ((eq (car e) 'cdr)   (cdr    (eval (cadr e) a)))
       ((eq (car e) 'cons)  (cons   (eval (cadr e) a)
                                    (eval (caddr e) a)))
       ((eq (car e) 'cond)  (evcon (cdr e) a))
       ('t (eval (cons (assoc (car e) a)
                        (cdr e))
                 a))))
    ((eq (caar e) 'label)
     (eval (cons (caddar e) (cdr e))
           (cons (list (cadar e) (car e)) a)))
    ((eq (caar e) 'lambda)
     (eval (caddar e)
           (append (pair (cadar e) (evlis (cdr e) a))
                   a)))))

(defun evcon (c a)
  (cond ((eval (caar c) a)
         (eval (cadar c) a))
        ('t (evcon (cdr c) a))))

(defun evlis (m a)
  (cond ((null m) '())
        ('t (cons (eval  (car m) a)
                  (evlis (cdr m) a)))))
```

在 `(eval e a)` 中，`e` 是将被解析的表达式，而 `a` 是一个形如 `pair` 格式的列表，表示原子的值的注册，称为「环境」。

`eval` 的核心是通过条件判断进行的。

第一个分支处理了原子。
```lisp
> (eval 'x '((x a) (y b)))
a
```

第二个分支处理了原始操作符。
```lisp
> (eval '(eq 'a 'a) '())
t
> (eval '(cons x '(b c))
        '((x a) (y b)))
(a b c)
```

除 `quote` 外它们都会调用 `eval` 自身来递归地得到值。

`cond` 的计算略复杂一些，该部分被放入了辅助函数 `evcon` 中。
```lisp
> (eval '(cond ((atom x) 'atom)
               ('t 'list))
        '((x '(a b))))
list
```

最后一个部分是函数调用。先把原子替换成对应的值（含 `label` 或 `function` 的表达式），再调用自身。

如 `(eval '(f '(b c)) '((f (lambda (x) (cons 'a x)))))` 变为 `(eval '((lambda (x) (cons 'a x)) '(b c)) '( ... ))`，然后得到 `(a b c)`。

第三个分支处理 `label` 函数调用。这会用内部 `lambda` 表达式替代，并将这一标签置于「环境」中。如，
```lisp
(eval '((label firstatom (lambda (x)
                           (cond ((atom x) x)
                                 ('t (firstatom (car x))))))
        y)
      '((y ((a b) (c d)))))
```

会被替代为，
```lisp
(eval '((lambda (x)
          (cond ((atom x) x)
          ('t (firstatom (car x)))))
        y)
      '((firstatom
         (label firstatom (lambda (x)
                          (cond ((atom x) x)
                                ('t (firstatom (car x)))))))
        (y ((a b) (c d)))))
```

最终返回 `a`。

最后一个分支处理 `lambda` 函数调用。对形如 `((lambda (p1 ... pn) e) a1 ... an)` 的表达式，`a1 ... an` 的值会置于「环境」中，对应 `p1 ... pn`。
```lisp
(eval '((lambda (x y) (cons x (cdr y)))
        'a
        '(b c d))
      '())
```

会变为，
```lisp
(eval '(cons x (cdr y))
      '((x a) (y (b c d))))
```

最终返回 `(a c d)`。

---

这一设计的实现使得仅通过七个原始操作符 `quote`，`atom`，`eq`，`car`，`cdr`，`cons` 和 `cond` 完成[^only_seven]一个可用且具备优雅性[^elegant]的计算模型是可能的（尽管可能相当复杂）。

### 反思
在 1960 的原始定义中缺乏了很多使用的特性。如语句不会产生 `side-effect`（即每一个完整的表达式被单独执行，没有联系），因而没有顺序执行流程。并且没有实用的数字（尽管可以用长度为 $n$ 的列表表示）。

McCarthy 的想法仍是今日的 Lisp 的语义的核心。Lisp 本质上并非一个为 AI 或 `rapid prototyping` 等任务设计的工具，它是当你试图公理化计算时的一个产物。

### 数字 {#lambda-natural-number}
自然数可以被 Peano 公理所描述。其核心是，存在起点 0，并且每个自然数都有其后继。

在 Lambda 演算中，我们可以这样定义。

$$iszero\ n = n\ (\lambda b.\ false)\ true$$
$$0=\lambda f.\ \lambda s.\ s$$
$$1=\lambda f.\ \lambda s.\ f\ s$$
$$2=\lambda f.\ \lambda s.\ f\ (f\ s)$$

……诸如此类。自然数的信息被表现在 $f$ 的层叠数目上，对其计算只需用 $s$ 给出的接口添加层叠数即可。

$$succ\ n = \lambda f.\ \lambda s.\ f\ (n\ f\ s)$$
$$add\ n_1\ n_2 = n_1\ succ\ n_2$$
$$mult\ n_1\ n_2 = n_1\ (add\ n_2)\ 0$$

举个例子。
```txt
add 0 = (λn1. λn2. n1 succ n2) 0
      = λn2. 0 succ n2
      = λn2. n2
      = λx. x
```

又如。
```txt
add 1 1 = 1 succ 1
        = succ 1
        = λf. λs. f (f s)
        = 2
```

又如。
```txt
mult 2 2 = 2 (add 2) 0
         = (add 2) ((add 2) 0)
         = 2 succ (2 succ 0)
         = succ (succ (succ (succ 0)))
         = succ (succ (succ (λf. λs. f (0 f s))))
         = succ (succ (succ (λf. λs. f s)))
         = succ (succ (λg. λy. g ((λf. λs. f s) g y)))
         = succ (succ (λg. λy. g (g y)))
         = ...
         = λg. λy. g (g (g (g y)))
         = 4
```

### side-effect
[^3]

## 重写系统
TODO. 可阅读 <https://www.bananaspace.org/wiki/%E9%87%8D%E5%86%99%E7%B3%BB%E7%BB%9F>

## LISP 方言

## 没有银弹

## 后记
### The Roots of Lisp 的说明
> In translating McCarthy's notation into running code I tried to change as little
> as possible. I was tempted to make the code easier to read, but I wanted to
> keep the flavor of the original.
>
> In McCarthy's paper, falsity is represented by `f`, not the empty list. I used
> `()` to represent falsity so that the examples would work in Common Lisp. The
> code nowhere depends on falsity happening also to be the empty list; nothing
> is ever consed onto the result returned by a predicate.
>
> I skipped building lists out of dotted pairs, because you don't need them to
> understand `eval`. I also skipped mentioning `apply`, though it was `apply` (a very
> early form of it, whose main purpose was to quote arguments) that McCarthy
> called the universal function in 1960; `eval` was then just a subroutine that `apply`
> called to do all the work.
>
> I defined `list` and the `cxrs` as abbreviations because that's how McCarthy
> did it. In fact the `cxrs` could all have been defined as ordinary functions. So
> could `list` if we modified `eval`, as we easily could, to let functions take any
> number of arguments.
>
> McCarthy's paper only had five primitive operators. He used `cond` and
> `quote` but may have thought of them as part of his metalanguage. He likewise
> didn't define the logical operators `and` and `not`, but this is less of a problem
> because adequate versions can be defined as functions.
>
> In the definition of `eval.` we called other functions like `pair.` and `assoc.`,
> but any call to one of the functions we defined in terms of the primitive operators
> could be replaced by a call to `eval.`. That is,
> ```lisp
> (assoc. (car e) a)
> ```
>
> could have been written as
> ```lisp
> (eval. '((label assoc.
>                 (lambda (x y)
>                   (cond ((eq (caar y) x) (cadar y))
>                   ('t (assoc. x (cdr y))))))
>          (car e)
>          a)
>         (cons (list 'e e) (cons (list 'a a) a)))
> ```
>
> There was a small bug in McCarthy's `eval`. Line 16 was (equivalent to)
> `(evlis. (cdr e) a)` instead of just `(cdr e)`, which caused the arguments in
> a call to a named function to be evaluated twice. This suggests that this
> description of eval had not yet been implemented in IBM 704 machine language
> when the paper was submitted. It also shows how hard it is to be sure of the
> correctness of any length of program without trying to run it.
>
> I encountered one other problem in McCarthy's code. After giving the
> definition of eval he goes on to give some examples of higher-order functions—
> functions that take other functions as arguments. He defines `maplist`:
> ```lisp
> (label maplist
>        (lambda (x f)
>          (cond ((null x) '())
>                ('t (cons (f x) (maplist (cdr x) f))))))
> ```
>
> then uses it to write a simple function `diff` for symbolic differentiation. But
> `diff` passes `maplist` a function that uses $x$ as a parameter, and the reference
> to it is captured by the parameter $x$ within `maplist`.
>
> It's an eloquent testimony to the dangers of dynamic scope that even the
> very first example of higher-order Lisp functions was broken because of it. It
> may be that McCarthy was not fully aware of the implications of dynamic scope
> in 1960. Dynamic scope remained in Lisp implementations for a surprisingly
> long time—until Sussman and Steele developed Scheme in 1975. Lexical scope
> does not complicate the definition of eval very much, but it may make compilers
> harder to write.

[^1]: ``Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part1.'' Communication of the ACM 3:4, April 1960, pp. 184-195.
[^2]: A. CHURCH, The Calculi of Lambda-Conversion (Princeton University Press, Princeton, N. J., 1941).
[^feature]: 在现实中，生物、语言等具有更复杂的此种特性。
[^only_seven]: 原文如此，但不借助 `lambda` 完成 `eval` 的递归似乎是不可能的。
[^elegant]: 尽管早有大量图灵完备的模型存在，先前并无具备足够抽象性的语言。\
            而这是发明 Lisp 的目标之一。
[^3]: Guy Lewis Steele, Jr. and Gerald Jay Sussman, ”The Art of the Interpreter, or the Modularity Complex (Parts Zero, One, and Two),” MIT AI Lab Memo 453, May 1978.
[^maplist]: > Present day Lisp programmers would use `mapcar` instead of `maplist` here. This example
            does clear up one mystery: why `maplist` is in Common Lisp at all. It was the original mapping
            function, and `mapcar` a later addition.
