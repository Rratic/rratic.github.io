+++
title = "LISP 模式：图灵完备及元编程"
description = "关于最早的 LISP 的核心想法，它如何实现图灵完备及元编程，及在此基础上的扩展和现状。"
date = 2025-03-22
updated = 2025-03-29

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "函数式编程", "Lisp"]
+++

前置知识
- [无类型 λ 演算](/posts/lambda-calculus/)

---

## 介绍 {#about}
在 1960 年，John McCarthy 发表了一篇划时代的论文。其对编程的贡献有如欧几里德对几何的贡献。[^1]

这篇论文中定义了一个名为 Lisp（意为 list processing）的编程语言。Paul Graham 认为，“目前为止只有两种真正干净利落，始终如一的编程模式：C 语言模式和 Lisp 语言模式。此二者就象两座高地……随着计算机变得越来越强大，新开发的语言一直在坚定地趋向于 Lisp 模式。”

Lisp 似乎受到了 Lambda 演算与 Kleene 的递归论的影响，但不完全来自于它们。

## Lisp 之根源 {#roots-of-lisp}
本部分参考了[其原始论文](https://www-formal.stanford.edu/jmc/recursive.pdf)。

另参考了 [Paul Graham](http://www.paulgraham.com/) 所作 [The Roots of Lisp](https://blog.freecloud.dev/img/the-roots-of-lisp.pdf)。找到了其一个 2003 年的译本 [Lisp 之根源](http://daiyuwen.freeshell.org/gb/rol/roots_of_lisp.html)，但该翻译多有谬误。

### S-表达式 {#s-expression}
一个表达式（expression）可以是：
- 一个原子（atom），在这里是一个连续的字母序列，如 `foo`。
- 一个表（list），表达方式形如 `()`，`(foo)`，`(foo bar)`，其中含有零个、一个、两个或多个表达式。

可见 `(foo (bar) baz)` 是表达式。

在 Lisp 中，合法的表达式有值（value），求值往往是默认进行的。[^evaluate]

### 原始操作符 {#primitive-operators}
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

`(atom x)` 会在 $x$ 是原子或空表时返回原子 `t`，否则返回 `()`。
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

`(eq x y)` 会在 $x$ 和 $y$ 的值是同一个原子或都是空表时返回 `t`，否则返回 `()`。
```lisp
> (eq 'a 'a)
t
> (eq 'a 'b)
()
> (eq '() '())
t
```

`(car x)` 期望 $x$ 的值是一个表，并且返回 $x$ 的第一个元素。
```lisp
> (car '(a b c))
a
```

`(cdr x)` 期望 $x$ 的值是一个表，并且返回 $x$ 的第一个元素之后的所有元素。
```lisp
> (cdr '(a b c))
(b c)
```

`(cons x y)` 期望 $y$ 的值是一个表，并且返回一个新表，其第一个元素是 $x$ 的值，之后是 $y$ 的值的各个元素。
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

`(cond (p1 e1) ... (pn en))` 依次对每个 $p$ 表达式求值直到有一个返回 `t`，如果能找到这样的 $p$ 表达式，相应的 $e$ 表达式的值作为整个 `cond` 表达式的返回值。此命名即 `condition` 简称。
```lisp
> (cond ((eq 'a 'b) 'first)
        ((atom 'a)  'second))
second
```

易发现 `(cond (x y) ('t z))` 即为一些语言中的 `if x then y else z`。

[^evaluate]: 当表达式以七个原始操作符中 `quote`，`cond` 以外的表达式进行时，它的自变量总是要求值的。其中 `cond` 不被触及的表达式可以不被求值。

称这样的操作符为函数。

### 函数 {#denoting-functions}
{% admonition(type="info", title="说明") %}
从这里开始的内容并非原始的定义。你将在[求值](#evaluation)中看到它们如何起作用。
{% end %}

函数表示为 `(lambda (p1 ... pn) e)`，其中 $p_i$ 是原子，称为参数，$e$ 是表达式。

如果表达式形如 `((lambda (p1 ... pn) e) a1 ... an)` 则称为函数调用。
它的值计算如下：每一个表达式 $a_i$ 先求值，然后 $e$ 再求值。在 $e$ 的求值过程中，每个出现在 $e$ 中的 $p_i$ 的值是相应的 $a_i$ 在最近一次的函数调用中的值。
```lisp
> ((lambda (x) (cons x '(b))) 'a)
(a b)
> ((lambda (x y) (cons x (cdr y)))
   'z
   '(a b c))
(z b c)
```

如果一个表达式的第一个元素 $f$ 是原子且 $f$ 不是原始操作符，形如 `(f a1 ... an)` 且 $f$ 的值是一个函数`(lambda (p1 ... pn) e)`，则以上表达式的值就是 `((lambda (p1 ... pn) e) a1 ... an)` 的值。

这意味着，参数在表达式中可以作为操作符使用。
```lisp
> ((lambda (f) (f '(b c)))
   '(lambda (x) (cons 'a x)))
(a b c)
```

有另外一个函数记号使得函数能提及它本身，使我们能方便地定义递归函数。

### 递归记号 {#mark-for-recursive-functions}
理论上说，引入新的记号是可以通过[组合子](/posts/lambda-calculus/#recursive-functions)避免的。

不管是为了降低复杂性还是组合子提出太晚，让我们引入记号 `label`。

用 `(label f (lambda (p1 ... pn) e))` 表示形如 `(lambda (p1 ... pn) e)` 的函数，并允许表达式 $e$ 中的原子 $f$ 对应整个函数 $f$。

假设我们要定义函数 `subst` 使 `(subst x y z)`，其中 $x$ 为表达式，$y$ 为原子，$z$ 为表，得到一个基于 $z$ 的表，但其中所有的（任意深度的）原子 $y$ 被替换为 $x$ 的值。如：
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

假定我们定义了运算符号，那么可以看到 S-表达式实际上使用了波兰标记法。

如 $a\times b+c^d$ 被表达为 `(+ (* a b) (^ c d))`。

### 一些函数 {#some-functions}
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

接下来定义一些常见的逻辑工具。

在原文中为防止和 Common Lisp 存在的函数重复而无法在 Common Lisp 中运行，在函数名后加 `.` 来进行区分。这里我们不关心这一点，因此不作此操作。

`(null x)` 检测 $x$ 是否为空表。
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

`(assoc x y)` 取原子 $x$ 和形如 `pair` 返回值的表 $y$，然后返回 $y$ 中每一对中第一个值是 $x$ 所对应的第二个值。
```lisp
(defun assoc (x y)
 (cond ((eq (caar y) x) (cadar y))
       ('t (assoc x (cdr y)))))
```

### 求值 {#evaluation}
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

在 `(eval e a)` 中，$e$ 是将被解析的表达式，而 $a$ 是一个形如 `pair` 格式的列表，表示原子的值的注册，称为「环境」。

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

这一设计的实现使得仅通过七个原始操作符 `quote`，`atom`，`eq`，`car`，`cdr`，`cons` 和 `cond` 完成[^only-seven]一个可用且具备优雅性[^elegant]的计算模型是可能的（尽管可能相当复杂）。

### 反思 {#review}
在 1960 的原始定义中缺乏了很多使用的特性。如语句不会产生 `side-effect`（即每一个完整的表达式被单独执行，没有联系），因而没有顺序执行流程。并且没有实用的数字（尽管可以用长度为 $n$ 的列表表示，或者[使用函数表达](/pages/lambda-calculus/#datatype-number)）。

McCarthy 的想法仍是今日的 Lisp 的语义的核心。Lisp 本质上并非一个为 AI 或 `rapid prototyping` 等任务设计的工具，它是当你试图公理化计算时的一个产物。

## 引入副作用 {#introduce-side-effects}
### 概述 {#conclusion}
如果想要阅读如何构建一个有 `side-effect` 的解释器并且完成一些任务，可以阅读 `AI Memo No. 543`。[^3]这篇文章里充满了技术细节。

如果想要在纯编程理论的方向继续前进，可以看看 Scheme。

这里我们讨论解决更现实的问题。不如看看 Common Lisp 是如何设计的。这参阅了 [Common Lisp 教程](https://lisp.fullstack.org.cn/learn/getting-started/) 中的内容。

首先，Common Lisp 允许 `3.14` 这样的数字，及 `+`，`my-variable` 等符号作为原子。另用冒号前缀符号表示关键字，其求值后为自身。

这里 `t` 为真常量。
```lisp
CL-USER> (format t "Hello, world!")
Hello, world!
NIL
```

添加了很多人性化的内置功能。
```lisp
(defun fib (n)
  "Return the nth Fibonacci number." ;; 允许一个字符串作为文档
  (if (< n 2) ;; 函数体声明
      n ;; 省去了 else
      (+ (fib (- n 1))
         (fib (- n 2))))) ;; if 是表达式，因此有返回值
```

可以以多种方式调用。
```lisp
(fib 30)
;; => 832040

(funcall #'fib 30)
;; => 832040

(apply #'fib (list 30))
;; => 832040
```

可以声明局部变量。
```lisp
(let ((x 1)
      (y 5))
  (+ x y))

;; => 6
```

同时允许全局变量，并允许局部上的作用域覆盖。

支持用 `defmacro` 定义宏，用 `defgeneric` 声明通用函数（用 `defmethod` 声明其方法），用 `defclass` 定义类。

可见相对于简洁的 Lisp 作出了很大的让步。

### LISP 方言 {#lisp-dialects}
现今有一席之地的 Lisp 方言包括 `Common Lisp`，`Clojure`，`Scheme` 和 `Emacs Lisp`。

它们为不同的目的而设计了不同的特性，允许安全或不安全的宏。

它们的类型系统都是动态的。

{% admonition(type="warning", title="警告") %}
	本表格基于 DeepSeek 的生成，不保证真实性。
{% end %}

| 特性 | Common Lisp | Clojure | Scheme | Emacs Lisp |
|---|---|---|---|---|
| **设计目标** | 通用、多范式 | 函数式、并发 | 极简、教学导向 | 编辑器扩展 |
| **主要运行方式** | 编译/JIT | JVM/JS/.NET | 解释/编译 | 解释 |
| **典型应用领域** | AI、复杂系统 | 分布式系统、数据 | 教学、语言研究 | 编辑器定制、自动化 |

### 相关内容 {#related-items}
可以看看其它的函数式语言。典型的如 Haskell。也可以看看 Prolog。

## 困境 {#dilemma}
{% admonition(type="warning", title="警告") %}
	此段未完全经过验证。
{% end %}

我们已经从纯理论与实践两个角度讨论了 Lisp。从表面上看，这种精巧的语言应该被广为接受才对。

然而，按照本句完成时的 [TIOBE 排名](https://www.tiobe.com/tiobe-index/)，Lisp 只在第二十三位[^lisp-count]，方言 Clojure 与 Scheme 在 51~100 位间，其它方言则在 100 位之后。

我们没有银弹。Lisp 的核心理念与任何后续叠加的理念，或者其它任何极好的理念，都不能单一地在生产力、可靠性或简单性方面带来数量级的提升。[^5]

### 缺陷 {#downside}
Lisp 的一个明显问题是可读性。S-表达式看起来不符合人类的习惯。

现代的一个可以接受的例子是 [Julia 的元编程](https://docs.juliacn.com/latest/manual/metaprogramming/)。这是通过支持在运行时解析字符串实现的。
```jl
julia> Meta.parse("1 + 1")
:(1 + 1)
```

你甚至可以运行这样的程序。
```jl
expr = :(function f(x)
    y = x ^ 2;
    x + y
end)

f = eval(expr)
display(f(8))
```

另一个明显的问题是性能。在编译器不够强大时，使用汇编进行底层操作，使用 C 模式进行指针操作是十分必要的。但函数式语言难以与这种优化产生联系。此外，动态类型具有较大的空间、时间开销。

可以看看新近的 [Exo 2](https://news.mit.edu/2025/high-performance-computing-with-much-less-code-0313)。

### 复杂性 {#complexity}
在 The Mythical Man-Month 等诸多著作中都有一个认识，复杂性来自于**本质复杂性**（问题本身的复杂性）和**偶然复杂性**（语言、工具或方法带来的复杂性）这两类。在纯粹模型之外，Lisp 降低的复杂性没有那么大。

Richard P. Gabriel 提出了[^6]两种设计理念的不同。
1. The Right Thing
  * 追求设计的完美和完整性。
  * 强调正确性、一致性和优雅性。
  * 倾向于复杂的设计，以满足所有可能的需求。
2. Worse is Better
  * 追求简单性和实用性。
  * 强调快速实现、易于理解和广泛采用。
  * 接受不完美和局限性，优先解决核心问题。

后者支持快速迭代和广泛使用，更符合实际需求。

实际上，由于 Lisp 可以较为自由地完成设计，而设计通常会很复杂，Lisp 社区常产生分歧。

## 后记 {#aftermath}
### The Roots of Lisp 的说明 {#aftermath-roots-of-lisp}
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

## 注释 {#footnotes}
[^1]: ``Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part1.'' Communication of the ACM 3:4, April 1960, pp. 184-195.
[^feature]: 在现实中，生物、语言等具有更复杂的此种特性。
[^only-seven]: 原文如此，但不借助 `lambda` 等记号完成 `eval` 的递归特性似乎是不可能的。
[^elegant]: 尽管早有大量图灵完备的模型存在，先前并无具备足够抽象性的语言。而这是发明 Lisp 的目标之一。
[^3]: Guy Lewis Steele, Jr. and Gerald Jay Sussman, ”The Art of the Interpreter, or the Modularity Complex (Parts Zero, One, and Two),” MIT AI Lab Memo 453, May 1978.
[^lisp-count]: 可能是指 Common Lisp。取决于具体的统计方式。
[^5]: No Silver Bullet—Essence and Accident in Software Engineering
[^6]: <https://www.dreamsongs.com/WorseIsBetter.html>
[^maplist]: > Present day Lisp programmers would use `mapcar` instead of `maplist` here. This example
            does clear up one mystery: why `maplist` is in Common Lisp at all. It was the original mapping
            function, and `mapcar` a later addition.
