+++
title = "LISP 模式"
date = 2025-03-18

[extra]
math = true

[taxonomies]
categories = ["文章"]
tags = ["数学", "计算机", "离散", "抽象"]
+++

<style>
  code::before, code::after {
    content: "" !important;
  }

  p code {
    text-decoration: 3px gold underline;
  }
</style>

## 关于
在 1960 年，John McCarthy 提出了一个划时代的理论。

这应当受到了 Alonzo Church 发明的 `Lambda Calculus` 的启发。

本文参考了[其原始论文](https://www-formal.stanford.edu/jmc/recursive.pdf) 及 Paul Graham 所作 [The Roots of Lisp](https://blog.freecloud.dev/img/the-roots-of-lisp.pdf)。参考了其一个译本
[Lisp 之根源](http://daiyuwen.freeshell.org/gb/rol/roots_of_lisp.html)，但该翻译多有谬误。

## 表达式
**表达式**（**expression**）可以是：
- 一个**原子**（**atom**），在这里是一个连续的字母序列，如 `foo`。
- 一个**表**（**list**），表达方式形如 `()`，`(foo)`，`(foo bar)`，其中含有零个、一个、两个或多个表达式。

可见 `(foo (bar) baz)` 是表达式。

在 Lisp 中，合法的表达式有**值**（**value**），求值往往是默认进行的。[^evaluate]

## 原始操作符
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

`quote` 的作用即是**引用**，这一奇怪特性来自于 Lisp 最与众不同的特征：代码和数据由相同的结构构成。

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

[^evaluate]: 当表达式以七个原始操作符中 `quote`，`cond` 以外的表达式进行时，它的自变量总是要求值的。

称这样的操作符为函数。

## 函数
函数表示为 `(lambda (p1 ... pn) e)`，其中 `pi` 是原子，称为参数，`e` 是表达式。

如果表达式形如 `((lambda (p1 ... pn) e) a1 ... an)` 则称为函数调用。
它的值计算如下：每一个表达式 `ai` 先求值，然后 `e` 再求值。在 `e` 的求值过程中，每个出现在 `e` 中的 `pi` 的值是相应的 `ai` 在最近一次的函数调用中的值。
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

## 递归
理论上说，引入新的记号是可以通过 `Y combinator` 避免的。可以参阅：[Y组合子的一个启发式推导](https://zhuanlan.zhihu.com/p/547191928)。

在 λ 演算中，其定义为：

$$Y = \lambda f. (\lambda x. f(x x))(\lambda x. f(x x))$$

这使得

$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f)$$
