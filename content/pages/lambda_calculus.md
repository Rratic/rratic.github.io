+++
title = "无类型 λ 演算"
date = 2025-03-22
updated = 2025-04-12

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "计算机", "笔记", "离散", "抽象"]
+++

<style>
  code::before, code::after {
    content: "" !important;
  }

  p > code, li > code {
    text-decoration: 2px gold underline;
    text-underline-offset: 2px;
  }
</style>

## 说明 {#about}
本节讨论的是 Alonzo Church 发明的 `Lambda Calculus`，即**无类型 λ 演算**。[^paper-invention]

## 语法 {#grammar}
Lambda 演算的语法形式极其简单。一种可理解的形式文法如下：
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
* 返回恒等函数的函数 $\lambda y.\ (\lambda x.\ x)$ 这里的 $y$ 参数被忽略了。引入参数而不使用它是被允许的。

在书写时，常常省略括号。对此的惯例是：
* 函数声明时，函数体尽可能向右扩展。
* 函数应用时，从左到右结合（即“左结合”）。

例如，$\lambda x.\ x\ \lambda y.\ x\ y\ z$ 应被理解为 $\lambda x.\ (x\ \lambda y.\ ((x\ y)\ z))$。

## Currying
尽管在定义中，函数必须恰有一个参数，多个参数的函数仍然可以通过 `currying` 技术间接地表示。

例如，不合法的 $\lambda x\ y.\ x+y$ 可以被表达为 $\lambda x.\ (\lambda y.\ x+y)$。后世的闭包思想也来自于此。

巧妙的是，你可以传入少于全部参数个数的参数。例如代入 3，可以得到 $\lambda y.\ 3+y$。

## 求值 {#lambda-evaluation}
有两条求值规则：
* `Alpha equivalence` (or conversion)

  $\alpha$-重命名意为，可任意改变变量名。如有歧义的 $\lambda x.\ x(\lambda x.\ x)$ 可改为 $\lambda x.\ x(\lambda y.\ y)$。
* `Beta reduction`

  $\beta$-规约意为，在应用时将声明展开。例如 $(\lambda x.\ x)(\lambda y.\ y)$ 被展开为 $\lambda y.\ y$。

此外，$\eta$-等价意为，相同作用的函数可相互替换。

## 求值顺序 {#evaluation-order}
考虑函数应用 $(\lambda y.\ (\lambda x.\ x)\ y) E$。它有两种计算方法：
* 先求内层，得到 $(\lambda y.\ y) E$，然后得到 $E$。
* 先求外层，得到 $(\lambda x.\ x) E$，然后得到 $E$。

根据 Church–Rosser 定理（其证明会在[之后](#church-rosser-theorem)提供），这两种方法是等价的，最终会得到相等的结果。

但我们在计算时必须作出选择。因而产生了两种不同的方式。
* 在函数应用前，就计算函数参数的值。
  也被记作：
  * `Call by Value`
  * `Applicative Order` 应用次序
  * `Eager Evaluation` 紧迫求值
* 在函数应用前，不计算函数参数的值，直到需要时才求值。
  也被记作：
  * `Call by Name`
  * `Normal Order` 正则/标准次序
  * `Lazy Evaluation` 懒惰求值

如果表达式可以被化简，那么标准次序总是能够将表达式成功化简，使用应用次序则可能陷入无限递归。

### 递归 {#recursive-functions}
通过 `Y combinator` 可以原生地实现递归。可以参阅：[Y组合子的一个启发式推导](https://zhuanlan.zhihu.com/p/547191928)。

在 Lambda 演算中，其定义为：

$$Y = \lambda f. (\lambda x. f(x x))(\lambda x. f(x x))$$

这使得

$$Y f = (\lambda x. f(x x))(\lambda x. f(x x))$$
$$= (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x))$$
$$= f(Y f)$$

如果我们希望定义一个递归函数。
$$f = \lambda fact. (\lambda n. \begin{cases} 1, & \text {if $n$ < 2} \\\\ n\times fact(n-1), & \text{else} \end{cases})$$

那么，若使用正则次序，可得到正确的结果。
```txt
Y f 2 = (λx. f(x x))(λx. f(x x)) 2
      = f((λx. f(x x))(λx. f(x x)) 2)
      = 2 * (λx. f(x x))(λx. f(x x))(1)
      = 2 * f((λx. f(x x))(λx. f(x x)) 1)
      = 2 * 1
```

但使用应用次序，则会造成无限递归。
```txt
Y f 2 = (λx. f(x x))(λx. f(x x)) 2
      = f((λx. f(x x))(λx. f(x x)) 2)
      = f(f((λx. f(x x))(λx. f(x x))) 2)
      = f(f(f((λx. f(x x))(λx. f(x x)))) 2)
      = ...
```

此时需改用 `Z combinator`，将参数改造成延迟求值的。

$$Z = \lambda f. (\lambda x. f(\lambda y. (x x) y))(\lambda x. f(\lambda y. (x x) y))$$

```txt
Z f 2 = (λx. f(λy. (x x) y)) (λx. f(λy. (x x) y)) 2
      = f((λy. (λx. f(λy. (x x) y)) (λx. f(λy. (x x) y)) y) 2)
      = 2 * (λy. (λx. f(λy. (x x) y)) (λx. f(λy. (x x) y)) y) 1
      = 2 * (λx. f(λy. (x x) y)) (λx. f(λy. (x x) y)) 1
      = ...
      = 2 * 1
```

## 数据类型 {#datatype}
Lambda 演算中只有函数而没有纯粹的、实践中关心的数据类型，不过我们可以用函数来间接地表达它们。

### 布尔值 {#datatype-boolean}
布尔值支持二元逻辑运算，但其最重要的意义是实现条件判断。

简单地定义 `true` 为 $\lambda x.\ \lambda y.\ x$，`false` 为 $\lambda x.\ \lambda y.\ y$。

这样，`if e then u else v` 就可被重写为 $e\ u\ v$。

### 自然数 {#datatype-number}
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

### 列表 {#datatype-list}
构造基于这一思想：将“如何遍历列表”的问题放到使用列表时。
```rust
let nil = λc. λn. n // 空列表
let cons = λh. λt. λc. λn. c h (t c n) // h 表示在开头添加的元素，t 是之后的列表部分

cons 1 (cons 2 (cons 3 nil)) // 构造列表示例
// = λc. λn. c 1 (λc. λn. c 2 (λc. λn. c 3 (λc. λn. n)))
```

## 解释器 {#interpreter}
在 [Part 1](https://tejqunair.com/posts/lambda-part-1/) 与 [Part 2](https://tejqunair.com/posts/lambda-part-2/) 阅读如何使用 Rust 完成 Lambda 演算解释器。你可以在[这里](https://sshwy.github.io/lamcalc/playground.html)找到一个在线演绎器，但它需要手动操作。

## 重写系统 {#rewriting-systems}
让我们探讨一个更一般的模型。本部分参考了[香蕉空间](https://www.bananaspace.org)（采用 CC BY-SA 4.0 许可）的相关词条（**重写系统**，**正规性**，**合流性**）。

**重写**是将表达式的一部分替换为其它表达式的过程，可以看作一种关系或者一组规则。在此之上，**重写系统**是由一个表达式的集合和表达式到表达式之间的重写关系组成的结构，类似于有向图。

### 记号 {#rewriting-notation}
因此，所有资料为：表达式的集合 $E$ 与重写关系 $(\to ) \subset E\times E$。

记 $\stackrel{*}{\to}$ 表示将 $\to$ 应用任意自然数次。

用双向箭头 $\stackrel{*}{\leftrightarrow}$ 表示两边都可的版本。

### 正规性 {#normalization}
对于重写系统 $E$ 和 $a,b\in E$，若 $a\stackrel{*}{\to} b \iff a=b$，那么 $a$ 是一个**正规形式**。

若重写系统中任意表达式都能通过某个特定的顺序重写为正规形式，那么该重写系统是**弱正规**的。

若重写系统中任意表达式都能通过任意顺序重写为正规形式，那么该重写系统是**强正规**的。强正规性又叫**停机性**。

### 合流性 {#confluence}
若对于重写系统 $E$ 和任意 $a,b,c\in E$ 一旦 $b\stackrel{\*}{\gets} a \stackrel{\*}{\to} c$ 就存在 $d$ 使得 $b\stackrel{\*}{\to} d \stackrel{\*}{\gets} c$，那么 $E$ 是合流的。

Church–Rosser 定理说，λ 演算具有合流性。

### Church–Rosser 定理 {#church-rosser-theorem}
本部分原本希望参考 [D. Kozen/Church–Rosser Made Easy](https://www.cs.cornell.edu/~kozen/Papers/ChurchRosser.pdf)[^paper-proof]，但其中包含了过多未声明含义的术语，且包含了今天看来不必要的步骤，例如，使用了包含序列的集合来定义树[^prefix-tree]，并混用术语。

其列举的文献中包含了其它的证明方式。此外，你可以[在此](https://pauillac.inria.fr/~huet/PUBLIC/residuals.pdf)找到一个使用 Coq 形式化验证的证明。

以下将对证明思路进行摘要。

首先，α-等价关系的刻画是易完成的。只需考虑 β-规约，全体的合法 Lambda 表达式记作 $\omega = \omega^{\*}/\sim _\alpha$。

一步 β-规约是之前定义的 $(\lambda x.\ a)\ b\to a[b/x]$。多步规约就是之前定义的 $\stackrel{\*}{\to}$，我们重新记作 $\twoheadrightarrow$，它也可看作一步 β-规约的自反传递闭包。

证明中最大的难题是归约时，内部的可归约式结构可能被破坏。

为此，我们定义并行归约（parallel reduction），使用符号 $\Longrightarrow$ 标记，满足。
- $x\Longrightarrow x$
- 若 $M\Longrightarrow M'$ 则 $\lambda x.\ M\Longrightarrow \lambda x.\ M'$
- 若 $M\Longrightarrow M', N\Longrightarrow N'$ 则 $M\ N\Longrightarrow M'\ N'$
- 若 $M\Longrightarrow M', N\Longrightarrow N'$ 则 $(\lambda x.\ M)N\Longrightarrow M'[N'/x]$

容易证明并行归约是合流的。

而后，存在包含关系 $(\to)\subset(\Longrightarrow)\subset(\twoheadrightarrow)=(\stackrel{\*}{\Longrightarrow})$ 其中最后一点可以分别说明两边的包含关系。

由此，λ 演算具有合流性。

[^paper-invention]: A. CHURCH, The Calculi of Lambda-Conversion (Princeton University Press, Princeton, N. J., 1941).
[^paper-proof]: DOI 10.3233/FI-2010-306
[^prefix-tree]: `prefix-closed` 的集合需满足，对其每个元素 $s$ 均有 $s$ 的前缀在集合中。
                例如，使用 $\{\epsilon, a, ab, ac, abd\}$ 定义一个树的父子关系，其中 $\epsilon$ 表示空序列。
