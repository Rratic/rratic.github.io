+++
title = "计算概论期末复习笔记（Bird Meertens 形式开始的部分）"
description = "使用 Bird Meertens Formalism 导出高效程序与进行自动并行化。"
date = 2025-12-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "函数式编程"]
+++

{{ ref_index(to = "functional-programming") }}

这是本课程的最后一个部分，同时可能是在上半学期和下半学期前部的铺垫下真正想讲的东西。其中函数式编程的想法提供了无副作用的函数和高阶函数的例子，从而能够被我们讨论；定理证明器则允许我们验证推导的正确性。

课程主页在 <https://zhenjiang888.github.io/FP/2025/>.

## 基本概念
BMF/Bird Meertens Formalism 是一套让人们从特定规范表述中导出（高效）程序的函数演算。

### 记号
先明确一些基础的规范：
- 函数应用的优先级最高；函数是可以被 curry 化的 $(f\ a)\ b$
- 函数复合使用 $f \cdot g$
- 外延性：两个函数 $f$ 与 $g$ 是等价的，如果对任意可以作为参数的 $x$ 有 $f\ x = g\ x$

现在已经可以推导出一些简单的结论，如：

$$(f\cdot)\cdot(g\cdot) = ((f\cdot g)\cdot)$$

我们将用 $\oplus, \otimes, \odot$ 等表示二元运算，并且：
- 允许 sectioned $(a\oplus)\ b = (\oplus b)\ a = a \oplus b$
- 用 $\mathrm{id}_\oplus$ 表示双边的单位元

记常值函数 $K\ a\ b = a$，这和 [SKI 演算](@/posts/lambda_calculus.md#evaluation-rules)的约定相同。

对列表来说：
- $[]$ 是空列表，有一个把值映到 singleton 列表的映射 $[\cdot]\ a = [a]$
- $+\\!\\!+$ 是我们熟知的拼接 concatenation
- 结构 bag 是在列表基础上忽略顺序，结构 set 是进一步忽略重复
- 熟知的 `map` 写作 $f \ast [a_1, \cdots, a_n] = [f\ a_1, \cdots, f\ a_n]$
- reduce 是指 $\oplus / [a_1, \cdots, a_n] = a_1 \oplus \cdots \oplus a_n$，现在就有 `maximum` 是 $\uparrow/$，其中 $a\uparrow b$ 结果是两者较大值
- `foldl` 即 left to right reduce 是 $\oplus \rightarrow\\!\\!\\!\\!\\!\\!/_e [a_1, \cdots, a_n] = ((e \oplus a_1) \oplus \cdots) \oplus a_n$，右下角不写 $e$ 则是把 $e$ 当作单位元的结果；相应地 `foldr` 即 right to left reduce 是 $\oplus \leftarrow\\!\\!\\!\\!\\!\\!/_e [a_1, \cdots, a_n] = a_1 \oplus (\cdots (a_n \oplus e))$

我们定义 `scanl` 即 left-accumulate 是：

$$\oplus \rightarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/_e [a_1, \cdots, a_n] = [e, e \oplus a_1, \cdots, ((e \oplus a_1) \oplus \cdots) \oplus a_n]$$

相应地 `scanr` 即 right-accumulate 是：

$$\oplus \leftarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/_e [a_1, \cdots, a_n] = [a_1 \oplus (\cdots (a_n \oplus e)), \cdots, a_n \oplus e, e]$$

### 推导示例
现在来看一个具体的问题：
{% admonition(type="question", title="Maximum Segment Sum Problem") %}
对一个整数序列，求所有连续子段的和的最大值。
{% end %}

作为一个朴素的想法，我们定义 $\mathrm{inits} = (+\\!\\!+ \rightarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/ _{\ []}) \cdot [\cdot] \ast$，效果是从空列表到整个列表列出所有前缀；定义 $\mathrm{tails} = (+\\!\\!+ \leftarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/ _{\ []}) \cdot [\cdot] \ast$，效果是从整个列表到空列表列出所有后缀；现在 $\mathrm{segs} = +\\!\\!+ / \cdot \mathrm{tails} \ast \cdot \operatorname{inits}$ 给出所有连续子段。

现在 $\mathrm{mss} = \uparrow/ \cdot +/\ast \cdot \operatorname{segs}$ 即是原问题的最朴素的解，只不过它的时间复杂度惨不忍睹。

---

现在我们将这个算法优化成线性的。首先，Horner’s Rule 是指在分配律 $(a \oplus b) \otimes c = (a \otimes c) \oplus (b \otimes c)$ 成立时有：

$$\oplus/ \cdot \otimes/\ast \cdot \operatorname{tails} = \odot \rightarrow\\!\\!\\!\\!\\!\\!/ _{\ \mathrm{id} _\otimes}$$

其中 $a \odot b = (a \otimes b) \oplus \mathrm{id} _\otimes$.

现在就可以进行推导：

$$
\begin{align*} 
& \mathrm{mss} \\\\
= & \uparrow/ \cdot +/\ast \cdot \operatorname{segs} \\\\
= & \uparrow/ \cdot +/\ast \cdot +\\!\\!+ / \cdot \mathrm{tails} \ast \cdot \operatorname{inits} \\\\
= & \uparrow/ \cdot (\uparrow/ \cdot +/\ast \cdot \operatorname{tails})\ast \cdot \operatorname{inits} \\\\
= & \uparrow/ \cdot \odot \rightarrow\\!\\!\\!\\!\\!\\!/ _{\ 0} \ast \cdot \operatorname{inits} \\\\
= & \uparrow/ \cdot \odot \rightarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/ _{\ 0}
\end{align*}
$$

其中 $a \odot b = (a + b) \uparrow 0$.

这是一个线性的算法。在 Agda 中的实现与证明从略。

## 同态理论
半群到半群、幺半群到幺半群的同态即是数学上的定义。易见 $f^\ast$ 与 $\oplus/$ 是同态。

### 性质
{% admonition(type="theorem", title="Promotion Lemma") %}
$h$ 是 $(\alpha, \oplus, \mathrm{id} _\oplus)$ 到 $(\beta, \otimes, \mathrm{id} _\otimes)$ 的同态当且仅当：

$$h \cdot \oplus/ = \otimes/ \cdot h\ast$$
{% end %}

易见。实际上这是因为列表是自由的。

{% admonition(type="theorem", title="Existence Lemma") %}
列表函数 $h$ 是同态当且仅当总成立：

$$h\ v = h\ x \wedge h\ w = h\ y \implies h\ (v +\\!\\!+ w) = h\ (x +\\!\\!+ y)$$
{% end %}

从同态推下式是通过 $h = \odot/ \cdot f\ast$.

从下式推同态是因为存在 $g$ 使 $h = h \cdot g \cdot h$. 存在性可用集合论方法说明。

令 $t \odot u = h\ (g\ t +\\!\\!+ g\ u)$，从而 $h\ (x +\\!\\!+ y) = h\ x \odot h\ y$. 由条件知，与 $g$ 的具体取值无关，是良定义的。

---

对于列表我们有三种观点：
1. 列表要么是空的，要么是单的，要么是两个列表的拼接；对应计算方式是同态
2. 列表要么是空的，要么形如 $x +\\!\\!+ [a]$；对应计算方式是 `foldl`
3. 列表要么是空的，要么形如 $[a] +\\!\\!+ x$；对应计算方式是 `foldr`

{% admonition(type="theorem", title="Specialization Lemma") %}
每一个列表上的同态都可表达为 left/right reduction.
{% end %}

易见。

### 记号
存在非常丰富的同态的例子：
- 长度 $\\# = +/ \cdot (K\ 1)\ast$
- 排序 $\mathrm{sort} = \mathrm{merge}/ \cdot [\cdot]\ast$
- 是否全部满足条件 $\mathrm{all}\ p = \wedge/ \cdot p\ast$；相应地 $\mathrm{some}\ p = \vee/ \cdot p\ast$

---

我们定义记号 all applied to $[f, g, \cdots, h]^o a = [f\ a, g\ a, \cdots, h\ a]$. 定义 $h = (p \to f, g)$ 是指 $h\ x = \mathbf{if}\ p\ x\ \mathbf{then}\ f\ x\ \mathbf{else}\ g\ x$.

现在就可以定义 filter 如下：

$$p\triangleleft = +\\!\\!+/ \cdot (p \to [\mathrm{id}]^o, []^o)\ast$$

---

我们希望定义 $\uparrow _f$，使得 $x \uparrow _f y$ 的值是 $x, y$ 之一，且 $f\ (x \uparrow _f y) = f\ x \uparrow f\ y$. 例如，可以让 $\uparrow _{\\#}$ 是取两个列表中更长的那一个（长度相等时为保证一致可按字典序比较）。

我们规定 $\omega = \uparrow _{\\#}/ []$，有 $\\#\omega = -\infty$. 它是 $+\\!\\!+$ 的一个 zero. 这里我们称一个 $\omega$ 是 left zero 如果对任意 $a$ 都有 $\omega \oplus a = \omega$.

### 推导示例
{% admonition(type="question", title="Longest Segment Problem") %}
对一个序列，求满足性质 $p$ 的最长连续子段。
{% end %}

仍然从朴素的想法出发推导：

$$
\begin{align*} 
& \mathrm{lsp} \\\\
= & \uparrow _{\\#}/ \cdot (\mathrm{all}\ p)\triangleleft \cdot \operatorname{segs} \\\\
= & \uparrow _{\\#}/ \cdot (\uparrow _{\\#}/ \cdot (\mathrm{all}\ p)\triangleleft \cdot \operatorname{tails})\ast \cdot \operatorname{inits} \\\\
= & \uparrow _{\\#}/ \cdot (\uparrow _{\\#}/ \cdot (+\\!\\!+/ \cdot (p \to [\mathrm{id}]^o, K\ \omega)\ast) \ast \cdot \operatorname{tails})\ast \cdot \operatorname{inits} \\\\
= & \uparrow _{\\#}/ \cdot \odot \rightarrow\\!\\!\\!\\!\\!\\!/ _{\ []}\ast \cdot \operatorname{inits} \\\\
= & \uparrow _{\\#}/ \cdot \odot \rightarrow\\!\\!\\!\\!\\!\\!\\!/\\!\\!/ _{\ []}
\end{align*} 
$$

其中 $x \odot a = (x +\\!\\!+ (p\ a \to [a], \omega)) \uparrow _{\\#} []$.

{% admonition(type="question", title="The Minimax Problem") %}
优化 $\downarrow/ \cdot \uparrow/\ast$.
{% end %}

把同态改为 left reduction 即可。

## Fusion 与 Tupling
### 融合
{% admonition(type="theorem", title="Foldr Fusion Lemma") %}
若 $f\ (a \oplus r) = a \otimes f\ r$ 则：

$$f \cdot \oplus \leftarrow\\!\\!\\!\\!\\!\\!/ _e = \otimes \leftarrow\\!\\!\\!\\!\\!\\!/ _{f\ e}$$
{% end %}

其证明与应用易见。

### 元组化
考虑一个典型问题：找到列表中所有那些比后面的元素大的元素。在 reduction 时除结果外还需要记录最大值。

{% admonition(type="definition", title="Mutumorphism") %}
称 $f_1, \cdots, f_n$ 构成 mutumorphism 如果对每个 $f_i$ 有：

$$f_i\ [] = e_i$$
$$f_i\ [a] +\\!\\!+ x = a \oplus_i (f_1\ x, \cdots, f_n\ x)$$

此时我们将 $f\ x = (f_1\ x, \cdots, f_n\ x)$ 记作：

$$f = [\\![ (e_1, \cdots, e_n), (\oplus_1, \cdots, \oplus_n) ]\\!]$$
{% end %}

关于它有一些易见的性质，此处从略。

## 自动并行化
对列表来说，我们希望能够并行化指的是能够把问题 $h\ (x +\\!\\!+ y)$ 分解为 $h\ x \odot h\ y$. 典型的例子包括求和 `sum`，排序 `sort`.

### 第三同态定理
{% admonition(type="theorem", title="第三同态定理") %}
函数 $f$ 既可以被 `foldl` 又可以被 `foldr` 刻画，即：$h = \oplus \leftarrow\\!\\!\\!\\!\\!\\!/_e = \otimes \rightarrow\\!\\!\\!\\!\\!\\!/_e$，当且仅当存在 $\odot$ 使得 $h\ (x +\\!\\!+ y) = h\ x \odot h\ y$.
{% end %}

通过 Existence Lemma 证明，两边分别用 right-to-left reduction 与 left-to-right reduction 的性质。

### 构造方法
我们回顾 Existence Lemma 的证明方法，定义：

{% admonition(type="definition", title="Weak (Right) Inverse") %}
称 $g$ 是 $f$ 的 weak (right) inverse 如果对 $y \in \operatorname{Im} f$，有：

$$g\ y = x \implies f\ x = y$$
{% end %}

weak inverse 总是存在，但不一定唯一。

使用 weak inverse 可以给出 $f$ 的具体的并行化构造：令 $a \odot b = f\ (g\ x +\\!\\!+ g\ y)$. 对 `sum` 来说，取 $g\ y = [y]$ 即可。

我们来考虑这个问题：

{% admonition(type="question", title="Maximum Prefix Sum Problem") %}
对一个序列，求所有前缀和的最大值。
{% end %}

我们无法对它直接使用 weak inverse 的构造，因为它本身不能用 right-to-left reduction 刻画。但把它改造成 $f = \mathrm{mps} \triangle \mathrm{sum}$ 就可以了（这里 $f\ x = (\mathrm{mps}\ x, \mathrm{sum}\ x)$）。它的 weak inverse 是 $g\ (p, s) = [p, s-p]$.

我们回忆 Maximum Segment Sum Problem, 它可以与 Maximum Prefix Sum, Maximum Suffix Sum 与 `sum` 配对，现在问题变成了解（带条件）线性方程组构造 weak inverse. 这可以被 Mathematica 解决。可参见 PLDI’07. [^PLDI07]

### 在树上做
实际上在树上，如果可以 bottom-up 与 top-down 地进行刻画则也可以并行。可参见 POPL’09. [^POPL09]

## Maximum Marking Problems
### 定义
这是一大类问题。一般的形式为：

{% admonition(type="definition", title="Maximum Marking Problems") %}
有一个列表，满足某种性质 $p$ 地标记若干个元素，求被标记元素和的最大值。
{% end %}

一个朴素的想法是让：

$$\mathrm{mmp}\ p = \uparrow_{\mathrm{sum}}/ \cdot p\triangleleft \cdot \mathrm{gen}$$

其中 $\mathrm{gen}\ [a] = [\\![ (a, \mathrm{True}), (a, \mathrm{False}) ]\\!]$，$\mathrm{gen}\ x +\\!\\!+ y = \mathrm{gen}\ x X_{+\\!\\!+} \mathrm{gen}\ y$，这里 $X_\oplus$ 是叉积，取遍所有配对。

{% admonition(type="theorem", title="定理") %}
如果 $p = \mathrm{fst} \cdot h$，其中 $h$ 是值域有限的 right-to-left reduction, 那么可以找到一个 $O(|\operatorname{Im} h| \cdot n)$ 的算法。它形如 $\mathrm{mmp}\ p = \uparrow_{\mathrm{fst}}/ \cdot h'$，这里 $\mathrm{fst}$ 是取元组的第一个分量，$h'$ 也是 right-to-left reduction.
{% end %}

相关论文见于 ICFP’00. [^ICFP00]

### 推导示例
我们来考虑一个问题：

{% admonition(type="question", title="Maximum Independent Sublist Sum Problem") %}
$p$ 的要求是不能有相邻的标记。
{% end %}

可以写出 $h = p \triangle (\mathrm{marked} \cdot \operatorname{head})$. 优化结果从略（大致想法其实和多个变量递推的组合题差不多）。

### 延申
实际上存在更强的定理[^SAIG01]去处理更广泛的一类问题：

$$\mathrm{mmp'}\ p\ f\ k = \uparrow_{\mathrm{sum} \cdot f\ast}/ \cdot p\triangleleft \cdot \mathrm{gen}\ k$$

## Unfold
### unfold
`unfold` 是最简单有效的生成列表的计算模式。

```hs
unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p f g x = if p x then [] else f x : unfold p f g (g x)
```

举一个例子（实现 `upto (3, 6) = [3, 4, 5, 6]`）：
```hs
upto (m, n) = unfold fstGreater fst succFst (m, n)
  where fstGreater (x, y) = x > y
        succFst (x, y) = (x + 1, y)
```

又如，`map` 可以被定义为：
```hs
map f = unfold (== []) (f . head) tail
```

有时我们希望生成无限长列表（称为 stream）：
```hs
unfold_infinitely = unfold (const False)
```

### Hylomorphism
Hylomorphism 是在 unfold 之后 fold. 定义如下：

$$\mathrm{hylo}\ (\oplus, e)\ (p,f,g) = \mathrm{foldr}\ (\oplus)\ e \cdot \mathrm{unfold}\ p\ f\ g$$

这可以被递归地写为：

$$\mathrm{hylo}\ (\oplus, e)\ (p,f,g)\ x = \mathbf{if}\ p\ x\ \mathbf{then}\ e\ \mathbf{else}\ f\ x \oplus \mathrm{hylo}\ (\oplus, e)\ (p,f,g)\ (g\ x)$$

其实相当于是把 $(x:xs)$ 换成了 $x \oplus xs$.

### Metamorphism
Metamorphism 是在 fold 之后 unfold. 定义如下：

$$\mathrm{meta}\ (p,f,g)\ (\oplus, e) = \mathrm{unfold}\ p\ f\ g \cdot \mathrm{foldr}\ (\oplus)\ e$$

这适合用于表示一种数据表达方式到另一种的转换。

---

[^PLDI07]: 会议 Programming Language Design and Implementation 2007
[^POPL09]: 会议 Principles of Programming Languages 2009
[^ICFP00]: Isao Sasano, Zhenjiang Hu, Masato Takeichi, Mizuhito Ogawa, Make it Practical: A Generic Linear Time Algorithm for Solving Maximum Weightsum Problems, The 2000 ACM SIGPLAN International Conference on Functional Programming, (ICFP 2000), Montreal, Canada, 18-20 September 2000. ACM Press. pp. 137-149.
[^SAIG01]: 会议 Semantics, applications, and implementation of program generation 2001
