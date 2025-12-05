+++
title = "范畴论练习（一）"
description = "选做一些 Mac Lane 书上的习题。"
date = 2025-12-05

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "范畴论"]
+++

感觉之前并没有学明白，需要做一些习题。

采取的是 Saunders Mac Lane 的 *Categories for the Working Mathmatician* 第二版。此书包含许多不废话的习题。

{% admonition(type="question", title="II 1-3 2") %}
Show that the product of two preorders is a preorder.
{% end %}

一个预序 preorder 是指一个范畴，两个对象之间最多一个箭头，并且我们称 $p\leq p'$ 若有箭头 $p\to p'$，此时函子就是保序映射。

对预序 $P$ 与 $Q$，考虑 $R$ 以 $(p, q)$ 为对象，$(p, q)\leq (p', q')$ 如果 $p\leq_P p'$ 且 $q\leq_Q q'$，此时两个投射函子可由 $\pi_P((p, q))= p$ 与 $\pi_Q((p, q)) = q$ 自然地诱导出。$R$ 是一个预序。

若 $S$ 也有两个投射函子 $\varphi_P$ 与 $\varphi_Q$，那么 $\phi(s) = (\varphi_P(s), \varphi_Q(s))$ 自然地诱导出的函子使图表交换，使图表交换的条件推出它是唯一的。故 $R$ 确实是积。

{% admonition(type="question", title="II 1-3 5") %}
Show that the ring of continuous real-valued functions on a topological space is the object function of a contravariant functor on $\mathbf{Top}$ to $\mathbf{Rng}$.
{% end %}

$\mathbf{Top}$ 是以 $\mathcal{U}$-小的拓扑空间为对象，以连续映射为态射的范畴；$\mathbf{Rng}$ 是以 $\mathcal{U}$-小的环为对象，以环同态（保幺元的）为态射的范畴。

首先它（记作 $F$）确实把拓扑空间映到环。现在看反变函子的 arrow function 部分是什么：

对连续映射 $\psi: T_1 \to T_2$ 及 $T_2$ 上的连续实值函数 $f: T_2 \to \mathbb{R}$，我们选取的环同态将 $f$ 对应到 $f\circ\psi$.

{% admonition(type="question", title="II 4 1") %}
For $R$ a ring, describe $R-\mathbf{Mod}$ as a full subcategory of the functor category $\mathbf{Ab}^R$.
{% end %}

这里 $R-\mathbf{Mod}$ 是 $R$ 上的 $\mathcal{U}$-小的左模与同态（线性映射）构成的范畴，$\mathbf{Ab}$ 是 $\mathcal{U}$-小的交换群与同态构成的范畴，全子范畴 [full subcategory](https://ncatlab.org/nlab/show/full+subcategory) 是指若某两对象在子范畴中，则它们的所有箭头也在子范畴中。

把环看作范畴是：只有一个对象 $\ast$，环的元素都是 $\ast$ 到自身的箭头，乘法对应到复合。

设 $R \to \mathbf{Ab}$ 的函子 $f$ 将 $\ast$ 映到 $M$，则 $M$ 与 $f$ 的 arrow function 部分除了不满足环那一侧的线性性外构成左模；反之是可以取出的。

现在验证它是 full 的。对 $f$ 与 $g$ 是左模对应的函子和 $f \stackrel{\bullet}{\to} g$ 的自然变换，这个自然变换对 $\ast$ 给出一个 $M$ 到 $N$ 的群同态 $\phi$，成立环元素 $r$ 对应的图表交换式 $\phi \circ f(r) = g(r) \circ \phi$，从而确实是模同态。

{% admonition(type="question", title="II 4 3") %}
Let $\mathbf{N}$ be the discrete category of natural numbers. Describe the functor category $\mathbf{Ab^N}$ (commonly known as the category of graded abelian groups).
{% end %}

看 $\mathbf{N} \to \mathbf{Ab}$ 的函子是什么。每个 $n$ 对应到一个交换群，且 $n$ 到自己的箭头对应到那个交换群的平凡自同构。验证和 [Graded](https://stacks.math.columbia.edu/tag/09MF) 所说相符。

{% admonition(type="question", title="II 4 7") %}
Given categories $B$, $C$, and the functor category $B^\mathbf{2}$, show that each functor $H: C \to B^\mathbf{2}$ determines two functors $S, T: C \to B$ and a natural transformation $\tau: S \stackrel{\bullet}{\to} T$, and show this assignment $H\mapsto \langle S, T, \tau \rangle$ is a bijection.
{% end %}

这里 $\mathbf{2}$ 是指 $\bullet \to \bullet$，那么发现 $B^\mathbf{2}$ 可以看成是 the category of arrows of $B$.

我们把 $H$ 对应到 $S(c) = \operatorname{dom} H(c)$，$T(c) = \operatorname{cod} H(c)$ 及 $\tau(c) = H(c)$ 即可。

{% admonition(type="question", title="II 5 1") %}
For small categories $A$, $B$, and $C$ establish a bijection

$$\mathbf{Cat}(A\times B, C) \cong \mathbf{Cat}(A, C^B)$$

and show it natural in $A$, $B$, and $C$. Hence show that $-\times B: \mathbf{Cat} \to \mathbf{Cat}$ has a right adjoint (see Chapter IX).
{% end %}

这里 $\mathbf{Cat}(X, Y)$ 是指以 $\mathcal{U}$-小范畴的范畴 $\mathbf{Cat}$ 中的对象 $X$，$Y$ 间的态射为对象的范畴，含义与 $Y^X$ 一样。

我们回顾对范畴 $A, B$，其积可以显示地表达为以 $\langle a, b\rangle$ 为对象，以 $\langle a\to a', b\to b'\rangle$ 为态射的范畴。那么对 $A\times B \to C$ 的函子 $f$ 满足，考虑 $g$ 把 $a$ 映到 $b\mapsto f(a, b)$ 是对应的函子。自然变换也是相对应的。

现在验证右伴随 [right adjoint](https://ncatlab.org/nlab/show/right+adjoint) 的定义，对 $F: \mathcal{C} \to \mathcal{D}$ 及 $G: \mathcal{D} \to \mathcal{C}$，称 $G$ 是 $F$ 的右伴随 $F \dashv G$，如果存在自然同构：

$$\Phi: \mathcal{D}(F(-), -) \stackrel{\cong}{\to} \mathcal{C}(-, G(-))$$

那么 $-^B$ 是 $-\times B$ 的右伴随。

{% admonition(type="question", title="II 5 5") %}
(Hilton-Eckmann). Let $S$ be a set with two (everywhere defined) binary operations $\cdot: S\times S\to S$, $\circ: S\times S\to S$ which both have the same (two-sided) unit element $e$ and which satisfy the interchange identity $(\tau'\cdot\sigma')\circ(\tau\cdot\sigma) = (\tau'\cdot\tau)\circ(\sigma'\cdot\sigma)$. Prove that $\cdot$ and $\sigma$ are equal, and that each is commutative.
{% end %}

取 $\sigma = \sigma' = e$ 就有 $\cdot \equiv \circ$，交换性也易知。

{% admonition(type="question", title="II 5 6") %}
Combine Exercise 4 and 5 to prove that the fundamental group of a topological group is abelian.
{% end %}

在 (4) 中 $\circ$ 是指路径的粘合，$\cdot$ 是指 pointwise product 逐点乘积，即 $(\tau\cdot\sigma)(t) = \tau(t)\sigma(t)$，因为拓扑群是有乘法的。

它们将满足 (5) 条件，从而 $\circ$ 是交换的，从而基本群是交换的。
