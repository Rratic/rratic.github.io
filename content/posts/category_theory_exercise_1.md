+++
title = "范畴论练习（一）"
description = "选做一些 Mac Lane 书上的习题（到 Ⅲ. Universals and Limits 的 1. Universal Arrows）。"
date = 2025-12-05
updated = 2025-12-08

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "范畴论"]
+++

<style>
	img:where(.dark,.dark *) {
        --tw-invert:invert(100%);
        filter: var(--tw-blur,)var(--tw-brightness,)var(--tw-contrast,)var(--tw-grayscale,)var(--tw-hue-rotate,)var(--tw-invert,)var(--tw-saturate,)var(--tw-sepia,)var(--tw-drop-shadow,)
    }
</style>

感觉之前并没有学明白，需要做一些习题。

采取的是 Saunders Mac Lane 的 *Categories for the Working Mathmatician* 第二版。此书包含许多不废话的习题。

交换图表图片是用 [Quiver](https://q.uiver.app/) 绘制的。

{% admonition(type="question", title="Ⅱ 3.2") %}
Show that the product of two preorders is a preorder.
{% end %}

一个预序 preorder 是指一个范畴，两个对象之间最多一个箭头，并且我们称 $p\leq p'$ 若有箭头 $p\to p'$，此时函子就是保序映射。

对预序 $P$ 与 $Q$，考虑 $R$ 以 $(p, q)$ 为对象，$(p, q)\leq (p', q')$ 如果 $p\leq_P p'$ 且 $q\leq_Q q'$，此时两个投射函子可由 $\pi_P((p, q))= p$ 与 $\pi_Q((p, q)) = q$ 自然地诱导出。$R$ 是一个预序。

若 $S$ 也有两个投射函子 $\varphi_P$ 与 $\varphi_Q$，那么 $\phi(s) = (\varphi_P(s), \varphi_Q(s))$ 自然地诱导出的函子使图表交换，使图表交换的条件推出它是唯一的。故 $R$ 确实是积。

{% admonition(type="question", title="Ⅱ 3.5") %}
Show that the ring of continuous real-valued functions on a topological space is the object function of a contravariant functor on $\mathbf{Top}$ to $\mathbf{Rng}$.
{% end %}

$\mathbf{Top}$ 是以 $\mathcal{U}$-小的拓扑空间为对象，以连续映射为态射的范畴；$\mathbf{Rng}$ 是以 $\mathcal{U}$-小的环为对象，以环同态（保幺元的）为态射的范畴。

首先它（记作 $F$）确实把拓扑空间映到环。现在看反变函子的 arrow function 部分是什么：

对连续映射 $\psi: T_1 \to T_2$ 及 $T_2$ 上的连续实值函数 $f: T_2 \to \mathbb{R}$，我们选取的环同态将 $f$ 对应到 $f\circ\psi$.

{% admonition(type="question", title="Ⅱ 4.1") %}
For $R$ a ring, describe $R-\mathbf{Mod}$ as a full subcategory of the functor category $\mathbf{Ab}^R$.
{% end %}

这里 $R-\mathbf{Mod}$ 是 $R$ 上的 $\mathcal{U}$-小的左模与同态（线性映射）构成的范畴，$\mathbf{Ab}$ 是 $\mathcal{U}$-小的交换群与同态构成的范畴，全子范畴 [full subcategory](https://ncatlab.org/nlab/show/full+subcategory) 是指若某两对象在子范畴中，则它们的所有箭头也在子范畴中。

把环看作范畴是：只有一个对象 $\ast$，环的元素都是 $\ast$ 到自身的箭头，乘法对应到复合。

设 $R \to \mathbf{Ab}$ 的函子 $f$ 将 $\ast$ 映到 $M$，则 $M$ 与 $f$ 的 arrow function 部分除了不满足环那一侧的线性性外构成左模；反之是可以取出的。

现在验证它是 full 的。对 $f$ 与 $g$ 是左模对应的函子和 $f \stackrel{\bullet}{\to} g$ 的自然变换，这个自然变换对 $\ast$ 给出一个 $M$ 到 $N$ 的群同态 $\phi$，成立环元素 $r$ 对应的图表交换式 $\phi \circ f(r) = g(r) \circ \phi$，从而确实是模同态。

{% admonition(type="question", title="Ⅱ 4.3") %}
Let $\mathbf{N}$ be the discrete category of natural numbers. Describe the functor category $\mathbf{Ab^N}$ (commonly known as the category of graded abelian groups).
{% end %}

看 $\mathbf{N} \to \mathbf{Ab}$ 的函子是什么。每个 $n$ 对应到一个交换群，且 $n$ 到自己的箭头对应到那个交换群的平凡自同构。验证和 [Graded](https://stacks.math.columbia.edu/tag/09MF) 所说相符。

{% admonition(type="question", title="Ⅱ 4.7") %}
Given categories $B$, $C$, and the functor category $B^\mathbf{2}$, show that each functor $H: C \to B^\mathbf{2}$ determines two functors $S, T: C \to B$ and a natural transformation $\tau: S \stackrel{\bullet}{\to} T$, and show this assignment $H\mapsto \langle S, T, \tau \rangle$ is a bijection.
{% end %}

这里 $\mathbf{2}$ 是指 $\bullet \to \bullet$，那么发现 $B^\mathbf{2}$ 可以看成是 the category of arrows of $B$.

我们把 $H$ 对应到 $S(c) = \operatorname{dom} H(c)$，$T(c) = \operatorname{cod} H(c)$ 及 $\tau(c) = H(c)$ 即可。

{% admonition(type="question", title="Ⅱ 5.1") %}
For small categories $A$, $B$, and $C$ establish a bijection

$$\mathbf{Cat}(A\times B, C) \cong \mathbf{Cat}(A, C^B)$$

and show it natural in $A$, $B$, and $C$. Hence show that $-\times B: \mathbf{Cat} \to \mathbf{Cat}$ has a right adjoint (see Chapter IX).
{% end %}

这里 $\mathbf{Cat}(X, Y)$ 是指以 $\mathcal{U}$-小范畴的范畴 $\mathbf{Cat}$ 中的对象 $X$，$Y$ 间的态射为对象的范畴，含义与 $Y^X$ 一样。

我们回顾对范畴 $A, B$，其积可以显示地表达为以 $\langle a, b\rangle$ 为对象，以 $\langle a\to a', b\to b'\rangle$ 为态射的范畴。那么对 $A\times B \to C$ 的函子 $f$ 满足，考虑 $g$ 把 $a$ 映到 $b\mapsto f(a, b)$ 是对应的函子。自然变换也是相对应的。

现在验证右伴随 [right adjoint](https://ncatlab.org/nlab/show/right+adjoint) 的定义，对 $F: \mathcal{C} \to \mathcal{D}$ 及 $G: \mathcal{D} \to \mathcal{C}$，称 $G$ 是 $F$ 的右伴随 $F \dashv G$，如果存在自然同构：

$$\Phi: \mathcal{D}(F(-), -) \stackrel{\cong}{\to} \mathcal{C}(-, G(-))$$

那么 $-^B$ 是 $-\times B$ 的右伴随。

{% admonition(type="question", title="Ⅱ 5.5") %}
(Hilton-Eckmann). Let $S$ be a set with two (everywhere defined) binary operations $\cdot: S\times S\to S$, $\circ: S\times S\to S$ which both have the same (two-sided) unit element $e$ and which satisfy the interchange identity $(\tau'\cdot\sigma')\circ(\tau\cdot\sigma) = (\tau'\cdot\tau)\circ(\sigma'\cdot\sigma)$. Prove that $\cdot$ and $\sigma$ are equal, and that each is commutative.
{% end %}

取 $\sigma = \sigma' = e$ 就有 $\cdot \equiv \circ$，交换性也易知。

{% admonition(type="question", title="Ⅱ 5.6") %}
Combine Exercise 4 and 5 to prove that the fundamental group of a topological group is abelian.
{% end %}

在 (4) 中 $\circ$ 是指路径的粘合，$\cdot$ 是指 pointwise product 逐点乘积，即 $(\tau\cdot\sigma)(t) = \tau(t)\sigma(t)$，因为拓扑群是有乘法的。

它们将满足 (5) 条件，从而 $\circ$ 是交换的，从而基本群是交换的。

{% admonition(type="question", title="Ⅱ 6.2") %}
If $t$ is a terminal object in $C$, prove that $(C\downarrow t)$ is isomorphic to $C$.
{% end %}

由于是终对象，每一个 $C$ 中的对象 $c$ 唯一对应到 $(C\downarrow t)$ 中的对象 $c\to t$.

{% admonition(type="question", title="Ⅱ 6.5") %}
Given any commutative diagram of categories and functors

![逗号范畴-别](/images/diagram/comma_category_another.png)

(bottom row as in

![逗号范畴](/images/diagram/comma_category.png)

![逗号范畴-解释](/images/diagram/comma_category_expl.png)

), prove that there is a unique functor $L: X \to (T\downarrow S)$ for which $P'=PL$, $Q'=QL$ and $R'=RL$. (This describes $(T\downarrow S)$ as a "pull-back", cf. §Ⅲ.4.)
{% end %}

对 $x\in \mathrm{Ob}(X)$，取 $e$ 是 $P'(x)$，取 $d$ 是 $Q'(x)$，取 $f$ 是通过使得 $R\circ L = R'$，然后验证。

{% admonition(type="question", title="Ⅱ 7.2") %}
Show that every finite ordinal number is a free category.
{% end %}

这里有限序数是指把自然数 $n$ 看作 $\\{0, 1, \cdots, n-1\\}$，即集合论中无穷公理给的那个构造，并令 $i \to j$ 若 $i \leq j$.

易见它是由图 $0 \to 1 \to \cdots \to n-1$ 生成的自由范畴。

{% admonition(type="question", title="Ⅲ 1.1") %}
Show how each of the following familiar constructions can be interpreted as a universal arrow:
1. The integral group ring of a group (better, of a monoid).
2. The tensor algebra of a vector space.
3. The exterior algebra of a vector space.
{% end %}

回忆一个幺半群上的整群环 integral group ring $\mathbb{Z}[M]$ 是指：取元素为形式有限和 $\sum_{g\in G} a_g g$，其中 $a_g\in\mathbb{Z}$，并令加法是按分量，乘法是：

$$\left(\sum_{g\in G} a_g g\right)\left(\sum_{h\in G} b_h h\right) = \sum_{g, h\in G} (a_gb_h) (gh)$$

考虑把环映到乘法幺半群的遗忘函子 $U: \mathbf{Ring} \to \mathbf{Mon}$，有：

![整群环的泛态射表述](/images/diagram/universal_integral_group_ring.png)

---

回忆 $F$-向量空间 $V$ 上的张量代数是指：

$$TV = \bigoplus_{p=0}^\infty V^{\otimes n}$$

其中：

$$V^{\otimes n} = \underbrace{V \otimes\cdots \otimes V}_n$$

在 $n = 0$ 时定义为 $F$.

取乘法是类似于多项式乘法地进行 $\otimes: V^{\otimes n}\times V^{\otimes m} \to V^{\otimes (n+m)}$，这给出了分级的 $F$-代数/结合代数。

考虑忘却 $F$-代数的结构的遗忘函子 $U: \mathbf{Alg}_F \to \mathbf{Vect}_F$，有：

![张量代数的泛态射表述](/images/diagram/universal_tensor_algebra.png)

这是因为对 $v\in V$，有 $f'((0, (v), \cdots)) = f(v)$，而 $TV$ 中的元素均可由所有 $(0, (v), \cdots)$ 经乘法与加法生成，这保证了唯一性。而张量代数不引入额外的等同关系，保证了存在性。

---

回忆 $F$-向量空间 $V$ 上的外代数是指：对张量代数商去所有形如 $v\otimes v$ 的元素生成的双边理想。

类似于 (2) 的 $\mathbf{Alg}_F$，讨论的是忘却反交换 $F$-代数的结构的遗忘函子 $U: \mathbf{AltAlg}_F \to \mathbf{Vect}_F$.

{% admonition(type="question", title="Ⅲ 1.2") %}
Find a universal element for the contravariant power set function $\mathscr{P}: \mathbf{Set}^\mathrm{op} \to \mathbf{Set}$.
{% end %}

这里 $\mathscr{P}$ 将 $X$ 映到 $\\{S | S \subseteq X\\}$，将 $f^\mathrm{op}: Y \to X$ 映到 $S \mapsto f^{-1}[S]$.

我们取 $r = \\{a\\}$ 及 $e = \\{a\\}$ 即可。此时 $f$ 的选取方式是，若在 $x$ 中则映到 $a$，否则映到 $b$.

![反变幂集函子的泛元素](/images/diagram/universal_power_set.png)

{% admonition(type="question", title="Ⅲ 1.4") %}
Use only universality (of projections) to prove the following isomorphisms of group theory:
1. For normal subgroups $M$, $N$ of $G$ with $M \subset N$, $(G/M)/(N/M) \cong (G/M)$.
2. For subgroups $S$ and $N$ of $G$, $N$ normal, with join $SN$, $SN/N \cong S/S\cap N$.
{% end %}

先来考虑商群的泛性质是什么。对 $p: G \to G/N$，对 $N\subseteq \ker \varphi$，有：

![商群的泛性质](/images/diagram/universal_quotient_group.png)

对 (1) 来说，考虑下图。如果我没有搞错的话，它是不言自明的。然后使用一般的技术（分别放到 $H$ 的位置上）就可说明同构。

![第二同构定理](/images/diagram/group_second_isomorphism.png)

对 (2) 来说，考虑下图。其中 $\tau$ 是自然的嵌入。

![第三同构定理](/images/diagram/group_third_isomorphism.png)
