+++
title = "一些简单定义的范畴论观点"
date = 2025-08-02
updated = 2026-11-09

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "范畴论"]
+++

范畴论给予了我们一个统一的框架去处理代数或者其它邻域的一些普适性质。这里讨论一些常见的定义在范畴论中的翻译是什么。

<!-- more -->

## 映射
容易想见，我们说 $f: X \to Y$ 是**同构**，如果存在 $g: Y \to X$ 使得 $f \circ g = \mathrm{id}_Y$ 且 $g \circ f = \mathrm{id}_X$，对于 $\mathcal U$-小的范畴（且选择公理成立），我们可以从每个等价类选取一个对象，保留态射，得到的称为范畴的**骨架**（skeleton）。

我们称 $f: X \to Y$ 是**单态射**（monomorphism）$f: X \hookrightarrow Y$，如果左消去律成立。即对任意 $g, h: T \to X$ 有 $f \circ g = f \circ h$ 蕴含 $g = h$，或者说 $f^\ast: \mathrm{Hom}(T, X) \to \mathrm{Hom}(T, Y)$ 总是单。右消去律成立的 $f: X \twoheadrightarrow Y$ 为**满态射**（epimorphism）。

若范畴中某个对象满足 $\mathbf 1$：对任意对象 $X$ 存在唯一箭头 $\mathbf 1 \to X$，则称它为**始对象**（initial object）；其对偶即**终对象**（terminal object）；既是始对象又是终对象的称为**零对象**（null object），记作 $\mathbf 0$，易见 $\mathbf 0 \to X$ 单而 $X \to \mathbf 0$ 满。

考虑一列单态射 $f_i: S_i \hookrightarrow$，若存在 $g: S_1 \to S_2$ 使图表交换，则记 $(S_1, f_1) \subseteq (S_2, f_2)$，若 $S_1 \subseteq S_2$ 且 $S_2 \subseteq S_1$ 则称它们等价。这样的等价类称为 $X$ 的**子对象**（subobject），构成偏序，$\mathrm{id}_X$ 给出极大元。其对偶即**商对象**（quotient object）。

## 积与余积
代数结构的积以笛卡尔积为基础。对两个集合 $A$，$B$，我们熟知 $A\times B = \set{(a, b) | a\in A, b\in B}$，其中 $(a, b)$ 在集合论上严格地被定义为 $\set{\set{a}, \set{a, b}}$，读者可验证我们确实可以从 $(a, b)$ 中提取出 $a$ 与 $b$ 来。

除笛卡尔积之外，我们有无交并。对实际上有交的集合，无交并在技术上可以定义为 $A\sqcup B = \set{(0, a) | a\in A}\cup \set{(1, b) | b\in B}$，其中记 $0 = \emptyset$ 及 $1 = \set{\emptyset}$.

{% <definition title="积"> %}
对范畴 $\mathcal{C}$，称 $P$ 是一族 $\\{X_i\\}_{i\in I}$ 的**积**，如果存在一族态射 $\pi_i: P\to X_i$，使得对任一族态射 $\varphi_i: Y\to X_i$，存在唯一的 $\phi: Y\to P$ 使 $\pi_i \circ \phi = \varphi_i$.
{% </definition> %}

自然地，可以在 $I = \emptyset$ 补充定义空积为终对象。

易知积在同构意义下是唯一的。

由于这个泛性质，对于 $f: Y \to A$ 与 $g: Y \to B$，可以用记号 $\braket{f, g}$ 表示那个用泛性质得到的 $Y \to A \times B$ 态射。

{% <note title="关于嵌入的诱导"> %}
在群范畴中，我们不仅有投影 $\pi_i$，还能自然地找到 $i_A: A \to A \times B$ 这样的嵌入。

实际上当范畴有零对象时，任意 $A, B$ 可以找到一个 $O_{AB}: A \to B$ 是 $B \to \mathbf 0$ 与 $\mathbf 0 \to A$ 那两个唯一映射的复合。使用 $\mathrm{id} _A: A \to A$ 及 $O _{AB}: A \to B$，就有 $i _A = \braket{\mathrm{id} _A, O _{AB}}$ 是所求。
{% </note> %}

{% <note title="关于箭头的方向"> %}
如果把“存在唯一的 $\phi: Y\to P$”改为“存在唯一的 $\phi: P \to Y$”会如何？对于有始对象的范畴，实际上 $P$ 取为始对象总是满足！
{% </note> %}

积的对偶是**余积**（或称**上积**，上的译法可能来自画图时的方向；或称**和**），即：

{% <definition title="余积"> %}
称 $P$ 是一族 $\\{X_i\\}_{i\in I}$ 的余积，如果存在一族态射 $\iota_i: X_i\to P$，使得对任一族态射 $\varphi_i: X_i\to Y$，存在唯一的 $\phi: P\to Y$ 使 $\phi \circ \iota_i = \varphi_i$.
{% </definition> %}

$I = \emptyset$ 时定义空积为始对象。

特别地，如果一族对象的积与余积相同，则称为双积。

读者可自行验证以下对应：

| 范畴 | 积 | 余积 |
| :-: | :-: | :-: |
| 集合范畴 | 笛卡尔积 | 无交并 |
| 向量空间范畴 | 直积 | 直和 |
| 群范畴 | 直积 | 自由积 |
| 交换群范畴 | 直积 | 直和 |
| 交换环范畴 | 直积 | 张量积 $\otimes_{\Z}$ |
| 拓扑空间范畴 | 积空间 | 无交并 |

## 拉回与推出
在拓扑中，对一个拓扑空间 $B$，一个 $B$ 上的空间（或称 $B$-空间）是指拓扑空间 $X$ 和连续映射 $p: X \to B$，可以将它看成一族空间 $X_b = p^{-1}(b)$（称作点 $b$ 的纤维）。两个 $B$-空间的连续 $B$-映射是满足 $p'\circ f = p$ 的连续映射 $f: X\to X'$.

对两个 $D$-空间 $(B, f: B\to D)$ 与 $(C, g: C\to D)$，它们的纤维积大概是：对 $d\in D$，考虑 $B_d\times C_d$，将它们拼成一个大空间，这也是一个 $D$-空间。

{% <definition title="拉回"> %}
对于对象 $B, C, D$ 与态射 $B\to D, C\to D$，它们的**拉回**/纤维积 $B \times_D C$ 是对象 $A$ 及态射 $A\to B, A\to C$，满足泛性质：对另一组对象 $A'$ 及态射 $A'\to B, A'\to C$，存在唯一的态射 $A'\to A$ 使图表交换。
{% </definition> %}

读者容易写出两个集合的拉回是 $\set{(b, c) \in B\times C | f(b) = g(c)}$，因此一个集合的两个子集的拉回是它们的交。

读者可验证，在下图中，如果两个小方块是拉回，那么大矩形也是拉回；如果大矩形和右侧方块是拉回，那么左侧方块也是拉回。

$$
\begin{CD}
	\bullet @>>> \bullet @>>> \bullet \cr
	@VVV @VVV @VVV \cr
	\bullet @>>> \bullet @>>> \bullet
\end{CD}
$$

拉回的对偶是**推出**/余纤维积，记作 $B \sqcup_A C$.

{% <definition title="等化子"> %}
对 $f, g: A \to B$，称对象 $E$ 及态射 $e: E \to A$ 为它们的**等化子** $\ker (f, g)$，如果 $f\circ e = g\circ e$，且对任意满足同样性质的 $z: Z \to A$ 有唯一的态射 $Z \to E$ 使图表交换。
{% </definition> %}

观察图表即可发现，$f: B \to D$ 与 $g: C \to D$ 的拉回是 $f \circ \pi_B, g \circ \pi_C: B \times C \rightrightarrows D$ 的等化子；与此同时等化子是 $\braket{1_A, f}, \braket{1_A, g}: A \to A \times B$ 的拉回。

等化子的对偶是**余等化子**，记作 $\mathrm{coker}(f, g)$.

{% <definition title="核"> %}
对零对象 $\mathbf{0}$，态射 $f: X \to Y$ 的**核** $\ker f$ 为 $f$ 与 $u: \mathbf{0} \to Y$ 的拉回。
{% </definition> %}

采用之前的等价，可以发现 $\ker f$ 可以看成 $f$ 与 $O_{XY}$ 的等化子。

核的对偶是**余核**，记作 $\operatorname{coker} f$.

{% <definition title="像"> %}
设 $Y \sqcup_X Y$ 存在，态射 $f: X \to Y$ 的**像** $\operatorname{im} f$ 是等化子 $\ker (Y \rightrightarrows Y \sqcup_X Y)$.
{% </definition> %}

像带有典范的单态射 $\operatorname{im} f \hookrightarrow Y$.

像的对偶是**余像**，记作 $\operatorname{coim} f$，易见存在唯一态射 $\operatorname{coim} f \to \operatorname{im} f$ 使下图与 $f$ 交换：

$$X \twoheadrightarrow \operatorname{coim} f \to \operatorname{im} f \hookrightarrow Y$$

当像、余像存在，且该态射是同构时，称 $f$ 是**严格态射**。集合范畴中所有态射都严格。对严格态射可以作满-单分解，故，一个严格态射是同构当且仅当它既单又满。
