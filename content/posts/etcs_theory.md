+++
title = "ETCS 理论简介"
description = "集合范畴基本理论的公理简介。"
date = 2026-05-12

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "基石"]
+++

ETCS 是 Lawvere 的集合范畴基本理论[^etcs-1] [^etcs-2]（Elementary Theory of the Category of Sets）。

对此会有一个误解是认为其基本动机是用范畴论来取代集合理论，其实不然，它就是集合论。这些公理是受范畴启发的，并不依赖于拥有一个一般的范畴的定义。

本文参考了 [*Rethinking Set Theory*](https://www.tandfonline.com/doi/abs/10.4169/amer.math.monthly.121.05.403) 及其[中译](https://chaoli.club/index.php/7082/0/)。

---

不同于一般公理化使用的集合和元素，我们使用集合和函数。假设存在单点集 $\mathbf{1} = \set{\bullet}$, 那么一个函数 $\mathbf{1} \to X$ 本质上是 $X$ 的一个元素。这并没有看起来的那么 trivial. 在此种观点下，求值可以看成函数的复合。

{% admonition(type="info", title="公理 1") %}
函数复合满足结合律、存在单位元。
{% end %}

后者即，对任一集合 $X$ 存在一个函数 $\mathbf{1}_X: X \to X$ 使得对所有 $g: X \to Y$ 有 $g \circ \mathbf{1}_X = g$; 对所有 $f: W \to X$ 有 $\mathbf{1}_X \circ f = f$. 从中可知唯一性。

{% admonition(type="info", title="公理 2") %}
存在一个 terminal set $T$, 即满足对任意 $X$ 都有唯一函数 $X \to T$.
{% end %}

放在一般的理论中，也就是集合范畴的终对象，这一点读者应当很熟悉了。

{% admonition(type="info", title="公理 3") %}
存在一个没有元素的集合。
{% end %}

{% admonition(type="info", title="公理 4") %}
对 $f, g: X \to Y$, 若对所有 $x \in X$ 都有 $f(x) = g(x)$, 那么 $f = g$.
{% end %}

关于等号在理论中处于哪一层，读者可参考[一阶逻辑](@/posts/logic_4.md)。

{% admonition(type="info", title="公理 5") %}
每对集合都有一个积。
{% end %}

积不仅包含一个集合，还有两个投影映射。积的定义即范畴论中的定义。

两个积是唯一同构的，故我们在讨论时假定一个优选的 $(X \times Y, \mathrm{pr}_1^{X,Y}, \mathrm{pr}_2^{X,Y})$ 是无碍的（或者直接将此运算放在原始概念列表中）。

对 $f: I \to X \times Y$, 可以与两个投影映射合成得到 $f_1$ 与 $f_2$.

{% admonition(type="info", title="公理 6") %}
对于所有集合 $X, Y$, 存在一个从 $X$ 到 $Y$ 的函数集。
{% end %}

函数集是指集合 $F$ 与函数 $\epsilon: F \times X \to Y$, 满足：对任意集合 $I$ 和函数 $q: I \times X \to Y$ 存在唯一的函数 $\bar{q}: I \to F$ 使得对所有 $t \in I, x \in X$ 有 $q(t, x) = \epsilon(\bar{q}(t), x)$.

{% admonition(type="info", title="公理 7") %}
对任意函数 $f: X \to Y$ 及元素 $y \in Y$ 存在 $f$ 下 $y$ 的逆像。
{% end %}

逆像是集合 $A$ 和一个函数 $j: A \to X$ 使得 $f(j(a)) = y, \forall a \in A$. 对同样满足此的 $I$ 和 $q: I \to X$ 存在唯一的函数 $\bar{q}: I \to A$ 使 $q = j \circ \bar{q}$.

{% admonition(type="info", title="公理 8") %}
存在一个子集分类器（subset classifier）。
{% end %}

首先定义嵌入映射是指 $j: A \to X$ 满足对于 $a, a' \in A$ 有 $j(a) = j(a') \implies a = a'$.

一个子集分类器是一个集合 $\mathbf{2}$ 和元素 $t \in 2$ 满足：对所有集合 $A, X$ 与嵌入映射 $j: A \to X$ 存在唯一的函数 $\chi:X \to \mathbf{2}$ 使得 $j$ 是 $\chi$ 下 $t$ 的逆像。

$$
\begin{CD}
   A @>>> \mathbf{1} \cr
   @VVjV @VVtV \cr
   X @>>\chi> \mathbf{2}
\end{CD}
$$

读者可以看出 $\mathbf{2}$ 中元素表意为真假。定义中没有提及它必须有两个元素，但十条公理实际上暗示了这一点。

{% admonition(type="info", title="公理 9") %}
存在一个自然数系。
{% end %}

一个自然数系是集合 $N$ 与其中元素 $0$ 及一个函数 $s: N \to N$ 满足：对集合 $X$ 与 $a \in X, r: X \to X$ 存在唯一的函数 $x: N \to X$ 使得 $x(0) = a$ 且 $x(s(n)) = r(x(n))$.

$$
\begin{CD}
   \mathbf{1} @>>0> N @>>s> N \cr
   @VV\mathbf{1_1}V @VVxV @VVxV \cr
   \mathbf{1} @>>a> X @>>r> X
\end{CD}
$$

读者易见 $s$ 即是后继。

{% admonition(type="info", title="公理 10") %}
每个满射都有一个右逆。
{% end %}

满射与右逆的定义显然。

---

通过一些麻烦的方式，我们可以用上述公理定义商 $X/\sim$ 与无交并 $X \sqcup Y$. 进而定义我们熟悉的 $\Z, \mathbb{Q}, \R$ 等。

大多数数学家永远不会使用比十条公理所保证的集合更多的性质。例如，McLarty 认为，在经典多卷作品 Éléments de Géométrie Algébrique (EGA) 和 Séminaire de Géométrie Algébrique (SGA) 中，任何地方都不再需要更多。

让我们考虑无限的笛卡尔积：能否形成积 $\prod _{i \in I} X_i$. 这取决于集族 $(X _i) _{i \in I}$ 的含义。我们可以将它看作 $p: X \to I$, 那么 $\prod _{i \in I} X _i$ 就是在 $p^I: X^I \to I^I$ 下 $\mathbf{1} _I$ 的逆像。

然而，十条公理没有保证无交并的存在：

$$\N \sqcup \mathscr{P}(\N) \sqcup \mathscr{P}(\mathscr{P}(\N)) \sqcup \cdots$$

我们可以额外添加第十一条公理，称为“替换”，大致说：对集合 $I$ 和一个对 $i \in I$ 在同构意义下指定集合 X_i 的一阶公式，存在集合 $X$ 与函数 $p: X \to I$ 使得对每个 $i \in I$ 有 $p^{-1}(i) \simeq X_i$.

原本的十条公理弱于 ZFC；而加入第十一个时，两个理论的强度相等。

---

[^etcs-1]: F. W. Lawvere, "An Elementary Theory of the Category of Sets," *Proceedings of the National Academy of Sciences of the U.S.A.* 52 (1964): 1506-1511.
[^etcs-2]: F. W. Lawvere, "An Elementary Theory of the Category of Sets (Long Version) with Commentary," *Reprints in Theory and Applications of Categories* 12 (2005): 1-35.
