+++
title = "从单形到无穷范畴理论"
date = 2026-08-09

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "范畴论", "拓扑学"]
+++

本文主要为 [∞-type Café 暑期学校 2026](https://infinity-type-cafe.github.io/ntype-cafe-summer-school-2026/) 的“无穷范畴”课程前两次笔记，参考了 [nLab](https://ncatlab.org/) 的资料及答疑群的回答。

<!-- more -->

<style>
	img:where(.dark,.dark *) {
        filter: invert(100%);
    }
</style>

无穷范畴用在类型论作为数学基础上，还体现了这样一种哲学诠释：如果一个命题有两个证明，而我们无法看出它们的关联，则没有真正理解它。

{% quote(by = "Stefan Banach") %}
A mathematician is a person who can find analogies between theorems; a better mathematician is one who can see analogies between proofs and the best mathematician can notice analogies between theories. One can imagine that the ultimate mathematician is one who can see analogies between analogies.
{% end %}

此诠释正文中不再赘述。

## 理论基础
### 单纯集的范畴
{% admonition(type="definition", title="单形范畴（simplex category）") %}
单形范畴 $\Delta$ 的对象是有限的 linear directed graph 的代表元。简单来说对 $n \in \N$ 我们取（对 $i \leq j$ 给予箭头 $i \to j$）：

$$[n] = \set{0 \leq 1 \leq \cdots \leq n}$$

并其态射 $[m] \to [n]$ 是此观点下的保序映射。
{% end %}

有两类典型的态射。面映射（face map）$\delta_i: [n-1] \to [n]$ 是：

$$\delta_i(k) = \begin{cases} k & k < i \cr k + 1 & k \geq i \end{cases}$$

退化映射（degeneracy map）$\sigma_i: [n+1] \to [n]$ 是：

$$\sigma_i(k) = \begin{cases} k & k \leq i \cr k - 1 & k > i \end{cases}$$

容易发现实际上所有的态射由它们生成。

{% admonition(type="definition", title="预层（presheaf）") %}
对小范畴 $\mathcal C$，其预层是一个 $\mathcal C \to \mathbf{Set}$ 的反变函子，也就是一个 $F: \mathcal C^{\mathrm{op}} \to \mathbf{Set}$. 以预层为对象，自然变换为态射的预层范畴在本文记作 $P(\mathcal C)$.
{% end %}

易见对 $x \in \mathrm{Ob}(\mathcal C)$ 有一个可表预层是：

$$\mathrm{Hom}_{\mathcal C}(-, x): \mathcal C^{\mathrm{op}} \to \mathbf{Set}$$

{% admonition(type="definition", title="单纯集（simplicial set）") %}
单纯集是指 $\Delta$ 上的一个预层。
{% end %}

实际上，单纯集是单纯复形的概念推广，一个单纯集 $X$ 包含的信息是：
- 对每个 $n \in \N$ 分配一个 $X_n = X([n])$，意味着 $n$-单形的集合
- 对每个单射的 $\delta_i: [n-1] \to [n]$ 分配一个 $d_i: X_n \to X_{n-1}$
- 对每个满射的 $\sigma_i: [n+1] \to [n]$ 分配一个 $s_i: X_n \to X_{n+1}$

其中 $d_i, s_i$ 满足 [simplicial identities](https://ncatlab.org/nlab/show/simplicial+identities).

参考以下来自 [nLab: simplicial set](https://ncatlab.org/nlab/show/simplicial+set) 的示意图：

![simplicial set](/images/misc/2026_08_08.jpg)

我们取 $\Delta^n \in P(\Delta)$ 是 $\mathrm{Hom}_\Delta(-, [n])$，则：

$$X \cong \operatorname*{colimit}_{[n] \in \Delta \downarrow X} \Delta^n$$

这里[余极限](@/posts/category_theory_exercise_2.md)体现的是粘合。回忆 $J = \set{0 \to 1 \to 2 \to \cdots}$ 及 $F_1 \subseteq F_2 \subseteq \cdots$ 时 $\operatorname{colimit} F = \bigcup_i F_i$ 体现的“取整体”意。$\Delta \downarrow X$ 的对象是 $([n] \in \Delta, \alpha: \Delta^n \to X)$，后者依 Yoneda 引理对应到 $X_n$ 的元素。读者可自行验证等式成立。

### 单纯范畴
{% admonition(type="theorem", title="引理") %}
对 $X \in \mathrm{Ob}(P(\Delta))$，将 $Y$ 打到 $X \times Y$ 的函子 $P(\Delta) \to P(\Delta)$ 有一个右伴随 $\underline{\mathrm{Hom}}(X, -): P(\Delta) \to P(\Delta)$，由下式决定：

$$\mathrm{Hom} _{P(\Delta)}(\Delta^n, \underline{\mathrm{Hom}}(X, Z)) = \mathrm{Hom} _{P(\Delta)}(\Delta^n \times X, Z)$$
{% end %}

由于 Yoneda 引理给出 $\mathrm{Hom}_{P(\Delta)}(\Delta^n, X) \cong X_n$. 我们令 $(\underline{\mathrm{Hom}}(X, Z))_n = \mathrm{Hom} _{P(\Delta)}(\Delta^n \times X, Z)$ 然后诱导面映射和退化映射即可。

{% admonition(type="definition", title="充实范畴（enriched category）") %}
充实范畴是范畴的推广。一个幺半范畴 $K$ 上的充实范畴拥有某个对象集 $\mathrm{Ob}(\mathcal{C})$；对两个对象 $X, Y$，有一个态射对象“hom-object” $\mathrm{Map}(X, Y) \in K$. 复合被定义为态射 $\circ: \mathrm{Map}(Y, Z) \otimes \mathrm{Map}(X, Y) \to \mathrm{Map}(X, Z)$，满足结合律、单位律公理。
{% end %}

典型的例子是考虑简单类型论，让对象是类型，对象 $A, B$ 的态射是一个 $f: A \to B$，[如此构成的范畴](@/posts/haskell_2.md)是充实自身的。

{% admonition(type="definition", title="单纯范畴（simplicial category）") %}
单纯范畴是指 $P(\Delta)$ 上的充实范畴。
{% end %}

我们取对象集是 $\mathrm{Ob}(P(\Delta))$，然后让 $\mathrm{Map}(X, Y)$ 是引理得到的 $\underline{\mathrm{Hom}}(X, Y)$.

现在定义态射如何复合。只需定义 $\circ_n: (\underline{\mathrm{Hom}}(Y, Z))_n \times (\underline{\mathrm{Hom}}(X, Y))_n \to (\underline{\mathrm{Hom}}(X, Z))_n$. 由引理，只需对 $\alpha: \Delta^n \times Y \to Z$ 及 $\beta: \Delta^n \times X \to Y$ 找到 $\gamma: \Delta^n \times X \to Z$，这是通过：

$$\Delta^n \times X \xrightarrow{(\mathrm{diag}, \mathrm{id}_X)} \Delta^n \times \Delta^n \times X \xrightarrow{\mathrm{id} \times \beta} \Delta^n \times Y \xrightarrow{\alpha} Z$$

如此得到的单纯范畴是 $\mathbf{sSet}$，它是充实自身的，即：

$$\mathrm{Map}_{\mathbf{sSet}}(X, Y) \in \mathrm{Ob}(P(\Delta)) = \mathrm{Ob}(\mathbf{sSet})$$ 

{% admonition(type="note", title="歧义") %}
$\mathbf{sSet}$ 有时也指代上文 $P(\Delta)$，此时采取的观点是充实 $\mathbf{Set}$.
{% end %}

### 几何实现
回忆拓扑 $n$-单形是 $\Delta^n_{\text{top}} = \set{(t_0, \dots, t_n) | \sum t_i = 1, t_i \geq 0}$，而一个拓扑空间 $X$ 上的奇异单纯集 $S(X) \in P(\Delta)$ 就是由[奇异单形](@/posts/homology_1.md) $S(X)_n = \mathrm{Hom} _{\mathbf{Top}}(\Delta^n _{\text{top}}, X)$ 给出的。

{% admonition(type="definition", title="几何实现") %}
几何实现 $|\cdot| \in \mathrm{Fun}(P(\Delta), \mathbf{Top})$ 定义为唯一的保余极限的将 $\Delta^n$ 打到 $\Delta^n_{\mathrm{top}}$ 的函子。
{% end %}

实际上存在一个结论，对拓扑空间 $Y$，由下述引理的伴随给出的 $\varepsilon_Y: |S(Y)| \to Y$ 是一个弱同伦等价。$|S(Y)|$ 必然是 CW 复形，由 Whitehead 定理，当 $Y$ 也是 CW 复形时它是一个[同伦等价](@/posts/geometry_2_final.md)。

{% admonition(type="theorem", title="引理") %}
$S(\cdot)$ 是 $|\cdot|$ 的右伴随，即：

$$\mathrm{Hom} _{\mathbf{Top}}(|X|, Y) \cong \mathrm{Hom} _{P(\Delta)}(X, S(Y))$$
{% end %}

$$
\begin{align*}
	& \text{LHS} \cr
	= & \mathrm{Hom} _{\mathbf{Top}}(\operatorname*{colim} _{\Delta \downarrow X} |\Delta^n|, Y) \cr
	= & \operatorname*{lim} _{\Delta \downarrow X} \mathrm{Hom} _{\mathbf{Top}}(|\Delta^n|, Y) \cr
	\cong & \operatorname*{lim} _{\Delta \downarrow X} \mathrm{Hom} _{P(\Delta)}(\Delta^n, S(Y)) \cr
	= & \mathrm{Hom} _{P(\Delta)}(\operatorname*{colim} _{\Delta \downarrow X} \Delta^n, S(Y)) \cr
	\cong & \text{RHS}
\end{align*}
$$

## 无穷范畴
### Kan 复形
{% admonition(type="definition", title="脉（nerve）") %}
一个范畴 $\mathcal C$ 的脉 $N(\mathcal C)$ 是一个单纯集 $\mathcal \Delta^{\text{op}} \to \mathbf{Set}$，将 $[n]$ 打到 $\mathrm{Fun}([n], \mathcal C)$.
{% end %}

考虑例子 $\mathcal{C}$ 是 $X \xrightarrow{f} Y$，则有如下不严格的诠释：

| 维数 | 直观诠释 |
| :-: | :-: |
| $N(\mathcal C)_0$ | 其对象 $\mathrm{Ob}(\mathcal{C})$ |
| $N(\mathcal C)_1$ | 其态射 $\mathrm{Mor}(\mathcal{C})$ |
| $N(\mathcal C)_2$ | 态射复合的定义 |
| $N(\mathcal C)_3$ | 复合的结合律/复合间的相容性 |
| $N(\mathcal C)_{\geq 4}$ | 更复杂的律，尽管确实是结合律的衍生 |

一个显然的例子是：

$$\Delta^n \cong N([n])$$

由此引出的问题是，什么样的单纯集恰好是某个范畴的脉？

{% admonition(type="definition", title="Serre fibration") %}
$f \in \mathrm{Hom}_{\mathbf{Top}}(X, Y)$ 是一个 Serre fibration，如果对所有形如下图的交换图表存在 $h: D^n \times I \to X$ 使图表交换：

$$
\begin{CD}
	D^n @>>> X \cr
	@VV(\mathrm{id}, 0)V @VVfV \cr
	D^n \times I @>>> Y
\end{CD}
$$
{% end %}

Serre fibration 的单纯对应物是 Kan fibration，即将 $D^n \to D^n \times I$ 换成嵌入：

$$\Lambda^n_j \hookrightarrow \Delta^n$$

这里 $\Lambda^n_j\\, (0 \leq j \leq n)$ 是角（horn）。$\Lambda^n_j$ 是去掉第 $j$ 个面得到的，即：

$$\Lambda^n_j = \bigcup_{i \neq j} \delta_i(\Delta^{n-1})$$

当 $0 < j < n$ 时称它是内角（inner horn），否则称为外角（outer horn）。

{% admonition(type="definition", title="Kan 复形") %}
$X \in P(\Delta)$ 是 Kan 复形，如果 $X \to \set{\ast}$ 是 Kan 纤维化。即，对 $\Lambda^n_j \to X$ 及 $\Lambda^n_j \hookrightarrow \Delta^n$，存在 $\Delta^n \to X$ 使图表交换。
{% end %}

作为例子，考虑 $n = 2$。取 $j = 1$，设 $0 \to 1$ 与 $1 \to 2$ 对应 $f, g$，则 $0 \to 2$ 被对应到 $g \circ f$，可见内角被填充成单形给出的是复合被更高的相容所验证。现在取 $j = 0$，设 $0 \to 1$ 与 $0 \to 2$ 对应 $f, h$，则不严格地说 $1 \to 2$ 被对应到的应当是 $h \circ f^{-1}$，故我们认为外角被填充成单形给出的是态射的逆。

{% admonition(type="theorem", title="引理") %}
对 $X \in \mathbf{Top}$，$S(X)$ 是 Kan 复形。
{% end %}

只需证明对 $|\Lambda^n_j| \to X$ 及 $|\Lambda^n_j| \hookrightarrow |\Delta^n|$，存在 $|\Delta^n| \to X$ 使图表交换。由于 $|\Lambda^n_j|$ 是 $|\Delta^n|$ 的强形变收缩核，易见可以取出一个 $|\Delta^n| \to |\Lambda^n_j|$，复合后即是所求。

{% admonition(type="theorem", title="定理") %}
$N(\mathcal C)$ 是 Kan 复形当且仅当 $\mathcal C$ 是群胚（即所有的态射都是同构）。
{% end %}

证明略。

### 拟范畴
{% admonition(type="definition", title="spine") %}
spine 是指 $[n]$ 忘却复合的结果，即：

$$0 \to 1 \to \cdots \to n$$

或记作：

$$I^n = \Delta^{\set{0, 1}} \cup \cdots \cup \Delta^{\set{n-1, n}}$$
{% end %}

把 Kan 复形定义中的角换成 spine，得到的东西称为 composer.

{% admonition(type="definition", title="同伦") %}
对 $X \in \mathbf{sSet}$，称 $f, g \in X_1$ 同伦 $f \simeq g$，如果存在 $\sigma: \Delta^2 \to X$ 使得：

$$\sigma| _{\Delta^{\set{0, 1}}} = f, \sigma| _{\Delta^{\set{0, 2}}} = g, \sigma| _{\Delta^{\set{1, 2}}} = \mathrm{id} _y$$

其中令 $x = d_1 f, y = d_0 f$，将 $f, g$ 看成 $x$ 到 $y$ 的箭头。
{% end %}

{% admonition(type="theorem", title="等价关系的条件") %}
上述同伦是等价关系，如果 $X$ 对 $3$ 维角的嵌入映射有提升关系（has the lifting property with 3-horn inclusion）。
{% end %}

证明如图：

![等价关系](/images/misc/2026_08_09.jpg)

{% admonition(type="definition", title="同伦范畴") %}
对 $X \in \mathbf{sSet}$，其同伦范畴 $h(X)$ 是以 $X_0$ 为对象集，以 $X_1$ 中的同伦类为态射集的范畴。这需要 $X$ 中同伦是等价关系，且复合在同伦类意义下存在且唯一。
{% end %}

在定义合法时，有 $h(N(\mathcal C))$ 到 $\mathcal C$ 的典范同构。

我们不妨推广 extension property for 3-horn inclusion 得到：

{% admonition(type="definition", title="拟范畴（quasi category）") %}
一个单纯集 $X$ 称为拟范畴，如果对所有内角都有扩张性质（has the extension property for all inner horn）。
{% end %}

我们把拟范畴及其等价的模型称为 $\infty$-范畴。

{% admonition(type="note", title="(n, r)-范畴") %}
为了和更古老的含义表示区分，这里的 $\infty$-范畴有时写作 $(\infty, 1)$-范畴。一个 $(n, r)$-范畴是指满足 $k > n$ 的 $k$-态射平凡、$k > r$ 的 $k$-态射可逆的高阶范畴。

“高阶范畴”引入了态射之间的态射、态射之间的态射之间的态射等 $k$-态射，此词有“严格高阶范畴”与“弱高阶范畴”两个指代。前者的定义是递归地通过：一个严格 $(n+1, r+1)$-范畴是一个充实于严格 $(n, r)$-范畴的范畴得到的，这要求复合是严格唯一的。而现实中出现的高阶结构大多不是严格的，因此考虑弱高阶范畴。实际上弱 $(\infty, n)$-范畴的一种定义方式就是使用拟范畴的推广。
{% end %}
