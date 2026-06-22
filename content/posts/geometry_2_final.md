+++
title = "几何学Ⅱ期末复习笔记"
date = 2026-06-22

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

下半学期的内容是基本群和复叠空间。参考尤承业《基础拓扑学讲义》及 Allen Hatcher 的 *Algebraic Topology* 第一章。

<!-- more -->

<style>
	img:where(.dark,.dark *) {
        filter: invert(100%);
    }
</style>

## 引入
### 闭曲面分类
这里所说曲面是指二维（拓扑）流形，闭曲面是指没有边界点的紧致连通曲面。

{% admonition(type="note", title="关于可数") %}
在《基础拓扑学讲义》中流形的定义没有第二可数的要求。这会带来一些病态的例子，如 Alexandroff 长直线：对 $\omega_1 \times [0, 1]$ 每一区间首尾相接。在听闻闭曲面分类定理时我疑心：如果有不可数个洞怎么办？但这里对闭曲面要求了紧致（这可以推出第二可数），是足够的。
{% end %}

我们定义对闭曲面的“环柄”手术是在该闭曲面上挖一个洞（挖去一个开圆盘），在环面上挖一个洞，然后粘起来。把球面上安 $n$ 个环柄得到的称为 $nT^2$ 亏格为 $n$ 的可定向闭曲面。

“交叉帽”是在洞口粘一条 Möbius 带，等效地说是把洞口的对径点粘合。把球面上安 $m$ 个交叉帽得到的称为 $mP^2$ 亏格为 $n$ 的不可定向闭曲面。有 $1P^2$ 就是 $P^2$，$2P^2$ 就是 Klein 瓶。

{% admonition(type="theorem", title="闭曲面分类定理") %}
球面、$\set{nT^2 | n \in \Z^+}$ 及 $\set{nP^2 | n \in \Z^+}$ 不重复地列出了闭曲面的所有拓扑类型。
{% end %}

我们引入闭曲面的多边形表示：在一个偶数边的多边形上标字母和箭头，表示把对应的边粘在一起。我们选取一个顶点和顺/逆时针，可以简写为形如 $aba^{-1}b$ 的表达式（此例为 Klein 瓶）。

通过三角剖分（存在冗长的初等证明），可以证明任一闭曲面都有多边形表示。对于多边形手术我们可以做两类手术：粘合相邻的 $aa^{-1}$；选定一个字母 $a$，沿着某条对角线剪开，然后沿 $a$ 粘在一起。我们可以得到标准的多边形表示：

$$a_1b_1a_1^{-1}b_1^{-1} a_2b_2a_2^{-1}b_2^{-1} \cdots a_nb_na_n^{-1}b_n^{-1} \tag{T}$$

$$a_1a_1a_2a_2 \cdots a_mb_m \tag{P}$$

这样一来我们证明了除了不重复之外的事情。

---

我们定义 Euler 示性数 $\chi = V - E + F$. 则 $nT^2$ 型的示性数 $2 - 2n$，$mP^2$ 型的示性数 $2 - m$.

{% admonition(type="question", title="2025 P2") %}
将 Klein 瓶表面挖去两个不相交的开圆盘，得到带边的紧曲面 $K$. 将 $K$ 拷贝两份，并将边界用恒同映射粘合，得到的曲面记为 $X$. 确定 $X$ 在闭曲面分类表中对应哪一种同胚型，并说明理由。
{% end %}

可以看成 $T^2$ 粘上两个 $2P^2$. 由于不可定向，用示性数算得 $6P^2$.

### 基本概念
{% admonition(type="definition", title="同伦") %}
对 $f, g \in C(X, Y)$，称它们**同伦** $f \simeq g$，如果存在连续映射 $H: X \times I \to Y$ 使得 $H(x, 0) = f(x), H(x, 1) = g(x)$. 此时称 $H$ 是 $f$ 与 $g$ 之间的同伦。
{% end %}

如果 $f$ 同伦于常值映射，称 $f$ 是零伦的。

{% admonition(type="note", title="连续") %}
这里说 $H$ 连续映射是指从积空间到 $Y$ 的按定义连续，而不是对两个参数分别连续。
{% end %}

{% admonition(type="note", title="2-同伦") %}
我们还可以讨论 $H, K: f \simeq g$ 之间的同伦 $F: X \times I \times I \to Y$，称为“$2$-同伦”。注意这和“$2$ 阶同伦”是不同的东西。
{% end %}

对 $A \subseteq X$，如果 $H$ 还满足 $H(a, t) = f(t) = g(t)$，则称 $f$ 和 $g$ 相对于 $A$ 同伦 $f \simeq g \text{ rel } A$.

设 $a, b$ 是 $X$ 上的两道路，如果 $a \simeq b \text{ rel } \set{0, 1}$，则称它们**定端同伦**。在此等价关系下 $X$ 所有道路的等价类称为 $X$ 的道路类。当起点、终点重合时称为基点。

称 $f: X \to Y$ 是一个**同伦等价**，如果存在 $g: Y \to X$ 使得 $g \circ f \simeq \mathrm{id}_X, f \circ g \simeq \mathrm{id}_Y$. 此时称 $X$ 和 $Y$ 有相同的伦型 $X \simeq Y$. 这个关系比同胚弱（我们用 $X \cong Y$ 表示同胚）。

对 $A \subseteq X$ 及含入映射 $i: A \hookrightarrow X$，若存在 $r: X \to A$ 是收缩映射（$r \circ i = \mathrm{id}_A$），使得 $i \circ r \simeq \mathrm{id}_X$，则称 $A$ 是 $X$ 的**形变收缩核**。对应的 $H: \mathrm{id}_X \simeq i \circ r$ 称为一个形变收缩。如果 $H$ 还满足保持 $A$ 中的点不动，则称之为**强形变收缩**。注意这是同伦等价的特例。

与单点空间同伦等价的称为**可缩空间**，此时其任一点都是形变收缩核。

{% admonition(type="note", title="有用的定理") %}
当 $(X, A)$ 是一个 CW 对，且 $A$ 可缩时，$X \to X/A$ 是一个同伦等价。
{% end %}

## 基本群
### 基本群
{% admonition(type="definition", title="基本群") %}
给定空间 $X$ 与点 $x$，以基点 $x$ 的道路的同伦类为元素，道路的拼接为乘法，易见构成群，记作 $\pi_1(X, x)$.
{% end %}

若 $X$ 道路连通，易见基本群（的同构型）与 $x$ 无关，记作 $\pi_1(X)$. 道路连通且基本群平凡的空间称为**单连通**的。

易见同伦等价的空间的基本群是同构的。

{% admonition(type="note", title="高阶同伦群") %}
$n$ 阶同伦群 $\pi_n(X, x)$ 的元素是从 $S^n$ 到 $X$ 带基点 $x$ 的连续映射的同伦类。若把 $S^n$ 视作 $I^n/\partial I^n$，则乘积定义为在第一个坐标上拼接。在 $n \geq 2$ 时一定是交换群。
{% end %}

{% admonition(type="theorem", title="圈的基本群") %}
$S^1$ 的基本群是 $\Z$.
{% end %}

使用复叠空间，这里略过。

读者容易证明，关于积空间有：

$$\pi_1(X \times Y, (x, y)) \cong \pi_1(X, x) \times \pi_1(Y, y)$$

从而 $\pi_1(T^2) = \Z \times \Z$.

{% admonition(type="theorem", title="Van-Kampen 定理") %}
如果拓扑空间 $X$ 可分解为开集 $U', U''$ 的并，$W = U' \cap U''$ 非空、道路连通，设：

$$
\begin{CD}
    W @>i'>> U' \cr
    @Vi''VV @VVj'V \cr
    U'' @>j''>> X
\end{CD}
$$

用 $[S]$ 表示正规闭包，对 $w \in W$ 有 $\pi_1(X, w) \cong \pi$，其中：

$$\pi = \pi_1(U', w) \ast \pi_1(U'', w) / [\set{i' _\pi(\alpha) i'' _\pi(\alpha^{-1}) | \alpha \in \pi_1(X, w)}]$$
{% end %}

不妨设 $U', U''$ 同在 $W$ 所在道路连通分支中。

同态 $(j') _\pi$ 与 $(j'') _\pi$ 唯一决定了一个 $\varphi: \pi _1(U', w) \ast \pi _1(U'', w) \to \pi _1(X, w)$. 它满足：

$$\varphi(i' _\pi(\alpha) i'' _\pi(\alpha^{-1})) = (j') _\pi i' _\pi(\alpha) \cdot (j'') _\pi i'' _\pi(\alpha) = 1$$

由 $\varphi$ 可以诱导出一个同态 $\Phi: \pi \to \pi_1(X, w)$，只需证它是同构。

先证 $\Phi$ 是满的，只需 $\varphi$ 是满的。对 $\gamma \in \pi_1(X, w)$，由于 $[0, 1]$ 是紧的，可取到足够大的 $n$ 将 $[0, 1]$ 进行 $n$ 等分，使得每个区间对应道路全在 $U'$ 中或全在 $U''$ 中，指定分割点到 $w$ 的道路将每一段改造为以 $w$ 为基点的，就找到了原像。

再证 $\Phi$ 是单的。对 $U'$ 或 $U''$ 中基点 $w$ 的道路 $\gamma$ 可以唯一指定 $\pi$ 中的一个元素 $[\gamma]$. 这一指定满足 $[\gamma \psi] = [\gamma] [\psi]$ 及在 $U'$ 或 $U''$ 中 $\gamma, \psi$ 定端同伦则 $[\gamma] = [\psi]$. 我们考察 $\varphi(\omega) = 1, \omega = [\gamma_1] \cdots [\gamma_n]$.

取 $\gamma$ 使得 $\gamma|_{[(i-1)/n, i/n]} = \gamma_i$，有 $\gamma$ 到 $\mathrm{id}_w$ 的同伦 $H: I \times I \to X$. 取 $m = kn$ 将 $I \times I$ 等分，使得每个小方块全在 $U'$ 中或全在 $U''$ 中，可分析知 $\omega = 1$.

{% admonition(type="note", title="改进") %}
可以作改进：$U', U''$ 为闭集，$W$ 是它一个开邻域的强形变收缩核。
{% end %}

使用此定理可以得到，对于 $nT^2$ 型曲面，$\pi_1(X)$ 的交换化是 $\Z^{2n}$，对于 $mP^2$ 型曲面，交换化是 $\Z^{m-1} \times \Z/2\Z$，故各不相同。基本群本身则恰好是所有的生成元商去多边形表示看成生成元关系的结果。

{% admonition(type="note", title="Wirtinger presentation") %}
这是一个关于对扭结 $K$ 的 $\pi_1(\mathbb{E}^3 \setminus K)$ 的一般算法，见 Hatcher 习题 1.2.22. 可以取基点在桌面下方，生成元是绕过 $\alpha_i$ 的圈（取定定向）来想象。
{% end %}

### 典型应用
基本群（及高阶同伦群）的典型应用如下：

{% admonition(type="theorem", title="Brouwer 不动点定理") %}
$D^n \to D^n$ 连续映射一定存在不动点。
{% end %}

假设没有不动点，则考虑 $g$ 如下：

$$g(x) = \frac{x - f(x)}{|x - f(x)|}$$

有 $\mathrm{id} _{S^{n-1}} \simeq g| _{S^{n-1}} = g \circ i$，而 $i: S^{n-1} \to D^n$ 是零伦的，矛盾。

{% admonition(type="theorem", title="Jordan 曲线定理") %}
对 $\mathbb{E}^2$ 上一条 Jordan 曲线 $J$，有 $\mathbb{E}^2 \setminus J$ 有两个连通分支，且都以 $J$ 为边界。
{% end %}

证明琐碎且用到 Tietze 扩张定理。由于存在初等证明，此处略去。

{% admonition(type="theorem", title="区域不变性定理") %}
对 $U \subseteq \R^n$ 开，$f: U \to \R^n$ 连续且单，有 $f$ 是 $U \to f(U)$ 的同胚。
{% end %}

使用此可以得到“边界不变性” $\R^n \ncong \R^{n-1} \times [0, +\infty)$ 与“维数不变性” $\R^n \ncong \R^m$.

只需要证 $f(U)$ 开，进一步只需要证 $f(B_0(1))$ 包含 $f(0)$ 的一个开邻域。假设存在反例，我们希望构造 $g: \R^n \to \R^n$ 满足：

$$
\begin{cases}
    |x - g(f(x))| \leq 1 \cr
    g(f(x)) \neq 0
\end{cases}
$$

这会使得 $x \mapsto x - g(f(x))$ 为 $B^n \to B^n$ 连续函数，且无不动点。

关于 $g$ 的构造略去。

## 复叠空间
### 复叠空间
{% admonition(type="definition", title="复叠空间") %}
对道路连通、局部道路连通的空间 $X, \tilde{X}$，称 $p: \tilde{X} \to X$ 为 $X$ 上的**复（覆）叠（迭）空间**，如果对任一 $x \in X$ 存在开邻域 $U$，满足 $p^{-1}(U)$ 是一族不交开集 $\set{V_\alpha}$ 的并，且 $p$ 把每个 $V_\alpha$ 同胚地映成 $U$. 此时称 $X$ 为底空间，满足上述性质的 $U$ 为基本邻域，$p^{-1}(x)$ 为 $x$ 上的纤维，其基数称为**叶/页/层数**。
{% end %}

由于叶数局部常值且 $X$ 连通，叶数与 $x$ 的选取无关。

典型的例子是 $S^1$ 的一个复叠空间是无限长弹簧，对应 $p: \mathbb{E}^1 \to S^1, x \mapsto e^{i\pi x}$. 基于此一个稍微不平凡的事实是：

$$
\begin{aligned}
p: \mathbb{E}^2 & \to \mathbb{E}^2 \setminus \mathbf{0} \cr
    (x, y) & \mapsto (e^x \cos y, e^x \sin y)
\end{aligned}
$$

对于 $f: X \to X$ 同胚满足 $f^n = \mathrm{id}$ 且 $f^m (0 < m < n)$ 没有不动点，当 $X$ Hausdorff 时，容易证明 $p: X \to X/f$ 是叶数为 $n$ 的复叠。

对复叠 $p: \tilde{X} \to X$，称 $\tilde{f}: Y \to \tilde{X}$ 是 $f: Y \to X$ 的**提升**，如果 $p \circ \tilde{f} = f$.

{% admonition(type="theorem", title="提升唯一性定理") %}
设 $Y$ 连通，$\tilde{f_1}, \tilde{f_2}$ 都是 $f$ 的提升，且在某一点 $x_0$ 处 $\tilde{f_1}(x_0) = \tilde{f_2}(x_0)$，则 $\tilde{f_1} = \tilde{f_2}$.
{% end %}

令 $A = \set{x \in X | \tilde{f_1}(x) = \tilde{f_2}(x)}$，只需证 $A$ 既开又闭。用定义取出 $A$ 及 $A^\complement$ 任一点的邻域即可。

其推论是，对 $X$ 中道路 $\gamma$，设 $\gamma(0) = x$ 及 $\tilde{x} \in p^{-1}(x)$，存在唯一提升 $\tilde{\gamma}$ 使 $\tilde{\gamma}(0) = \tilde{x}$. 这里存在性是用 $[0, 1]$ 的紧性：对 $\gamma(I)$ 中的点的基本邻域，所有 $\gamma^{-1}(U)$ 是一个覆盖，取有限子覆盖，在 $\tilde{X}$ 中使用粘接引理即可。

{% admonition(type="note", title="紧性的使用") %}
看完的感受是紧性生来就是这么用的。如果从 $0$ 处开始每次在邻域中取点作新的邻域，则很难说明为什么会在有限步终止。
{% end %}

容易看到对 $\tilde{x} \in p^{-1}(x)$，有一个单同态 $p_\pi: \pi_1(\tilde{X}, \tilde{x}) \to \pi_1(X, x)$. 有 $[\pi_1(X, x) : p_\pi(\pi_1(\tilde{X}, \tilde{x}))]$ 等于 $p$ 的叶数。

{% admonition(type="theorem", title="同伦提升定理") %}
$$
\begin{CD}
    Y @>\tilde{f}>> \tilde{X} \cr
    @V\mathrm{inj}_0VV @VVpV \cr
    Y \times I @>F>> X
\end{CD}
$$

存在 $\tilde{F}: Y \times I \to \tilde{X}$ 使图表交换。
{% end %}

构造方式略。只需证其连续。若 $F(\set{y} \times [s, t])$ 在某个基本邻域中，且 $\tilde{F}$ 的 $s$-切片在 $y$ 连续，则存在 $y$ 的邻域 $W$ 使得 $\tilde{F}$ 在 $W \times [s, t]$ 连续。

{% admonition(type="theorem", title="映射提升定理") %}
对道路连通、局部道路连通的 $Y$ 及 $f: Y \to X$, $f(y) = p(\tilde{x}) = x$，则存在 $f$ 的提升使 $\tilde{f}(y) = \tilde{x}$ 当且仅当 $f_\pi(\pi_1(Y, y)) \subseteq p_\pi(\pi_1(\tilde{X}, \tilde{x}))$.
{% end %}

仿照之前证明。

### 复叠变换
{% admonition(type="definition", title="复叠变换") %}
复叠变换是满足 $p \circ h = p$ 的 $\tilde{X}$ 自同胚，也即 $(\tilde{X}, p)$ 的自同构，记作 $\mathrm{Deck}(\tilde{X}/X)$.
{% end %}

如果 $p^{-1}(x)$ 的每一点都可被自同胚映作另外每一点（即群作用可递），则称 $\tilde{X}$ 是 $X$ 的**正则复叠**。

使用映射提升引理，知这等价于对每个 $\tilde{x}$，$p_\pi(\pi_1(\tilde{X}, \tilde{x}))$ 是 $\pi_1(X, p(\tilde{x}))$ 的正规子群。

记 $G = \pi_1(X, x)$ 及 $H = p_\pi(\pi_1(\tilde{X}, \tilde{x}))$，用 $N$ 表示正规化子，有：

$$\mathrm{Deck}(\tilde{X}/X) \cong N_G(H) / H$$

{% admonition(type="definition", title="万有复叠") %}
如果复叠空间 $\tilde{X}$ 是单连通的，就称为**万有/泛复叠**。
{% end %}

容易证明万有复叠在 $X$ 上同构意义下唯一。

{% admonition(type="theorem", title="复叠空间存在定理") %}
如果 $X$ 道路连通、局部道路连通、半局部单连通，则它有万有复叠空间。
{% end %}

取一点 $x$，让 $\tilde{X}$ 是以 $x$ 为起点的道路类集合，$p$ 把道路类映到其终点，有 $p$ 是满的。

记集合 $(\alpha, U)$ 包含所有 $\alpha \omega$，其中 $\omega$ 是 $U$ 中起点为 $\alpha$ 终点的道路。所有 $\alpha \in \tilde{X}$ 与 $\alpha(1)$ 的道路连通开邻域 $U$ 对应的 $(\alpha, U)$ 构成拓扑基 $\mathscr{B}$，规定 $\tilde{X}$ 上的拓扑是由 $\mathscr{B}$ 生成的。现在有 $p$ 连续且开。

对 $X$ 中的点，其半单连通的开邻域是一个基本邻域。

---

$S^1 \vee S^1$ 的万有复叠如图：

![万有复叠](/images/geometry/covering_space_1.jpg)

$z \mapsto z^3$ 的映射柱（这带有 $2$-胞腔）的万有复叠如图：

![万有复叠](/images/geometry/covering_space_cw_2.jpg)

{% admonition(type="question", title="2024 P6") %}
设 $\pi_1(S^1 \vee S^1) = \braket{a, b}$. 求复叠空间，使其基本群表示为 $\braket{a^2, b^2, (ab)^4}$ 在 $\braket{a, b}$ 的正规闭包。
{% end %}

![复叠](/images/geometry/covering_space_3.jpg)

一般的方法是，让 $Q = \braket{a, b} / [\braket{a^2, b^2, (ab)^4}]$ 中的每一个元素是一个顶点，连边 $q \to qa$ 与 $q \to qb$. 这相当于直接在万有复叠上把对应的值粘起来。

---

一般地在万有复叠构造上将特定的道路粘起来，知保持基点的复叠等价类与 $\pi_1(X, x)$ 的子群一一对应。

{% admonition(type="note", title="自由群的子群是自由群") %}
设自由群 $F$ 是图 $X$ 的基本群，取它的复叠对应 $F$ 的子群，这个复叠仍然是图。可以通过生成树证明图的基本群是自由群。
{% end %}

{% admonition(type="question", title="2024 P4") %}
求 $\R\mathrm{P}^2 \vee \R\mathrm{P}^2$ 所有连通的复叠空间。
{% end %}

只需考虑 $\Z/2\Z \ast \Z/2\Z$ 的子群。我们把它看成无限二面体群（$ab$ 是旋转，$b$ 是对称），知非平凡的子群有无限循环子群 $\braket{(ab)^n}$，二阶循环子群 $\braket{(ab)^m a}$ 及无限二面体子群 $\braket{(ab)^n, (ab)^m a}$.

{% admonition(type="question", title="2025 P6") %}
令 $X = \Complex \setminus \Z$，即复平面上挖掉所有实轴上的整点得到的空间。证明：$X$ 存在万有复叠，且其万有复叠空间同胚于平面。
{% end %}

由于得到的空间是二维流形（其中第二可数可能需要用定理的构造），且单连通、不紧，由黎曼单值化定理只能（同胚于）平面。

或者有构造：

![万有复叠](/images/geometry/covering_space_2.jpg)

{% admonition(type="definition", title="自由不连续") %}
群作用 $G \to \mathrm{Homeo}(X)$ 称为**自由不连续**的，如果对每个点 $x$ 存在开邻域 $U$，使得对所有 $g \in G$，有 $gU \cap U \neq \emptyset \iff g = 1$.
{% end %}

如 $\mathbb{E}^2$ 上变换 $g(x, y) = (x+1, y)$ 与 $h(x, y) = (-x, y+1)$ 生成的群给出的作用。

{% admonition(type="definition", title="恰当不连续") %}
群作用 $G \to \mathrm{Homeo}(X)$ 称为**恰当不连续**的，如果对任意紧集 $K$，$gK \cap K \neq \emptyset$ 只对有限多个 $g$ 成立。
{% end %}

如平面上整格平移和关于原点中心对称生成的作用。

一个非恰当不连续的例子是对 $a > 0$ 取 $\tau_a(x, y) = (ax, a^{-1}y)$.

{% admonition(type="theorem", title="命题") %}
对 $X$ 局部紧 Hausdorff 及 $G$ 在 $X$ 上恰当不连续群作用，有 $X/G$ 局部紧 Hausdorff. 且如果 $G$ 无挠（$g \neq 1 \implies g^n \neq 1$），则该作用自由不连续，这使得 $X \to X/G$ 是正则复叠。
{% end %}

证明略。

---

考虑模群 $\mathrm{SL}(2, \Z)$ 的无挠子群：

$$\Gamma(2) = \ker (\mathrm{SL}(2, \Z) \to \mathrm{SL}(2, \Z/2))$$

有 $\mathrm{UHP} \to \mathrm{UHP}/\Gamma(2)$ 是正则复叠，商 $\mathrm{UHP}/\Gamma(2)$ 是一个“双曲曲面”。

## 补充
一些与上半学期的联系。

{% admonition(type="definition", title="几何结构") %}
设 $(X, G)$ 是某种“几何”，流形 $M$ 上一个局部 $(X, G)$-几何结构是指一族图卡 $\set{U _\alpha, \varphi _\alpha} _{\alpha \in A}$ 满足 $\Phi _{\alpha\beta}: \varphi _\beta(U _\alpha \cap U _\beta) \to \varphi _\alpha(U _\alpha \cap U _\beta)$ 由 $G$ 中变换实现。
{% end %}

闭曲面有平面/球面/双曲三种几何结构之一。在 $3$ 维的版本是 Thurston 几何化纲领。

{% admonition(type="theorem", title="命题") %}
度量完备、单连通、Gauss 曲率恒 $-1$ 的曲面 $M$ 等距同构于 $\mathbb{H}^2$.
{% end %}

用测地参数系证局部等距同构存在，再证等距同构存在。

{% admonition(type="question", title="2025 P7") %}
给定区域 $U$ 上的光滑函数 $E, F, G, L, M, N$，满足曲面论基本定理中的正定性条件和 Gauss-Codazzi 方程组给出的相容性条件。问：当 $U$ 是单连通区域时，是否存在参数曲面片 $\varphi: U \to \mathbb{E}^3$，使得其第一第二类基本量分别为 $E, F, G, L, M, N$？说明理由。
{% end %}

存在。通过曲面论基本定理及紧知可以延拓，只需证明延拓与道路无关。取 $H: \gamma_0 \simeq \gamma_1$，用 $I \times I$ 的紧性即可。
