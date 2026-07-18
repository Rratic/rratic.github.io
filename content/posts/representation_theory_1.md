+++
title = "群表示论"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

{% admonition(type="definition", title="群表示") %}
有限群 $G$ 在线性空间 $V$ 上的一个表示是一个 $G$ 到 $\mathrm{GL}(V)$ 的同态。
{% end %}

最简单的是群的一维表示，我们可以为每个群元素分配一个复数并保持乘法结构。给定群 $G$ 到置换群 $S_n$ 的同态，可以让 $V$ 的基是被置换的元 $e_i$，此时得到**置换表示**。我们也可以让 $V$ 的基形式地是群的元素，作用是左乘，此时得到**正则表示**。

我们称两个表示同构，如果存在一个线性空间的同构保持群作用结构。

{% admonition(type="definition", title="类函数") %}
类函数是指满足下式的 $f: G \to \Complex$，其全体记为 $\mathrm{Cf}(G)$：

$$f(s) = f(tst^{-1}),\quad \forall s, t \in G$$
{% end %}

一个重要的类函数是**特征标**。对一个群 $G$ 在有限维 $\Complex$-线性空间 $V$ 上的表示，记 $\rho(g)$ 是 $g$ 对应的线性映射，定义特征标为：

$$\chi_V(g) = \mathrm{tr}(\rho(g))$$

{% admonition(type="theorem", title="对偶空间的特征标") %}
$$\chi_{V^\ast}(g) = \overline{\chi_{V}(g)}$$
{% end %}

这里 $V$ 上的表示诱导的 $V^\ast$ 上的表示需要满足 $gf(gv) = f(v)$，此时在取定的基下（用方括号表示对应的矩阵）：

$$[f]^\perp [\pi(g)]^\perp [\rho(g)] [v] = [f]^\perp [v]$$

故 $[\pi(g)]^\perp [\rho(g)] = I$. 由于 $g$ 有限两个矩阵是幂零的，特征值是单位根，从而 $\mathrm{tr}([\pi(g)]) = \overline{\mathrm{tr}([\rho(g)])}$.

{% admonition(type="theorem", title="张量积的特征标") %}
对有限维的 $V, W$：

$$\chi_{V \otimes W}(g) = \chi_V(g) \chi_W(g)$$
{% end %}

这里 $V \otimes W$ 上的表示是由 $g(v \otimes w) = gv \otimes gw$ 诱导的。同前一证明，考虑取基将张量积写成矩阵的方法强行计算即可。

{% admonition(type="theorem", title="映射空间的特征标") %}
对有限维的 $V, W$：

$$\chi_{\mathrm{Hom}(V, W)}(g) = \overline{\chi_V(g)} \chi_W(g)$$
{% end %}

$\mathrm{Hom}(V, W)$ 上的表示是由 $gF = g^{-1} \circ F \circ g$ 给出的，即按如下图表：

$$
\begin{CD}
	V @>F>> W \cr
	@V\rho(g)VV @VV\pi(g)V \cr
	V @>>gF> W
\end{CD}
$$

考虑以如下方式诱导出的同构，通过图上追猎即知与定义一致：

$$
\begin{aligned}
	\Phi: V^\ast \otimes W &\to \mathrm{Hom}(V, W) \cr
	f \otimes w &\mapsto (v \mapsto f(v) \cdot w)
\end{aligned}
$$

{% admonition(type="definition", title="特征标的内积") %}
群 $G$ 的特征标 $\chi_V, \chi_W$ 的内积定义为：

$$\braket{\chi_V, \chi_W} = \frac 1 {|G|} \sum_{g \in G} \chi_V(g) \overline{\chi_W(g)}$$
{% end %}

{% admonition(type="theorem", title="第一正交关系") %}
对群 $G$ 的不可约表示 $X_1, X_2$ 有：

$$
\braket{\chi_{X_1}, \chi_{X_2}} = \begin{cases}
	1 & X_1 \cong X_2 \cr
	0 & \text{otherwise}
\end{cases}
$$
{% end %}

按定义有：

$$\braket{\chi_{X_1}, \chi_{X_2}} = \frac 1 {|G|} \sum_{g \in G} \chi_{\mathrm{Hom}(W, V)}(g)$$

然后我们考虑对 $V$ 上表示定义以下 $V \to V$ 线性映射：

$$P = \frac 1 {|G|} \sum_{g \in G} \rho(g)$$

展开知 $P^2 = P$ 是投影映射，且由 $\rho(h) \cdot P = P$ 知：

$$\operatorname{Im} P = \set{v \in V | \rho(g)v = v, \forall g}$$

考虑 $V = \operatorname{Im} P \oplus \ker P$ 有 $\mathrm{tr}(P) = \dim (\operatorname{Im} P)$，现在代入 $\mathrm{Hom}(W, V)$ 即可。

---

考虑将一个表示分解为不可约表示 $W = \bigoplus_{i=1}^n V_i^{n_i}$，则有 $\chi_W = \sum n_i \chi_{V_i}$，即 $n_i = \braket{\chi_{V_i}, \chi_W}$，这会使得 $W \cong W' \iff \chi_W = \chi_{W'}$.

{% admonition(type="theorem", title="不可约表示与群阶数关系") %}
用 $\mathrm{Irr}(G)$ 表示所有互不同构的复表示，则：

$$\sum_{V \in \mathrm{Irr}(G)} (\dim V)^2 = |G|$$
{% end %}

考虑正则表示 $V_{\mathrm{reg}}$，设 $V$ 不可约，则：

$$
\begin{align*}
    & \braket{\chi_V, \chi_{V_{\mathrm{reg}}}} \cr
    = & \frac 1 {|G|} \sum_{g \in G} \chi_V(g) \overline{\chi_{V_{\mathrm{reg}}}(g)} \cr
    = & \frac 1 {|G|} \chi_V(1) \overline{\chi_{V_{\mathrm{reg}}}(1)} \cr
    = & \chi_V(1) \cr
	= & \dim V
\end{align*}
$$

作为一个例子，考虑 $S^3$，有两个 $1$ 维表示分别是全部映到 $1$ 与映射到 $\mathrm{sgn}(g)$. 通过对正则表示分解 $V_{\mathrm{reg}} = \mathrm{span}\set{a + b + c} \oplus \mathrm{span}\set{a - b, b - c}$ 有一个二维表示将 $(1)$ 映到 $I$，将 $(1 \\; 2)$ 映到 $\begin{pmatrix} -1 & ~ \cr 1 & 1 \end{pmatrix}$，将 $(1 \\; 2 \\; 3)$ 映到 $\begin{pmatrix} ~ & 1 \cr -1 & -1 \end{pmatrix}$. 由定理知这些就是所有不同构的不可约复表示。

{% admonition(type="theorem", title="不可约表示与共轭等价类") %}
$$\\# \mathrm{Irr}(G) = \\# \set{[a] | a \sim gag^{-1}}$$
{% end %}

我们知道 $\\# \set{[a] | a \sim gag^{-1}} = \dim \mathrm{Cf}(g)$，且 $\set{\chi_V | V \in \mathrm{Irr}(G)}$ 两两正交，现在证明它构成一组基。

假设不是基，取类函数 $f$ 使得 $\braket{f, \overline{\chi_V}}$ 均为零，令：

$$P_f = \sum_{g \in G} f(g) \rho(g)$$

这会使得以下图表交换：

$$
\begin{CD}
	V @>P_f>> V \cr
	@V\rho(g)VV @VV\rho(g)V \cr
	V @>>P_f> V
\end{CD}
$$

而满足这样的全体函数是 $1$ 维的，故 $P_f = \lambda I$，又：

$$\mathrm{tr}(P_f) = \sum_{g \in G} f(g) \chi_V(g) = |G| \cdot \sum_{g \in G} \braket{f, \overline{\chi_V}} = 0$$

故 $P_f \equiv 0$，知 $f \equiv 0$，结论成立。

---

另一种证法是，让不可约表示是行标题 $V_i$，共轭类是列标题 $C_j$，建立表格，填入：

$$\sqrt{\frac{|C_j|}{|G|}} \chi_{V_i}(C_j)$$

有行构成标准正交基，从而列也构成。故填入 $\chi_{V_i}(C_j)$ 时正交。

考虑 $Q_8$，$4$ 个一维表示容易找到，然后此结论可用于填满以下例子的表格：

| $Q_8$ | $1$ | $-1$ | $\pm i$ | $\pm j$ | $\pm k$ |
| :-: | :-: | :-: | :-: | :-: | :-: |
| $V_1$ | $1$ | $1$ | $1$ | $1$ | $1$ |
| $V_2$ | $1$ | $1$ | $1$ | $-1$ | $-1$ |
| $V_3$ | $1$ | $1$ | $-1$ | $1$ | $-1$ |
| $V_4$ | $1$ | $1$ | $-1$ | $-1$ | $1$ |
| $V_5$ | $2$ | $?$ | $?$ | $?$ | $?$ |
