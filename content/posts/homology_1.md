+++
title = "同调论的拓扑基础与持续同调"
date = 2026-06-30

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

由于高阶同伦群不好算，我们引入了同调群。其一个应用是数据分析的持续同调。

<!-- more -->

## 同调群
### 胞腔同调
考虑胞腔复形 $X$ 为点 $x$ 到点 $y$ 的四条标记了方向的道路 $a, b, c, d$. 有 $ab^{-1}$ 与 $b^{-1}a$ 描述的是同一个圈，我们用加法表示运算是交换的，一个边链 $ka + lb + mc + nd$ 是圈的充要条件是 $k + l + m + n = 0$.

对此，我们作一般的描述：令 $C_0$ 是顶点生成的自由 Abel 群（即包含所有形式和），令 $C_1$ 是边生成的自由 Abel 群，令同态 $\partial_1: C_1 \to C_0$ 将边映射到 $y - x$，就有 $0$ 阶**胞腔同调群** $H_1(X) = \ker \partial_1 / \mathrm{Im} \partial_2 = \braket{a - b, b - c, c - d} \cong \Z \oplus \Z \oplus \Z$.

如果我们在 $X$ 基础上添加一个顶点为 $x, y$，边为 $a, b$ 的面 $A$，并令 $\partial_2: C_2 \to C_1$ 将 $A$ 映射到 $a - b$，则现在 $H_1(X) = \ker \partial_1 / \mathrm{Im} \partial_2 = \braket{b - c, c - d}$. 这反映的是面的加入使得 $ab^{-1}$ 不再是圈。

以类似的方式在胞腔复形上定义 $\partial_n: C_n \to C_{n-1}$ 及 $H_n(X) = \ker \partial_n / \mathrm{Im} \partial_{n+1}$.

现在的问题是在定义 $\partial$ 时相当麻烦。

### 单纯同调
为此我们考虑三角剖分。令 $n$-单形是指顶点 $v_0, \dots, v_n$ 的闭包，标准的单形形如：

$$\Delta^n = \set{(t_0, \dots, t_n) \in \R^{n+1} | \sum t_i = 1, t_i \geq 0}$$

类似胞腔复形地定义 $\Delta$-复形，并且令边缘同态是：

$$\partial_n([v_0, \dots, v_n]) = \sum_i (-1)^i [v_0, \dots, v_{i-1}, v_{i+1}, \dots, v_n]$$

$\Delta$-复形可以进一步割为单纯复形，其要求单形唯一被其边决定。

容易发现 $\partial_{n-1} \circ \partial_n = 0$. 由此方法定义的 $H_n^\Delta(X)$ 记作**单纯同调群**。

### 奇异同调
为了证明同胚、同伦等价的空间有相同的同调群，我们引入奇异同调。一个奇异 $n$-单形指的是一个连续映射 $\sigma: \Delta^n \to X$. 仿照前文定义**奇异同调群**。基本的结论略过，注意 $H_1(X)$ 实际上是 $\pi_1(X)$ 的交换化。

为了方便描述，我们定义约化同调群 $\tilde{H}_n(X)$，它只在 $0$ 处不同，把 $\partial_0$ 换成 $\varepsilon(\sum n_i\sigma_i) = \sum n_i$. 有 $H_0(X) \simeq \tilde{H}_0(X) \oplus \Z$.

对 $f: X \to Y$ 有交换的图表：

$$
\begin{CD}
	\cdots @>>> C_{n+1}(X) @>\partial>> C_n(X) @>\partial>> C_{n-1}(X) @>>> \cdots \cr
	@. @Vf_\sharp VV @Vf_\sharp VV @Vf_\sharp VV @. \cr
	\cdots @>>> C_{n+1}(Y) @>\partial>> C_n(Y) @>\partial>> C_{n-1}(Y) @>>> \cdots
\end{CD}
$$

我们称这些 $f_\sharp$ 是两个**链复形**的**链映射**。这会诱导对应同调群之间的同态。

{% admonition(type="theorem", title="定理") %}
若 $f, g: X \to Y$ 同伦，则它们诱导相同的 $f_\ast = g_\ast: H_n(X) \to H_n(Y)$.
{% end %}

对一个 $\Delta^n \times I$，令 $\Delta^n \times \set{0} = [v_0, \dots, v_n]$，$\Delta^n \times \set{1} = [w_0, \dots, w_n]$. 对 $F: f \simeq g$ 定义 prism operator $P: C_n(X) \to C_{n+1}(Y)$ 如下：

$$P(\sigma) = \sum_i (-1)^i F((\sigma \times \mathrm{id})([v_0, \dots, v_i, w_i, \dots w_n]))$$

算得 $\partial P = g_\sharp - f_\sharp - P\partial$，满足此条件称 $P$ 为**链同伦**。从而 $\alpha \in \ker \partial_n \implies g_\sharp(\alpha) - f_\sharp(\alpha) \in \mathrm{Im} \partial_{n+1}$，知结论成立。

### 正合列
我们说一列同态在以下 $B$ 处正合，如果 $\ker \beta = \mathrm{Im} \alpha$. 处处正合的称为**正合列**。

$$\begin{CD} A @>\alpha>> B @>\beta>> C \end{CD}$$

我们将证明对于空间 $X$ 与 $X$ 中某个开集的非空、闭的形变收缩核 $A$（注意 CW 对 $(X, A)$ 一定满足此条件）有以下正合列：

$$
\begin{CD}
	\cdots @>>> \tilde{H} _n(A)
	@>\mathrm{inj} _\ast>> \tilde{H} _n(X)
	@>\mathrm{quot} _\ast>> \tilde{H} _n(X/A)
	@>\partial>> \tilde{H} _{n-1}(A)
	@>>> \cdots @>>> 0
\end{CD}
$$

我们定义 $C_n(X, A) = C_n(X) / C_n(A)$. 依此同前定义**相对同调群** $H_n(X, A)$.

考虑如下交换图表，其中每一列是短正合列，每一行是链复形：

$$
\begin{CD}
	~ @. 0 @. 0 @. 0 @. ~ \cr
	@. @VVV @VVV @VVV @. \cr
	\cdots @>>> A_{n+1} @>\partial>> A_n @>\partial>> \boxed{A_{n-1}} @>>> \cdots \cr
	@. @ViVV @ViVV @ViVV @. \cr
	\cdots @>>> B_{n+1} @>\partial>> \boxed{B_n} @>\partial>> \boxed{B_{n-1}} @>>> \cdots \cr
	@. @VjVV @VjVV @VjVV @. \cr
	\cdots @>>> C_{n+1} @>\partial>> \boxed{C_n} @>\partial>> C_{n-1} @>>> \cdots \cr
	@. @VVV @VVV @VVV @. \cr
	~ @. 0 @. 0 @. 0 @. ~
\end{CD}
$$

我们需要定义 $\partial: H_n(C) \to H_{n-1}(A)$. 考虑图中被框出群的元素 $a, b, \partial b, c$，有 $\partial [c] = [a]$ 是良定义的。从而可代入证明所需结论。

{% admonition(type="theorem", title="Excision Theorem") %}
对 $X$ 的子空间 $\bar{Z} \subseteq A^{\circ}$，嵌入 $(X-Z, A-Z) \hookrightarrow (X, A)$ 诱导了同构 $H_n(X-Z, A-Z) \to H_n(X, A)$.
{% end %}

先证明引理：对 $\mathcal{U} = \set{U_j}$ 满足它们的内部构成 $X$ 的一个开覆盖，令 $C_n^\mathcal{U}(X)$ 是由像只在单个 $U_j$ 中的 $\sigma$ 生成的自由 Abel 群，则嵌入 $\iota: C_n^\mathcal{U}(X) \hookrightarrow C_n(X)$ 是链同伦等价。

首先定义单形 $[v_0, \dots, v_n]$ 的 Barycentric Subdivision. 我们令 barycenter $b$ 是重心，考虑单形 $[b, v_0, \dots, v_{i-1}, v_{i+1}, \dots, v_n]$. 然后归纳地对每个面作 Barycentric Subdivision 再分割……最终得到 $n!$ 个 $n$ 维流形，满足 $\mathrm{diam}[w_0, \dots, w_n] \leq n/(n+1) \mathrm{diam} [v_0, \dots, v_n]$. 之后会用到 $(n/(n+1))^r \to 0$.

接下来考虑欧氏空间中的凸集 $Y$，记线性映射 $\Delta^n \to Y$ 生成的自由 Abel 群为 $\mathrm{LC} _n(Y)$. 令 $\mathrm{LC} _{-1}(Y) = \Z$. 对 $b \in Y$ 诱导一个同态：

$$
\begin{aligned}
b: \mathrm{LC} _n(Y) & \to \mathrm{LC} _{n+1}(Y) \cr
	[w_0, \dots, w_n] & \mapsto [b, w_0, \dots, w_n]
\end{aligned}
$$

对于 $\lambda: \Delta^n \to Y$ 记 $b_\lambda$ 是像的重心，可归纳定义：

$$
\begin{aligned}
S: \mathrm{LC} _n(Y) & \to \mathrm{LC} _n(Y) \cr
	\lambda & \mapsto b _\lambda(S\partial\lambda)
\end{aligned}
$$

容易归纳验证 $\partial S = S\partial$，从而我们可以考虑以下图表：

$$
\begin{CD}
	\cdots @>>> \mathrm{LC} _1(Y) @>\partial>> \mathrm{LC} _0(Y) @>\partial>> \mathrm{LC} _{-1}(Y) @>>> 0 \cr
	@. @VSVV @VS=\mathrm{id}VV @VS=\mathrm{id}VV @. \cr
	\cdots @>>> \mathrm{LC} _1(Y) @>\partial>> \mathrm{LC} _0(Y) @>\partial>> \mathrm{LC} _{-1}(Y) @>>> 0
\end{CD}
$$

我们归纳定义 $T$ 使得 $\partial T + T\partial = \mathrm{id} - S$，即 $T$ 是 $\mathrm{id}$ 与 $S$ 的链同伦：

$$
\begin{aligned}
T: \mathrm{LC} _n(Y) & \to \mathrm{LC} _{n+1}(Y) \cr
	\lambda & \mapsto b _\lambda(\lambda - T\partial\lambda)
\end{aligned}
$$

容易用此诱导非线性情形的定义。

对 $\sigma: \Delta^n \to X$ 取 $m(\sigma)$ 使得 $S^{m(\sigma)}(\sigma)$ 在 $C_n^{\mathcal{U}}(X)$ 中，定义：

$$
\begin{aligned}
D: C _n(X) & \to C _{n+1}(X) \cr
	\sigma & \mapsto \sum _{i=0}^{m(\sigma)-1} TS^i \sigma
\end{aligned}
$$

我们将下式方括号部分记作 $\rho(\sigma) \in C_n^{\mathcal{U}}(X)$：

$$\partial D\sigma + D\partial\sigma = \sigma - [S^{m(\sigma)}\sigma + D_{m(\sigma)}(\partial\sigma) - D(\partial\sigma)]$$

从而 $\iota\rho$ 和 $\rho\iota$ 均与恒同映射链同伦。推论是 $\iota$ 诱导了 $H_n^{\mathcal{U}}(X)$ 与 $H_n(X)$ 的同构。

回到原定理，我们令 $\mathcal{U} = \set{A, X - Z}$，分析即可。

其推论是，对 $X$ 与 $X$ 中某个邻域的非空、闭的形变收缩核 $A$，商映射诱导了 $H_n(X, A) \to H_n(X/A, A/A)$ 的同构，而后者同构于 $H_n(X/A)$.

{% admonition(type="theorem", title="维数不变性") %}
若非空开集 $U \subseteq \R^m$ 与 $V \subseteq \R^n$ 同胚，则 $m = n$.
{% end %}

使用 excision 及之前长正合列知：

$$
H _k(U, U - \set{x}) \cong H _k(\R^m, \R^m - \set{x}) \cong \tilde{H} _{k-1}(\R^m - \set{x}) \cong
\tilde{H} _{k-1}(S^{m-1}) = \begin{cases} \Z & k=m \cr 0 & \text{else} \end{cases}
$$

一般地定义**局部同调群**是指 $H_n(X, X - \set{x})$. 由 excision 知它只和局部的拓扑有关。这可以用于考察局部的同胚情况。

### 单纯与奇异同调的等价
{% admonition(type="theorem", title="等价性") %}
对 $\Delta$-复形 $X$ 与子复形 $A$，$H_n^\Delta(X, A)$ 与 $H_n(X, A)$ 总是同构。
{% end %}

先归纳地考虑有限维，$A = \emptyset$ 情形。记 $X^k$ 是 $k$-骨架。

$$
\begin{CD}
	H_{n+1}^\Delta(X^k, X^{k-1}) @>>> H_n^\Delta(X^{k-1}) @>>> H_n^\Delta(X^k) @>>> H_n^\Delta(X^k, X^{k-1}) @>>> H_{n-1}^\Delta(X^{k-1}) \cr
	@VVV @VVV @VVV @VVV @VVV \cr
	H_{n+1}(X^k, X^{k-1}) @>>> H_n(X^{k-1}) @>>> H_n(X^k) @>>> H_n(X^k, X^{k-1}) @>>> H_{n-1}(X^{k-1})
\end{CD}
$$

上图在行上正合，第 $2, 5$ 列由归纳假设是同构。由于 $\Phi: \coprod_\alpha(\Delta_\alpha^k, \partial\Delta_\alpha^k) \to (X^k, X^{k-1})$ 诱导了 $\coprod_\alpha \Delta_\alpha^k / \coprod_\alpha \partial\Delta_\alpha^k$ 与 $X^k / X^{k-1}$ 的同构，我们知道 $1, 4$ 列是同构。

根据以上条件使用 $5$-引理（通过对具体的元素作 diagram chasing）知 $3$ 列是同构。

对无限维的 $X$，使用 $X$ 的紧集只能与可数个开单形（单形去掉面）有交，知 $H_n^\Delta(X) \to H_n(X)$ 满，又它一定是单的，得证。$A \neq \emptyset$ 的证法则同理。

## 持续同调
持续同调是拓扑数据分析（TDA）的一种技术。我们考虑一般的欧氏空间中的点集，我们有一些方法将数据降维到适合观察的低维空间，如主成分分析（PCA），但降维可能丢失数据。而持续同调（PH）则不需要降维就能刻画数据全貌。

### Betti 数
定义 Betti 数 $\beta_n = \operatorname{rank} H_n(X)$. 历史上同调群最开始是用 Betti 数和扭系数表示的。我们知道有限生成 Abel 群的结构定理：

$$H_n(X) \cong \Z^{\beta_n} \oplus \bigoplus_i \Z/d_i\Z, \quad d_1 \mid d_2 \mid \cdots \mid d_k$$

### 构建复形
对一个取定的 $\varepsilon$，我们用该点集构建 Vietoris–Rips 复形，定义为：

$$V_\varepsilon(P) = \set{\sigma \subseteq P | \lVert u-v \rVert \leq \varepsilon, \forall u, v \in \sigma}$$

另一种选择是 Čech 复形：

$$C_\varepsilon(P) = \set{\sigma \subseteq P | \bigcap_{x \in \sigma} B(x, \varepsilon) \neq \emptyset}$$

### 算法
对于这样建立的单纯复形，我们可以计算它的 Betti 数。考虑 $\varepsilon$ 在 $[0, +\infty)$ 连续改变，观察那些拓扑特性的持续时间。持续时间长的更可能是信号而不是噪声。对每个持续段 $[l, r]$，有两种表现方式：在平面上绘制点 $(l, r)$，得到的持续图，和绘制区间 $[l, r]$ 得到的条形码。

在计算 Betti 数时，只需用矩阵化简的方法即可。
