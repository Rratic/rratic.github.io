+++
title = "一点点几何学随机学习"
date = 2026-08-17

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

在暑假初的时候阅读丘赛几何与拓扑的考纲，发现自己什么都不会，打算好好学习重新做人（然而并没有）。当时参考了知乎上一篇[学习建议](https://zhuanlan.zhihu.com/p/40333692)。本文为一些碎片学习的整合。

<!-- more -->

## 层
首先，对拓扑空间 $X$，其上的一个 $\mathcal C$-值**预层** $\mathcal F$ 是一个从 $X$ 的开集格（按包含偏序）到 $\mathcal C$ 的反变函子。考虑集合值预层，一个开集 $U$ 对应的对象 $\mathcal F(U)$ 称为 $U$ 的截面；$U \subseteq V$ 对应的态射 $\mathrm{res}_{U, V}$ 称为 $U$ 到 $V$ 的限制映射。

在此之上，一个预层 $\mathcal F$ 称为**层**（sheaf），如果对 $X$ 的任意开集 $U$ 及 $U$ 的开覆盖 $\set{U_i}$，满足局部确定性：

$$(\forall i: \mathrm{res}_ {U, U _i}(s) = \mathrm{res} _{U, U _i}(t)) \implies s = t$$

及粘合性：如果对一族截面 $s_i \in \mathcal F(U_i)$ 它们在重叠部分一致（即下式），则存在截面 $s \in \mathcal F(U)$ 使得对每个 $i$ 都有 $\mathrm{res}_{U, U_i}(s) = s_i$.

$$\mathrm{res} _{U _i, U _i \cap U _j}(s _i) = \mathrm{res} _{U _j, U _i \cap U _j}(s _j)$$

由局部确定性这还是唯一的。实际上，这两个条件也可以视作下图是等化子：

$$\mathcal F(U) \to \prod_i \mathcal F(U_i) \rightrightarrows \prod_{i, j} \mathcal F(U_i \cap U_j)$$

典型的例子是连续函数层：

$$\mathcal C^0(U) = \set{f: U \to \R | f \text{ continuous}}$$

类似地有光滑函数层、全纯函数层、常值层（截面是局部常值函数）。

---

点 $x \in X$ 处的**茎**（stalk）定义为如下余极限（即所有邻域截面的芽（germ）组成的对象）：

$$\mathcal F_x = \operatorname*{colim}_{U \ni x} \mathcal{F}(U)$$

## de Rham 理论
本部分参考 *Introduction to Smooth Manifolds* (GTM 218) 及 Bott, Tu *Differential Forms in Algebraic Topology* 的引入。

考虑[高阶同伦群的环路空间理解](@/posts/geometry_2_final.md)。由于同伦群不易计算，我们考虑对偶观点。一个连通分支满足这样的性质：所有局部常值函数都是常值的。我们让 $H^0(X)$ 是 $X$ 上局部常值实函数构成的向量空间。当 $X$ 的连通分支与道路连通分支一致且分支数有限时，有：

$$\operatorname{card} \pi_0(X) = \dim H^0(X)$$

在光滑流形 $M$ 上，这个空间正是零阶 de Rham 上同调 $H^0_{\mathrm{dR}}(M)$. 更一般地，de Rham 定理给出自然同构：

$$H^p_{\mathrm{dR}}(M) \cong H^p_{\mathrm{sing}}(M; \R)$$

{% admonition(type="definition", title="de Rham 上同调") %}
光滑流形 $M$ 的 $p$ 阶 de Rham 上同调是向量空间（按向量加法构成上同调群）：

$$H^p_{\mathrm{dR}}(M) = \frac{\ker(\mathrm d:\Omega^p(M)\to\Omega^{p+1}(M))}{\operatorname{im}(\mathrm d:\Omega^{p-1}(M)\to\Omega^p(M))}.$$
{% end %}

一个非平凡上同调群的例子是，在 $\R^2 \setminus \set{0}$ 中存在闭但非恰的 $1$-形式：

$$\omega = \frac{x\mathrm{d}y - y\mathrm{d}x}{x^2 + y^2}$$

对光滑流形（可带边）的光滑映射 $f: M \to N$，其拉回 $f^\ast: \Omega^p(N) \to \Omega^p(M)$ 会诱导一个上同调映射 $f^\ast: H^p_{\mathrm{dR}}(N) \to H^p_{\mathrm{dR}}(M)$.

$$
\begin{CD}
	\cdots @>>> \Omega^{p-1}(N) @>\mathrm{d}>> \Omega^p(N) @>\mathrm{d}>> \Omega^{p+1}(N) @>>> \cdots \cr
	@. @Vf^\ast VV @Vf^\ast VV @Vf^\ast VV @. \cr
	\cdots @>>> \Omega^{p-1}(M) @>\mathrm{d}>> \Omega^p(M) @>\mathrm{d}>> \Omega^{p+1}(M) @>>> \cdots
\end{CD}
$$

我们称一族 $h: \Omega^p(N) \to \Omega^{p-1}(M)$ 是一个**同伦算子/上链同伦**，如果：

$$\mathrm{d}h + h\mathrm{d} = g^\ast - f^\ast$$

{% admonition(type="theorem", title="同伦不变性") %}
若 $f, g: M \to N$ 光滑同伦，则它们诱导相同的 $f^\ast = g^\ast: H^p_{\mathrm{dR}}(N) \to H^p_{\mathrm{dR}}(M)$.
{% end %}

令 $i_t: M \to M \times I, i_t(x) = (x, t)$，令 $M \times \R$ 上的向量场 $S$ 是 $S_{(q, s)} = (0, \partial / \partial s|_s)$. 则 $i_0^\ast, i_1^\ast: \Omega^\ast(M \times I) \to \Omega^\ast(M)$ 间存在同伦算子：

$$h\omega = \int_0^1 i_t^\ast (S \lrcorner \omega) \mathrm{d}t$$

## 活动标架
Cartan 和陈省身在微分几何中发展了活动标架法：在每一点选取一组随点变化且适应几何对象的基，通过研究基的变化来提取曲率等几何量。

设 $M$ 是 $n$ 维光滑流形，$U \subseteq M$ 是开集。$U$ 上的一个**局部标架**是一组光滑向量场 $(e_1, \dots, e_n)$，使得对每个 $p \in U$，$(e_1(p), \dots, e_n(p))$ 都是 $T_pM$ 的一组基。标架通常只能局部选取，如球面 $\mathbb S^2$ 不存在全局光滑标架，但去掉一点可以。

若 $M$ 带有 Riemann 度规 $g$，并且 $g(e_i, e_j) = \delta_{ij}$，就称它是标准正交标架；若还与给定定向相容，称为定向标准正交标架。

与 $(e_i)$ 对偶的一组 $1$-形式 $(\theta^1, \dots, \theta^n)$ 称为**对偶标架**，由下式给出：

$$\theta^i(e_j) = \delta_{ij}$$

易见任意切向量场 $X$ 都可唯一写成（采用 Einstein 求和约定，重复出现的一个上标和一个下标默认求和）：

$$X = \theta^i(X)e_i$$

更一般地，对光滑映射 $f: N \to M$，可以在拉回丛 $f^\ast TM$ 上局部选取标架，称为沿 $f$ 的标架。当 $N$ 是区间时，$f^\ast TM$ 平凡，因而可全局选取沿曲线移动的一组基；当 $f$ 是浸入且 $M$ 带有 Riemann 度规时，可以局部让一部分基向量切于子流形，其余基向量法于子流形，这称为**适配标架**。

现在考虑光滑映射 $f: U \to \R^n$ 及沿 $f$ 的一组正交标架 $(e_1, \dots, e_n)$，我们定义：

$$\theta^i = \braket{\mathrm df, e_i}, \qquad \omega_i^{\ j} = \braket{\mathrm de_i, e_j}$$

于是有：

$$\mathrm df = e_i\theta^i, \qquad \mathrm de_i = e_j\omega_{ij}$$

由 $\mathrm d\braket{e_i, e_j} = 0$ 有 $\omega_i^{\ j} = -\omega_{ij}$. 又，使用 $\mathrm d^2 = 0$ 得到欧氏空间中的 Cartan 结构方程：

$$\mathrm d\theta^j + \omega_i^{\ j} \wedge \theta^i = 0, \qquad \mathrm d\omega_i^{\ j} + \omega_k^{\ j} \wedge \omega_i^{\ k} = 0 \tag{Cartan}$$

若令矩阵值 $1$-形式 $A$ 的第 $(j, i)$ 个分量为 $A_{ji} = \omega_i^{\ j}$，则第二式也可写成 $\mathrm dA + A\wedge A = 0$. 活动标架的主要计算都来自这两个方程。

## Levi-Civita 联络
从联络开始，考虑这种定义方式：

{% quote(by = "伍鸿熙、沈纯理、虞言林《黎曼几何初步》") %}
……所以想要定义出 $M$ 上的 $D_V X$，无疑要在 $M$ 上附加一个异于微分结构的结构。干脆设想这个附加结构不多不少正是 $D_V X$.
{% end %}

光滑流形 $M$ 上的一个**联络**就是对每一对（光滑）向量场 $V, X$，指定一个新的（光滑）向量场 $D_V X$，满足（其中 $f, g\in C^\infty(M)$）：

$$
\begin{align*}
	D_{fV + gW} X = fD_V X + gD_W X \tag{C1} \cr
	D_V fX = (Vf) X + fD_V X \tag{C2} \cr
	D_V (X+Y) = D_V X + D_V Y \tag{C3}
\end{align*}
$$

指定一个联络后，称 $D_V X$ 为 $X$ 沿 $V$ 的协变导数。$D$ 有时也用记号 $\nabla$.

由于对一组联络 $D^i$ 和满足 $\sum f_i = 1$ 的光滑函数 $f_i$ 有 $\sum f_i D^i$ 也是联络，在局部上使用 $\R^n$ 的方向导数，知整体上联络一定存在。

{% admonition(type="theorem", title="Levi-Civita 联络") %}
对 $M$ 上给定的黎曼度量 $g$，存在唯一的联络 $D$ 满足，对任意向量场 $X, Y, Z$ 有：

$$
\begin{align*}
	X \braket{Y, Z} = \braket{D_X Y, Z} + \braket{Y, D_X Z} \tag{L1} \cr
	D_X Y - D_Y X - [X, Y] = 0 \tag{L2}
\end{align*}
$$

这里 $[X, Y]$ 定义为 $[X, Y]f = X(Yf) - Y(Xf)$.
{% end %}

先证唯一性。在某个坐标邻域内（坐标函数 $x^i$）定义 $\Gamma_{ij}^k$ 为：

$$D_{\partial / \partial x^i} \frac{\partial}{\partial x^j} = \Gamma_{ij}^k \frac{\partial}{\partial x^k}$$

容易发现条件 (L2) 等价于 $\Gamma_{ij}^k = \Gamma_{ji}^k$. 我们再记：

$$g_{ij} \equiv \left\langle\frac{\partial}{\partial x^i}, \frac{\partial}{\partial x^j}\right\rangle$$

那么由 (L1) 知：

$$\frac{\partial g_{jk}}{\partial x^i} = g_{lk} \Gamma_{ij}^l + g_{jl} \Gamma_{ik}^l$$

使用一个经典的技巧，考虑上式的轮换对称，就可得到：

$$2g_{lk} \Gamma_{ij}^l = \frac{\partial g_{ki}}{\partial x^j} + \frac{\partial g_{kj}}{\partial x^i} - \frac{\partial g_{ij}}{\partial x^k}$$

故由 $g$ 唯一确定。将此式作为定义式也知存在性。可以整理成如下 Koszul 公式：

$$\braket{D_X Y, Z} = \frac 1 2 (X \braket{Y, Z} + Y \braket{Z, X} - Z \braket{X, Y} + \braket{Z, [X, Y]} + \braket{Y, [Z, X]} - \braket{X, [Y, Z]})$$

## 配边理论
两个 $n$ 维闭流形 $M, N$ 称为**配边**的，如果存在一个 $n + 1$ 维紧流形 $W$，使得：

$$\partial W \cong M \sqcup N$$

一个典型的例子是裤子状曲面将一个圆与两个圆配边，反例是 $\R\mathrm{P}^2$ 与 $\emptyset$ 不配边。

如果考虑的是定向流形，则需要 $W$ 是定向的，关系式改为 $\partial W \cong M \sqcup (-N)$.

设 $M^m$ 是闭光滑流形，并选取光滑嵌入 $i: M \hookrightarrow \R^{m+k}$. 记 $i$ 的秩 $k$ 法丛为 $\nu_i$，一个法丛标架是向量丛同构：

$$\varphi: \nu_i \cong M \times \R^k$$

将嵌入加入额外的平凡法方向，会把 $\varphi$ 替换为 $\varphi \oplus \mathrm{id}_{\R}$. 如果两个法丛标架在分别加入有限个平凡方向后可以通过一族法丛标架相连，就称它们稳定等价；不同高维嵌入给出的稳定法丛也按这种方式识别。这样的等价类称为 $M$ 的稳定法丛标架，会诱导流形的定向。两个带稳定标架的闭 $m$ 维流形 $M, N$ 称为带标架配边的，如果存在 $m + 1$ 维紧流形 $W$ 及其稳定法丛标架，使得：

$$\partial W \cong M_0 \sqcup (-M_1)$$

并且 $W$ 的标架按照外法向优先的边界约定，在两个边界分支上分别限制为给定标架；负号表示反转诱导定向及相应的边界标架。带标架配边类关于不交并构成 Abel 群，记为 $\Omega_m^{\mathrm{fr}}$；单位元由空流形表示，逆元可由反转标架中的一个法向量得到。

球面的第 $m$ 个稳定同伦群定义为悬挂映射构成的归纳系统的余极限：

$$\pi _m^{\mathrm S}(\mathbb S) = \operatorname*{colim} _{k \to \infty} \pi _{m+k}(\mathbb S^k)$$

Pontryagin–Thom 构造给出了如下结果：

$$\Omega_m^{\mathrm{fr}} \cong \pi_m^{\mathrm S}(\mathbb S)$$
