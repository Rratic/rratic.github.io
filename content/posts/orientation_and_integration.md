+++
title = "光滑流形上的定向与积分"
date = 2026-07-20

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

在光滑流形上我们可以对微分形式作积分，其定义需要一些关于定向的说明。基于这一套语言可以给出 Stokes 定理。

<!-- more -->

本文参考 *Introduction to Smooth Manifolds* (GTM 218)，读者可先参阅[光滑流形上的微分与微分形式](@/posts/differential_forms.md)。

## 定向
对向量空间来说，考虑等价关系是两组有序基的变换矩阵行列式为正。一个 $V$ 的定向是指一个等价类。易知这对应于 $\Lambda^n(V^\ast) \setminus \set{0}$ 的两支。当我们指定一个定向是正的时，可以称另一个是负的。

流形 $M$ 上的一个定向是指，对每一点处的切空间选择一个定向，且是连续的。此时称 $M$ 是**可定向**的。或者，也可以用不取零的光滑 $n$-形式定义。

对于可定向光滑流形 $M_1, \dots, M_k$，有一个积定向对应于：

$$\pi_1^\ast \omega_1 \wedge \cdots \wedge \pi_k^\ast \omega_k$$

如果一个流形可平行化（有光滑的全局标架），则易见它可定向。

---

考虑 $M$ 是光滑流形（可带边），$S \subseteq M$ 是浸入/嵌入的光滑子流形（可带边）。有一个沿着 $S$ 的向量场 $N: S \to TM$ 使得 $N_p \in T_pM$.

{% admonition(type="theorem", title="超平面的定向") %}
$M$ 是可定向光滑流形（可带边），$S$ 是的超平面（可带边），存在沿着 $S$ 的向量场 $N$ 处处与 $S$ 不相切，则 $S$ 可定向。
{% end %}

对于 $M$ 的定向形式 $\omega$，考察 $S$ 上的 $(n-1)$-形式 $\sigma$，定义为 $\sigma_p(v_1, \dots, v_{n-1}) = \omega_p(N(p), v_1, \dots, v_{n-1})$. 由不相切有 $\sigma_p \neq 0$.

其一个推论是 $\mathbb S^n$ 可定向，因为可以在 $\R^{n+1}$ 中让：

$$N = \sum x^i \frac{\partial}{\partial x^i}$$

此定向被作为 $\mathbb S^n$ 的标准定向。

{% admonition(type="theorem", title="边界的定向") %}
$M$ 是可定向的光滑带边流形（$n \geq 1$），则 $\partial M$ 可定向，且 $\partial M$ 所有向外的向量场决定相同的定向。
{% end %}

同上一证明。

{% admonition(type="definition", title="Riemann 体积形式") %}
$(M, g)$ 是可定向的 Riemann 流形（可带边；$n \geq 1$），则存在唯一的光滑定向形式（称为 **Riemann 体积形式**）$\omega_g \in \Omega^n(M)$，使得对任意局部定向标准正交标架 $(E_i)$ 有：

$$\omega_g(E_1, \dots, E_n) = 1$$
{% end %}

取 $(\varepsilon^1, \dots, \varepsilon^n)$ 是 $E_1, \dots, E_n$ 的对偶，只需：

$$\omega_g = \varepsilon^1 \wedge \cdots \wedge \varepsilon^n$$

对定向的光滑坐标 $x^i$ 如果考虑写为：

$$\omega_g = f \mathrm{d}x^1 \wedge \cdots \wedge \mathrm{d}x^n$$

则可以计算（设 $\partial / \partial x^i = A_i^j E_j$；遵循 Einstein 求和约定）：

$$
f = \omega_g\left(\frac{\partial}{\partial x^1}, \dots, \frac{\partial}{\partial x^n}\right) =
\varepsilon^1 \wedge \cdots \wedge \varepsilon^n\left(\frac{\partial}{\partial x^1}, \dots, \frac{\partial}{\partial x^n}\right) =
\det (A_i^j)
$$

$$
g_{ij} = \left\langle\frac{\partial}{\partial x^i}, \frac{\partial}{\partial x^j}\right\rangle_g =
\braket{A_i^k E_k, A_j^l E_l}_g =
A_i^k A_j^l \braket{E_k, E_l}_g =
A_i^k A_j^k
$$

故有 $f = \sqrt{\det (g_{ij})}$.

{% admonition(type="theorem", title="覆盖的定向") %}
设 $\widetilde{M}$ 是连通、可定向的光滑流形（可带边），且 $\pi: \widetilde{M} \to M$ 是光滑正则复叠，则 $M$ 可定向当且仅当 $\mathrm{Aut}_\pi(\widetilde{M})$ 在 $\widetilde{M}$ 上的作用是保定向的。
{% end %}

记 $\widetilde{M}$ 的给定定向 $\mathcal{O} _{\widetilde{M}}$. 假设 $M$ 可定向，任取其一点 $q$，由于 $M$ 连通，恰有两个定向，其一使得 $\mathrm{d}\pi _q: T _q\widetilde{M} \to T _{\pi(q)}M$ 保定向，记作 $\mathcal{O} _M$. 对 $\varphi \in \mathrm{Aut} _\pi(\widetilde{M})$ 由 $\pi \circ \varphi = \pi$ 知保定向。

假设所有的作用都保定向，设 $p \in M$，$U$ 是 $p$ 的被均匀覆盖的邻域，则存在光滑截面 $\sigma: U \to \widetilde{M}$ 诱导一个 $U$ 上的定向。对另外的局部光滑截面 $\sigma_1: U \to \widetilde{M}$ 存在复叠变换 $\varphi$ 使得 $\sigma_1 = \varphi \circ \sigma$. 由此，可以用局部光滑截面诱导的拉回定向定义 $M$ 的定向。

这典型的应用是 $\mathbb{RP}^n$ 可定向当且仅当 $n$ 为奇。

## 流形上的积分
### 微分形式的积分
在流形上，如果不附加 Riemann 度规这样的额外结构，没有办法独立于坐标系对实值函数积分。例如闭球的体积会被坐标系变换影响。而另一方面，我们可以沿着曲线对 $1$-形式积分：

$$\int_{[a, b]} (f(t) \mathrm{d}t) = \int_a^b f(t) \mathrm{d}t$$

为了符合关于体积的直觉，可以发现我们需要积的东西是线性、交错的，因此考虑对微分形式积分。

先考虑在的积分域（边界零测的有界集）上积分。对其上的（连续）$n$-形式 $\omega$，我们用多元微积分定义：

$$\int_D f \mathrm{d}x^1 \wedge \cdots \wedge \mathrm{d}x^n = \int_D f \mathrm{d}x^1 \cdots \mathrm{d}x^n$$

而如果定义在 $\R^n$（或 $\mathbb{H}^n$）的子集 $U$ 上的 $\omega$ 有紧支集是积分域 $D$，则我们定义（与 $D$ 的选取无关）：

$$\int_U \omega = \int_D \omega$$

可以验证这样的性质：假设 $D, E$ 是 $\R^n$ 或 $\mathbb{H}^n$ 上的开积分域，$G: \bar D \to \bar E$ 是一个可以限制为 $D \to E$ 的保定向/反定向微分同胚的光滑映射；若 $\omega$ 是 $\bar E$ 上的 $n$-形式，则（符号取决于保定向还是反定向）：

$$\int_D G^\ast \omega = \int_E \omega$$

---

对 $M$ 是可定向光滑流形（可带边），若 $n$-形式 $\omega$ 在单个图卡 $(U, \varphi)$ 上被紧支撑，则可以定义 $\omega$ 在 $M$ 上的积分是（符号取决于图卡的定向）：

$$\int_M \omega = \pm \int_{\varphi(U)} (\varphi^{-1})^\ast \omega$$

一般地，对 $\omega$ 紧支，设 $\set{U_i}$ 是 $\operatorname{supp} \omega$ 的有限开覆盖，令 $\set{\psi_i}$ 是其单位分解[^partition-of-unity]，则可定义：

$$\int_M \omega = \sum_i \int_M \psi_i \omega$$

{% admonition(type="example", title="球面") %}
考虑单位球面上由 $\R^3$ 给出的 $2$-形式：

$$\omega = x\mathrm{d}y\wedge\mathrm{d}z + y\mathrm{d}z\wedge\mathrm{d}x + z\mathrm{d}x\wedge\mathrm{d}y$$
{% end %}

令 $D = (0, \pi) \times (0, 2\pi)$，映射 $F: \bar D \to \mathbb S^2$，其中 $F(\varphi, \theta) = (\sin\varphi \cos\theta, \sin\varphi \sin\theta, \cos\varphi)$ 保定向，这使得：

$$
\begin{cases}
	F^\ast \mathrm{d}x = \cos\varphi \cos\theta \\,\mathrm{d}\varphi - \sin\varphi \sin\theta \\,\mathrm{d}\theta \cr
	F^\ast \mathrm{d}y = \cos\varphi \sin\theta \\,\mathrm{d}\varphi + \sin\varphi \cos\theta \\,\mathrm{d}\theta \cr
	F^\ast \mathrm{d}z = -\sin\varphi \\,\mathrm{d}\varphi
\end{cases}
$$

从而有：

$$\int_{\mathbb S^2} \omega = \int_D \sin\varphi \\,\mathrm{d}\varphi \wedge \mathrm{d}\theta = 4\pi$$

### Stokes 定理
{% admonition(type="theorem", title="Stokes 定理") %}
设 $M$ 是可定向光滑 $n$-维带边流形，$\omega$ 是其上的光滑紧支 $(n - 1)$ 形式，则：

$$\int_M \mathrm{d}\omega = \int_{\partial M} \omega$$
{% end %}

先假设 $M$ 是上半空间 $\mathbb{H}^n$，由 $\omega$ 紧支只需考虑某个 $A = [-R, R] \times \cdots \times [-R, R] \times [0, R]$. 我们设（用 hat 表示跳过）：

$$\omega = \sum_{i=1}^n \omega_i \mathrm{d}x^1 \wedge \cdots \wedge \widehat{\mathrm{d}x^i} \wedge \cdots \wedge \mathrm{d}x^n$$

然后强行计算即可，$\R^n$ 也是同理。对一般的带边流形，用图卡拆分即可。

{% admonition(type="theorem", title="Green 定理") %}
对 $\R^2$ 的紧正则区域 $D$，及其上光滑实值函数 $P, Q$，有：

$$\int_D \left(\frac{\partial Q}{\partial x} - \frac{\partial P}{\partial y}\right) \mathrm{d}x\mathrm{d}y = \int_{\partial D} P\mathrm{d}x + Q\mathrm{d}y$$
{% end %}

对 $P\mathrm{d}x + Q\mathrm{d}y$ 用 Stokes 定理即可。

为了更广泛地使用 Stokes 定理，我们也会考虑带角落的流形，证法类似。

{% admonition(type="theorem", title="路径无关性") %}
对光滑流形 $M$ 及 $\gamma, \gamma': [a, b] \to M$ 是定端同伦的逐段光滑曲线，则对任意闭的 $1$-形式，有：

$$\int_{\gamma} \omega = \int_{\gamma'} \omega$$
{% end %}

考察光滑同伦 $H: [a, b] \times I \to M$，有：

$$\int_{\partial ([a, b] \times I)} H^\ast \omega = \int_{[a, b] \times I} \mathrm{d}(H^\ast \omega) = \int_{[a, b] \times I} H^\ast \mathrm{d}\omega = 0$$

然后分析即可。其推论是，在单连通的光滑流形上，所有闭的 $1$-形式是恰当的。

### Riemann 流形上的积分
我们将度规对应的 Riemann 体积形式 $\omega_g$ 写作 $\mathrm{d}V_g$，这并不意味着 $\omega_g$ 是恰当的，只是记号上的便利。

回忆之前结论，对紧支的连续实值非负的 $f$，只需考虑 $f$ 在单个定向光滑图卡上被支撑，有：

$$\int_M f \mathrm{d}V_g = \int_{\varphi(U)} f(x) \sqrt{\det (g_{ij})} \mathrm{d}x^1 \cdots \mathrm{d}x^n \geq 0$$

### 密度
对 $n$ 维线性空间 $V$ 来说，其上的密度是函数：

$$\mu: \underbrace{V \times \cdots \times V}_{n \text{ copies}} \to \R$$

且满足对线性映射 $T$ 有 $\mu(Tv_1, \dots, Tv_n) = |\det T| \mu(v_1, \dots, v_n)$. 用 $\mathcal{D}(V)$ 表示 $V$ 上所有的密度。

{% admonition(type="theorem", title="密度的性质") %}
1. $\mathcal{D}(V)$ 是线性空间
2. 若某个基下 $\mu_1(e_1, \dots, e_n) = \mu_2(e_1, \dots, e_n)$，则 $\mu_1 = \mu_2$
3. $\omega \in \Lambda^n(V^\ast)$ 诱导一个密度 $|\omega|$，其中 $|\omega|(v_1, \dots, v_n) = |\omega(v_1, \dots, v_n)|$
4. $\mathcal{D}(V)$ 是一维的
{% end %}

由此我们定义正密度是满足 $\mu(v_1, \dots, v_n) > 0$ 的密度，负密度同理。我们定义密度丛：

$$\mathcal{D}M = \bigsqcup_{p \in M} \mathcal{D}(T_pM)$$

---

[^partition-of-unity]: 即让 $\sum \psi_\alpha = 1$，且每个 $\operatorname{supp} \psi_\alpha$ 含于某个 $U_i$.
