+++
title = "黎曼几何（一）：联络与测地线的变分"
date = 2026-10-02

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

由于讨论班需要，进行一些补习。参考《黎曼几何初步》及 Milnor *Morse Theory* 第二、三部分及[学长的笔记](https://www.zhihu.com/column/c_2076322926907998524)。

<!-- more -->

## 基本定义
### 联络
从联络开始，考虑这种定义方式：

{% <quote by = "伍鸿熙、沈纯理、虞言林《黎曼几何初步》"> %}
……所以想要定义出 $M$ 上的 $D_V X$，无疑要在 $M$ 上附加一个异于微分结构的结构。干脆设想这个附加结构不多不少正是 $D_V X$.
{% </quote> %}

光滑流形 $M$ 上的一个**联络**就是对每一对（光滑）向量场 $V, X$，指定一个新的（光滑）向量场 $D_V X$，满足（其中 $f, g\in C^\infty(M)$）：

$$
\begin{align*}
	D_{fV + gW} X = fD_V X + gD_W X \tag{C1} \cr
	D_V fX = (Vf) X + fD_V X \tag{C2} \cr
	D_V (X+Y) = D_V X + D_V Y \tag{C3}
\end{align*}
$$

指定一个联络后，称 $D_V X$ 为 $X$ 沿 $V$ 的协变导数。$D$ 有时也用记号 $\nabla$，或者用记号：

$$V \vdash X$$

由于对一组联络 $D^i$ 和满足 $\sum f_i = 1$ 的光滑函数 $f_i$ 有 $\sum f_i D^i$ 也是联络，在局部上使用 $\R^n$ 的方向导数，知整体上联络一定存在。

{% <theorem title="Levi-Civita 联络"> %}
对 $M$ 上给定的黎曼度量 $g$，存在唯一的联络 $D$ 满足，对任意向量场 $X, Y, Z$ 有：

$$
\begin{align*}
	X \braket{Y, Z} = \braket{D_X Y, Z} + \braket{Y, D_X Z} \tag{L1} \cr
	D_X Y - D_Y X - [X, Y] = 0 \tag{L2}
\end{align*}
$$

这里 $[X, Y]$ 定义为 $[X, Y]f = X(Yf) - Y(Xf)$.
{% </theorem> %}

先证唯一性。在某个坐标邻域内（坐标函数 $x^i$）定义 Christoffel 记号 $\Gamma_{ij}^k$ 为：

$$D_{\partial / \partial x^i} \frac{\partial}{\partial x^j} = \Gamma_{ij}^k \frac{\partial}{\partial x^k}$$

容易发现条件 $(\text L2)$ 等价于 $\Gamma_{ij}^k = \Gamma_{ji}^k$. 我们再记：

$$g_{ij} \equiv \left\langle\frac{\partial}{\partial x^i}, \frac{\partial}{\partial x^j}\right\rangle$$

那么由 $(\text L1)$ 知：

$$\frac{\partial g_{jk}}{\partial x^i} = g_{lk} \Gamma_{ij}^l + g_{jl} \Gamma_{ik}^l$$

使用一个经典的技巧，考虑上式的轮换对称，就可得到：

$$2g_{lk} \Gamma_{ij}^l = \frac{\partial g_{ki}}{\partial x^j} + \frac{\partial g_{kj}}{\partial x^i} - \frac{\partial g_{ij}}{\partial x^k}$$

故由 $g$ 唯一确定。将此式作为定义式也知存在性。可以整理成如下 Koszul 公式：

$$\braket{D_X Y, Z} = \frac 1 2 (X \braket{Y, Z} + Y \braket{Z, X} - Z \braket{X, Y} + \braket{Z, [X, Y]} + \braket{Y, [Z, X]} - \braket{X, [Y, Z]})$$

考虑联络的另一种看法。设 $\gamma: [a, b] \to M$ 是一条嵌入曲线，称向量场 $X$ 是沿 $\gamma$ **平行**的，如果 $D_{\dot \gamma} X = 0$. 如若 $X(a) = v, X(b) = w$，称 $w$ 是 $v$ 沿 $\gamma$ 平行移动的结果。对于 $\R^n$ 上平坦度量给出的 Levi-Civita 联络（称为标准联络），沿 $\gamma$ 平行表明 $X$ 是我们熟悉的平行向量场。

对浸入曲线 $\gamma$，可以分段作上述平行移动，从而给出了一个同构，称为平移同构：

$$\mathbf P^\gamma: M_{\gamma(a)} \to M_{\gamma(b)}$$

这表明，联络联络的是切空间。读者可验证 $(\text L1)$ 等价于所有平移同构都是切空间作为内积空间的等距同构。

我们称满足 $D_{\dot \gamma} \dot \gamma = 0$ 的曲线为联络的**测地线**，这是直线的推广，使用 ODE 的结果有满足 $\gamma(0) = x, \dot \gamma(0) = v$ 的测地线是局部存在且唯一的。

### 协变微分
回忆向量空间的同构 $\varphi: V \to W$ 可以自然诱导张量代数之间的同构：

$$\tilde \varphi: T^\ast(V) \to T^\ast(W)$$

$$T^\ast(V) = \bigoplus_{r, s} T^{r, s}(V)$$

对 $v \in T_xM$，取 $\dot \gamma(0) = v$ 的曲线，由平行移动诱导一个同构：

$$\tilde{\mathbf P} _t: T^\ast(M _{\gamma(0)}) \to T^\ast(M _{\gamma(t)})$$

从而我们定义**协变导数**：

$$\nabla_vK = \frac{\mathrm d}{\mathrm dt} [\tilde{\mathbf P} _t^{-1}(K(\gamma(t)))]| _{t=0}$$

它与 $\gamma$ 选取无关、会保持张量场类型、与[缩并](@/posts/general_relativity_1.md)交换，且是作用在张量场代数上的**导子**，即：

$$D_v(K_1 \otimes K_2) = D_v(K_1) \otimes K_2 + K_1 \otimes D_v(K_2)$$

这可以通过取 $\gamma$ 的一组平行向量场的基（及其 $1$-形式对偶基）证明。

我们可以直接看 $\nabla K$ 为 $(r, s+1)$-型张量，称为**协变微分**。对函数（即 $(0, 0)$-型张量）$f$ 即有 $\nabla f = \mathrm df$，对对称的联络我们称 $\nabla^2 f$ 是 $f$ 的 Hessian.

通过用缩并定义迹，可以定义 **Laplace-Beltrami 算子/二阶微分算子**：

$$\Delta f = \operatorname{tr} \nabla^2 f$$

### 曲率张量
我们定义**曲率算子**（并约定符号）是：

$$R_{XY} = -\nabla_X\nabla_Y + \nabla_Y\nabla_X + \nabla_{[X, Y]}$$

它是张量场代数上的导子、会保持张量场类型、会将函数打到 $0$，且：

$$R_{(fX)Y}K = R_{X(fY)}K = R_{XY}(fK) = fR_{XY}K$$

**曲率张量**是指 $R_{XY}Z$ 或者：

$$R(X, Y, Z, W) = \braket{R_{XY}Z, W}$$

{% <note title="曲率张量是度量的二阶不变量"> %}
取局部坐标系 $\set{x^i}$，我们可以通过琐碎的计算得到：

$$
\begin{aligned}
R_{ijkl} = &\frac 1 2 \left(\frac{\partial g_{il}}{\partial x^j \partial x^k} + \frac{\partial g_{jk}}{\partial x^i \partial x^l} - \frac{\partial g_{ik}}{\partial x^j \partial x^l} - \frac{\partial g_{jl}}{\partial x^i \partial x^k}\right) \\\\
    &+ \sum_{r,s} (g_{rs} \Gamma_{jk}^r \Gamma_{il}^s + g_{rs} \Gamma_{jl}^r \Gamma_{ik}^s)
\end{aligned}
$$
{% </note> %}

曲率张量的基本性质是：关于前两元反对称、关于后两元反对称、$R(X, Y, Z, W) = R(Z, W, X, Y)$ 及第一 Bianchi 恒等式 $R_{XY}Z + R_{YZ}X + R_{ZX}Y = 0$，由此 $Q(X, Y) = R(X, Y, X, Y)$ 完全决定了曲率张量。

对于 $T_xM$ 的二维子空间 $\Pi = \mathrm{span}\set{v_1, v_2}$，我们定义其**截面曲率**为：

$$K(\Pi) = \frac{R(v_1, v_2, v_1, v_2)}{|v_1 \wedge v_2|^2}$$

这里 $|v_1 \wedge v_2|^2 = |v_1|^2 \cdot |v_2|^2 - \braket{v_1, v_2}^2$，可见与基的选取无关。实际上可以取一组单位正交基去掉分母。

对二维黎曼流形，可以发现截面曲率恰好是 [Gauss 曲率](@/posts/geometry_2_midterm.md)。

{% <theorem title="第二 Bianchi 恒等式"> %}
曲率张量 $R_{XY}Z$ 适合：

$$(D_X R) _{YZ} + (D_Y R) _{ZX} + (D_Z R) _{XY} = 0$$
{% </theorem> %}

通过下式轮换求和：

$$D _X (R _{YZ} W) = (D _X R) _{YZ} W + R _{D _X Y, Z} W + R _{Y, D _X Z} W + R _{YZ} (D _X W)$$

{% <theorem title="Ricci 恒等式"> %}
对于张量场 $T$ 有：

$$D^2 T(\dots, X, Y) - D^2 T(\dots, Y, X) = (R_{XY} T)(\dots)$$
{% </theorem> %}

$$D^2 T(\dots, X, Y) = (D_Y (D_X T))(\dots) - (D_{D_Y X} T)(\dots)$$

最后我们提及 **Ricci 张量**是曲率张量的如下缩并（取单位正交基）：

$$\mathrm{Ric}(X, Y) = \sum_{i=1}^n R(e_i, X, e_i, Y)$$

## 测地线
回忆测地线是满足 $\nabla_{\dot \gamma} \dot \gamma = 0$ 的参数曲线。我们有：

$$\nabla_{\dot \gamma} \braket{\dot \gamma, \dot \gamma} = 2\braket{\nabla_{\dot \gamma} \dot \gamma, \dot \gamma} = 0$$

也就是 $\lVert\dot \gamma\rVert$ 为常值。故弧长函数 $L$ 是线性的。

{% <theorem title="引理"> %}
对 $p_0 \in M$，存在其一个邻域 $U$ 及 $\varepsilon > 0$，使得对任意 $p$ 及长度小于 $\varepsilon$ 的切向量 $v \in T_p M$，有唯一的测地线 $\gamma_v: (-2, 2) \to M$ 满足 $\gamma_v(0) = p, \dot \gamma_v(0) = v$.
{% </theorem> %}

用 ODE 证明存在充分小的测地线，然后让 $t \mapsto \gamma(ct)$.

我们令 $v \in T_qM$，假定存在测地线 $\gamma: [0, 1] \to M$ 使得 $\gamma(0) = q, \dot \gamma(0) = v$，我们记 $\gamma(1) = \exp_q(v)$，称为**指数映射**。此时有 $\gamma(t) = \exp_q(tv)$.

{% <definition title="测地完备"> %}
流形 $M$ 称为测地完备的，如果对所有的 $q$ 和 $v \in T_qM$ 都有 $\exp_q(v)$ 有定义。这等价于所有测地线段都可延拓成无限长的。
{% </definition> %}

考虑 $F(p, v) = (p, \exp_p(v))$，计算 Jacobi 知在 $(p, 0)$ 附近是微分同胚。

我们称一条测地线最短，如果其长度小于等于任意分段光滑曲线的长度。

我们的目标是：

{% <theorem title="Hopf–Rinow 定理"> %}
对黎曼流形 $M$，以下条件等价：
1. $M$ 作为一个度量空间（令 $d = \inf L$）完备
2. $M$ 测地完备
3. $M$ 的有界闭集紧
{% </theorem> %}

(1) 推 (2)、(2) 推 (3) 容易。我们来证：若存在 $x$，所有 $\exp_x(v)$ 有定义，则 (3) 成立。

令 $\bar B_r = \exp_x \overline{B(\mathbf 0; r)}$，它是紧的。我们只需要证明 $M$ 中每一个点可以用一条最短测地线与 $q$ 相连。

令 $\bar{\mathscr B}(r) = \set{y \in M | d(x, y) \leq r}$，$\Sigma(r)$ 是其中可以用一条最短测地线与 $x$ 相连点构成的子集。再令：

$$\mathscr T = \set{r \in [0, \infty) | \bar{\mathscr B}(r) = \Sigma(r)}$$

这对充分小的 $r$ 成立，故非空，只需证它既开又闭。

完备黎曼流形的例子有：
- 具有平坦度量的 $\R^n$
- 作为 $\R^n$ 闭子集的子流形
- 紧黎曼流形
- 齐性黎曼流形（对 $x, y$ 存在等距同构 $\varphi$ 使得 $\varphi(x) = y$）

## 测地线的变分
### 变分
我们用 $\Omega_{p, q}(M)$ 表示所有从 $p$ 到 $q$ 的分段光滑 $\omega: [0, 1] \to M$ 构成的集合。之后我们会给它拓扑结构。

定义 $T_\omega \Omega$ 是所有沿着 $\omega$、端点处为 $0$ 的分段光滑向量场构成的线性空间。

一个 $\omega$ 的固定端点的**变分**是一个函数 $\bar \alpha: (-\varepsilon, \varepsilon) \to \Omega$，满足 $\bar \alpha(0) = \omega$、固定端点，且分段光滑。这里分段光滑意为存在 $0 = t_0 < \cdots < t_n = 1$ 使得 $\alpha(u, t) = \bar \alpha(u)(t)$ 在 $(-\varepsilon, \varepsilon) \times [t_{i-1}, t_i]$ 上光滑。

上述 $(-\varepsilon, \varepsilon)$ 可以改为一般的 $\R^n$ 中开集，此时称为 $n$-参数的变分。

变分可以被看成某种 $\Omega$ 中的路径。我们记变分 $\alpha$ 相关的**变分向量场**：

$$W_t = \frac{\partial \alpha}{\partial u}(0, t)$$

{% <definition title="临界道路"> %}
称道路 $\omega$ 是 $F: \Omega \to \R$ 的临界道路，如果对所有变分 $\bar \alpha$，

$$\left.\frac{\mathrm d(F(\bar \alpha(u)))}{\mathrm du}\right|_{u=0} = 0$$
{% </definition> %}

### 能量
对 $\omega \in \Omega$ 我们定义其**能量**：

$$E_a^b(\omega) = \int_a^b \lVert\dot \omega\rVert^2 \mathrm dt$$

由 Cauchy-Schwarz 易见：

$$(L_a^b)^2 \leq (b-a) E_a^b$$

这意味着，对完备黎曼流形及 $p, q \in M$，能量函数只在最短测地线集合上取到最小值。

为了方便陈述接下来的定理，我们令 $W_t$ 是变分向量场，令 $V_t = \dot \omega$ 是速度，$A_t = \ddot \omega = \nabla_{\dot \omega} \dot \omega$ 是加速度，$\Delta_t V = V_{t+} - V_{t-}$ 是在坏点处的跳跃，则：

{% <theorem title="第一变分公式"> %}
$$\frac 1 2 \left.\frac{\mathrm dE(\bar \alpha(u))}{\mathrm du}\right|_{u=0} = - \sum_t \braket{W_t, \Delta_t V} - \int_0^1 \braket{W_t, A_t} \mathrm dt$$
{% </theorem> %}

计算即可。

其推论是，$\omega$ 是测地线当且仅当它是 $E$ 的临界点。左推右容易。考虑一个变分 $W_t = f(t)A_t$ 使得 $f$ 在坏点消没，在其它点正。则 $f(t) \braket{A_t, A_t}$ 积分为零，故 $\omega$ 是未坏的测地线。

### 能量的 Hessian
我们定义 Hessian 是：对 $W_1, W_2 \in T_\gamma \Omega$，取 $2$-参数变分 $\alpha: U \times [0, 1] \to M$ 使得 $\alpha(0, 0, \cdot) = \gamma$，$\frac{\partial \alpha}{\partial u_i}(0, 0, t) = W_i(t)$，令：

$$E_{\ast\ast}(W_1, W_2) = \left.\frac{\partial^2 E(\bar \alpha(u_1, u_2))}{\partial u_1 \partial u_2}\right|_{(0, 0)}$$

以下定理说明它是良定、对称、双线性的：

{% <theorem title="第二变分公式"> %}
$$\frac 1 2 E_{\ast\ast}(W_1, W_2) = - \sum_t \braket{W_2(t), \Delta_t \dot W_1} - \int_0^1 \braket{W_2, \ddot W_1 + R(\dot \gamma, W_1) \dot \gamma} \mathrm dt$$
{% </theorem> %}

一个推论是，若 $\gamma$ 是 $p$ 到 $q$ 的最短测地线，则 $E_{\ast\ast}$ 是半正定的。

### Jacobi 场
一个沿着测地线 $\gamma$ 的向量场 $J$ 称为 **Jacobi 场**，如果：

$$\ddot J + R(\dot \gamma, J) \dot \gamma = 0$$

由 ODE，$J$ 被其初始条件 $J(0), \dot J(0)$ 唯一决定。

{% <theorem title="一个有趣的引理"> %}
可以作分解 $J = J^\perp + (at + b) \dot \gamma$，其中 $\braket{J^\perp, \dot \gamma} = 0$.
{% </theorem> %}

$$\frac{\mathrm d^2}{\mathrm dt^2} \braket{J, \dot \gamma} = \frac{\mathrm d}{\mathrm dt} \braket{\dot J, \dot \gamma} = \braket{\ddot J, \dot \gamma} = \braket{- R_{\dot \gamma J} \dot \gamma, \dot \gamma} = 0$$

{% <definition title="共轭"> %}
称 $p = \gamma(a), q = \gamma(b)$ 沿着 $\gamma$ 共轭，如果存在沿着 $\gamma$ 的非零 Jacobi 场 $J$ 在 $t = a, b$ 处消没。$p$ 和 $q$ 作为共轭的重数（$q = \exp_p(v)$ 时的 $\dim \ker \mathrm d(\exp_p) _v$）等于所有这样的 Jacobi 场构成线性空间的维数。
{% </definition> %}

我们来考虑 Hessian 的零空间，即满足对任意 $W_2$ 有 $E_{\ast\ast}(W_1, W_2) = 0$ 的 $W_1$.

{% <theorem> %}
$W_1$ 属于 $E_{\ast\ast}$ 的零空间当且仅当 $W_1$ 是 Jacobi 场。因此 $E_{\ast\ast}$ 退化当且仅当端点沿着 $\gamma$ 共轭。
{% </theorem> %}

使用第二变分公式。

这可以说明 $E_{\ast\ast}$ 的零化度是有限的。实际上零化度将满足 $0 \leq \nu < n$.

{% <theorem> %}
如果（不一定保持端点）的 $\alpha$ 使得每个 $\bar \alpha(u)$ 是测地线，则变分向量场 $W_t$ 是沿着 $\gamma$ 的 Jacobi 场。
{% </theorem> %}

由条件，$\nabla_{\partial \alpha / \partial u} \partial \alpha / \partial u = 0$，有下式，然后依定义。

$$
\begin{align*}
0 &= \nabla_{\frac{\partial \alpha}{\partial u}} \nabla_{\frac{\partial \alpha}{\partial t}} \frac{\partial \alpha}{\partial t} \cr
&= \nabla_{\frac{\partial \alpha}{\partial t}} \nabla_{\frac{\partial \alpha}{\partial u}} \frac{\partial \alpha}{\partial t} + R(\frac{\partial \alpha}{\partial t}, \frac{\partial \alpha}{\partial u})\frac{\partial \alpha}{\partial t} \cr
&= \nabla_{\frac{\partial \alpha}{\partial t}} \nabla_{\frac{\partial \alpha}{\partial t}} W_t + R(\frac{\partial \alpha}{\partial t}, W_t) W_t
\end{align*}
$$

{% <theorem> %}
每个沿着 $\gamma$ 的 Jacobi 场可由某个沿着 $\gamma$ 的变分得到。
{% </theorem> %}

证明略。

### 指数定理
{% <theorem title="Morse 的定理"> %}
$E_{\ast\ast}$ 的[指数](@/posts/morse_theory_1.md)等于与 $\gamma(0)$ 共轭的 $\gamma(t)\\, (0 < t < 1)$ 个数（计重数）。指数总是有限的。
{% </theorem> %}

对每个 $\gamma(t)$，回忆有一个邻域 $U$ 使得其中任两点有一条唯一最短测地线，且随端点变动光滑。故可找充分细的划分 $0 = t_0 < \cdots < t_k = 1$ 使得每个 $\gamma| _{[t _{i-1}, t _i]}$ 均在上述 $U$ 中，从而最短。

我们令 $T_\gamma \Omega(t_0, \dots, t_k) \subseteq T_\gamma \Omega$ 是这样的线性空间：对每个 $W$，在 $[t_{i-1}, t_i]$ 片段 $W$ 是沿着 $\gamma$ 的 Jacobi 场；且在端点处消没。

令 $T'$ 包含的是所有 $W(t_0) = \cdots = W(t_k) = 0$ 的向量场构成的空间。有：

$$T_\gamma \Omega = T_\gamma \Omega(t_0, \dots, t_k) \oplus T'$$

计算知这两者在以 $E_{\ast\ast}$ 为内积下垂直，且 $E_{\ast\ast}$ 限制在 $T'$ 上是正定的。

故而，$E_{\ast\ast}$ 的零化度/指数等于其限制在 $T_\gamma \Omega(t_0, \dots, t_k)$ 上的零化度/指数，从而指数是有限的。

我们令 $\lambda(\tau)$ 是 $(E_0^\tau)_{\ast\ast}$ 的指数，则：
- $\lambda(\tau)$ 是单调不减函数
- 对小的值 $\lambda(\tau) = 0$
- 对充分小的 $\varepsilon > 0$ 有 $\lambda(\tau - \varepsilon) = \lambda(\tau)$
- 设 $\nu$ 是 $(E_0^\tau)_{\ast\ast}$ 的零化度，则对充分小的 $\varepsilon > 0$ 有 $\lambda(\tau + \varepsilon) = \lambda(\tau) + \nu$

### $\Omega^c$ 的有限维逼近
现在为 $\Omega$ 赋予拓扑。对 Riemann 度规诱导的 $M$ 上的度量 $\rho$，设 $\omega, \omega'$ 的弧长 $s(t), s'(t)$ 我们定义：

$$d(\omega, \omega') = \max_{0 \leq t \leq 1} \rho(\omega(t), \omega'(t)) + \sqrt{\int_0^1 (\dot s - \dot s')^2 \mathrm dt}$$

这个度量可以诱导 $\Omega$ 上的拓扑。

我们记 $\Omega^c$ 是 $E^{-1}([0, c])$，则我们可以给 $(\operatorname{Int} \Omega^c) \cap \Omega(t_0, \dots, t_k)$ 自然地赋予有限维光滑流形结构。

这是因为，作充分细的划分，使得每段 $\omega| _{[t _{i-1}, t _i]}$ 被端点唯一决定且随变化光滑。此时就有如下对应：

$$\omega \mapsto (\omega(t_1), \dots, \omega(t_{k-1}))$$

{% <theorem> %}
对完备黎曼流形 $M$ 及 $p, q$ 不沿着任何长度 $\leq \sqrt a$ 测地线共轭，则 $\Omega^a$ 有有限维 CW-复形的同伦型，且若 $E_{\ast\ast}$ 某处指标 $\lambda$，则对应一个 $\lambda$ 维胞腔。
{% </theorem> %}

证明略，然后与[之前结论](@/posts/morse_theory_1.md)结合。

### 道路空间的拓扑
对 Riemann 流形 $(M, g)$，设诱导的度量 $\rho$，取两点 $p, q$. 在同伦论中我们关心的是所有从 $p$ 到 $q$ 的连续曲线构成的空间：

$$\Omega^\ast = \set{\omega: [0, 1] \to M}$$

取紧开拓扑，这可以被描述为由以下度量诱导：

$$d^\ast(\omega, \omega') = \max_t \rho(\omega(t), \omega'(t))$$

另一方面我们前文研究的是 $p$ 到 $q$ 的分段光滑函数构成的空间 $\Omega$，度量：

$$d(\omega, \omega') = d^\ast(\omega, \omega') + \sqrt{\int_0^1 (\dot s - \dot s')^2 \mathrm dt}$$

由于 $d \geq d^\ast$，自然的映射 $i: \Omega \to \Omega^\ast$ 是连续的。

{% <theorem> %}
$i$ 是 $\Omega$ 与 $\Omega^\ast$ 的同伦等价。
{% </theorem> %}

使用一个基本事实，每个点有一个邻域是“测度凸”的（即任意两个点间有一条唯一最短测地线，其完全在该邻域中、随端点变化光滑）。

取这样的邻域的覆盖 $N_\alpha$，将 $[0, 1]$ 等分成 $2^k$ 块，用 $\Omega_k^\ast \subseteq \Omega^\ast$ 表示额外要求每个小块都被某个 $N_\alpha$ 包含的集合。有：

$$\Omega_1^\ast \subseteq \Omega_2^\ast \subseteq \cdots$$

对应的 $\Omega_k = i^{-1}(\Omega_k^\ast)$ 是 $\Omega$ 的开集，且并等于 $\Omega$.

我们可以定义 $h: \Omega_k^\ast \to \Omega_k$，$h(\omega)$ 在每一块端点与 $\omega$ 重合，中间取最短测地线。这可以说明 $i|_{\Omega_k}$ 是同伦等价。

由同伦正向极限性质知整个 $i$ 是同伦等价。

一个事实是 $\Omega$ 有 CW-复形的同伦型，故知 $\Omega^\ast$ 有 CW-复形的同伦型。

{% <theorem title="Morse 理论基本定理"> %}
对完备黎曼流形 $M$ 及 $p, q$ 不沿着任何测地线共轭，$\Omega_p^q(M)$ 有可数 CW-复形的同伦型，对每个指数 $\lambda$ 的 $p$ 到 $q$ 测地线对应一个 $\lambda$ 维胞腔。
{% </theorem> %}

类似[之前的证明](@/posts/morse_theory_1.md)。

{% <example title="球面的道路空间"> %}
考虑球面上的非共轭点 $p, q$（即 $q$ 不与 $p$ 或 $p$ 对径点 $p'$ 重合）。令 $\gamma_0$ 是过 $p, q$ 最短的大圆弧，$\gamma_1$ 是 $pq'p'q$，$\gamma_2$ 是 $pqp'q'pq$，以此类推。
{% </example> %}

由此，$\Omega(\mathbb S^n)$ 对应的 CW-复形的同伦型是维数 $0, n-1, 2(n-1), \dots$ 各恰有一个胞腔。由此可以算出 $n > 2$ 时 $\Omega(\mathbb S^n)$ 的同调。

又，$n > 2$ 时有 $\Omega(\mathbb S^n)$ 的同伦型的 $M$ 中非共轭点间必有无限多条测地线。

### 非共轭点存在性
回忆我们可以从 $f: N \to M$ 诱导一个 $f_\ast: T_x N \to T_{f(x)} M$，当 $\dim M = \dim N$ 时称 $x$ 临界点如果 $f_\ast$ 不是 1-1 的。同样可以对 $\exp_p$ 定义此。

{% <theorem> %}
对从 $p$ 到 $\exp_p(v)$ 的测地线，两端点共轭当且仅当 $\exp_p$ 在 $v$ 处临界。
{% </theorem> %}

证明略，然后可以用 Sard 定理。

### 拓扑与曲率的一些关联
考虑负曲率、正曲率带来的测地线的行为。

典型的完备负曲率空间有：抛物面、双曲面、螺旋面（$x \cos z + y \sin z = 0$）。又如伪球面：

$$z = - \sqrt{1 - x^2 - y^2} + \operatorname{sech}^{-1} \sqrt{x^2 + y^2} \quad (z > 0)$$

{% <theorem> %}
若 $M$ 恒有负截面曲率，则任两点不共轭。
{% </theorem> %}

由 Jacobi 场定义式，

$$\braket{\ddot J, J} = - \braket{R(V, J)V, J} \geq 0$$

从而 $\mathrm d / \mathrm dt \braket{\dot J, J} \geq 0$，故如果在端点处消没，则 $J$ 会整个消没。

{% <theorem title="Cartan 的定理"> %}
若 $M$ 单连通、完备、恒有非正截面曲率，则任两点间有唯一的测地线。更进一步 $M$ 与 $\R^n$ 微分同胚。
{% </theorem> %}

由于没有共轭点，由指数定理所有测地线指数 $0$，故 $\Omega_p^q(M)$ 是 $0$-维 CW-复形。$M$ 单连通意味着它连通，故是单点，测地线唯一。

从而，$\exp$ 是 1-1 的。回忆它局部是微分同胚，它是全局的微分同胚。

其推论是，若 $M$ 完备、恒有非正截面曲率，则大于 $1$ 阶同伦群消没，且基本群没有单位元外的有限阶元素。

---

现在考虑正曲率。

{% <theorem title="Myers 的定理"> %}
设 Ricci 曲率满足存在常数 $r$，在任一点处的单位向量 $U$ 都有：

$$\mathrm{Ric}(U, U) \geq \frac{(n - 1)}{r^2}$$

则每一条长度大于 $\pi r$ 的测地线包含共轭点，从而不是最短的。
{% </theorem> %}

证明略。

{% <theorem> %}
$M$ 完备，Ricci 张量处处正定，则 $\Omega_p^q(M)$ 的同伦型中每个维数只有有限个胞腔。
{% </theorem> %}

证明略。
