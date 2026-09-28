+++
title = "【草稿】黎曼几何（一）：联络与测地线的变分"
date = 2026-09-28

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

由于讨论班需要，进行一些补习。参考《黎曼几何初步》及 Milnor *Morse Theory* 第二、三部分。

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

{{ <todo /> }}

我们令 $v \in T_qM$，假定存在测地线 $\gamma: [0, 1] \to M$ 使得 $\gamma(0) = q, \dot \gamma(0) = v$，我们记 $\gamma(1) = \exp_q(v)$，称为**指数映射**。此时有 $\gamma(t) = \exp_q(tv)$.

{% <definition title="测地完备"> %}
流形 $M$ 称为测地完备的，如果对所有的 $q$ 和 $v \in T_qM$ 都有 $\exp_q(v)$ 有定义。这等价于所有测地线段都可延拓成无限长的。
{% </definition> %}

{% <theorem title="Hopf and Rinow"> %}
若 $M$ 测地完备，则任意两点可以被一最小测地线连接。
{% </theorem> %}

其推论是有界集有紧的闭包，从而作为一个度量空间完备。

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

{{ <todo /> }}

临界

### 能量
对 $\omega \in \Omega$ 我们定义其**能量**：

$$E_a^b(\omega) = \int_a^b \lVert\dot \omega\rVert^2 \mathrm dt$$

由 Cauchy-Schwarz 易见：

$$(L_a^b)^2 \leq (b-a) E_a^b$$

这意味着，对完备黎曼流形及 $p, q \in M$，能量函数只在最短测地线集合上取到最小值。

为了方便陈述接下来的定理，我们令 $W_t = \frac{\partial \alpha}{\partial u}(0, t)$；令 $V_t = \dot \omega$ 是速度，$A_t = \ddot \omega = \nabla_{\dot \omega} \dot \omega$ 是加速度，$\Delta_t V = V_{t+} - V_{t-}$ 是在坏点处的跳跃，则：

{% <theorem title="第一变分公式"> %}
$$\frac 1 2 \left.\frac{\mathrm dE(\bar \alpha(u))}{\mathrm du}\right|_{u=0} = - \sum_t \braket{W_t, \Delta_t V} - \int_0^1 \braket{W_t, A_t} \mathrm dt$$
{% </theorem> %}

计算即可。

其推论是，$\omega$ 是测地线当且仅当它是 $E$ 的临界点。左推右容易。考虑一个变分 $W_t = f(t)A(t)$ 使得 $f$ 在坏点消没，在其它点正。则 $f(t) \braket{A_t, A_t}$ 积分为零，故 $\omega$ 是未坏的测地线。

### 能量的 Hessian

### Jacobi 场

### 指标定理
