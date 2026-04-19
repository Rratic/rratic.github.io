+++
title = "几何学Ⅱ期中复习笔记"
date = 2026-04-18

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "几何"]
+++

本文用于准备几何学Ⅱ的期中考试。这半学期的主要内容是古典微分几何，嵌入 $\mathbb{E}^3$ 看的曲线论与曲面论。

<!-- more -->

## 曲线论
空间曲线被定义为光滑映射 $\gamma: J \to \mathbb{E}^3$. 如果懒得讨论端点，可以取 $J$ 为一个开区间。

我们定义长度：

$$\mathrm{Length}_\gamma([a, b]) = \int_a^b \lVert \gamma'(t) \rVert \mathrm{d}t$$

弧长参数是指参数恒成立：

$$\lVert \gamma'(t) \rVert = 1$$

### Frenet 标架
在弧长参数下，由 $(\gamma'(s) \cdot \gamma'(s))' = 0$ 知 $\gamma'(s)$ 与 $\gamma''(s)$ 垂直。我们定义**切向** $\mathbf{t}(s) = \gamma'(s)$, **主法向** $\mathbf{n}(s) = \frac{\gamma''(s)}{\lVert \gamma''(s) \rVert}$, 次法向 $\mathbf{b}(s) = \mathbf{t}(s) \times \mathbf{n}(s)$.

称 $(\gamma(s); \mathbf{t}(s), \mathbf{n}(s), \mathbf{b}(s))$ 是 $\gamma$ 的 Frenet 标架场，并称 $\mathbf{t}(s)$ 和 $\mathbf{n}(s)$ 张成**密切平面**，称 $\mathbf{b}(s)$ 和 $\mathbf{t}(s)$ 张成从切平面，称 $\mathbf{n}(s)$ 和 $\mathbf{b}(s)$ 张成法平面。

定义衡量弯曲程度的**曲率**为弧长参数下 $\kappa = \lVert \gamma''(s) \rVert$, 在一般正则参数下即是：

$$\frac{\lVert \gamma''(s) \times \gamma'(s) \rVert}{\lVert \gamma'(s) \rVert^3}$$

衡量偏出密切平面趋势的**挠率**为（注意实际上 $\dot{\mathbf{b}}$ 与 $\mathbf{n}$ 是共线的）：

$$\tau = -\mathbf{b}'(s) \cdot \mathbf{n}(s)$$

这里的负号是采取的人为约定；反参数定向不会改变挠率的符号。

它在一般正则参数下是：

$$\frac{(\dot\gamma(s) \times \ddot\gamma(s)) \cdot \dddot\gamma(s))}{\lVert \dot\gamma(s) \times \ddot\gamma(s) \rVert^2}$$

一个有用的结论是：

$$
\frac{\mathrm{d}}{\mathrm{d}s} (\mathbf{t}, \mathbf{n}, \mathbf{b}) = (\mathbf{t}, \mathbf{n}, \mathbf{b})
\begin{pmatrix}
	0 & -\kappa & 0 \cr
	\kappa & 0 & -\tau \cr
	0 & \tau & 0
\end{pmatrix}
$$

此矩阵的反对称性和对角元素为 $0$ 可由 Frenet 标架右手单位正交推得。

### 曲线论基本定理
{% admonition(type="theorem", title="曲线论基本定理") %}
假设 $\kappa, \tau$ 是开区间 $J$ 上的光滑函数，且 $\kappa$ 恒正，那么：
1. 存在弧长参数曲线段 $\gamma: J \to \mathbb{E}^3$，使得曲率、挠率与 $\kappa, \tau$ 相对应
2. 这样的曲线段在差一个刚体运动下唯一
{% end %}

这是通过 ODE 证明的。

## 曲面论
局部正则参数曲面片记为：

$$\varphi: U \to \mathbb{E}^3$$

其中 $U \subseteq \R$ 中的点记作 $u = (s, t)$. 我们用下标 $s, t$ 分别表示对 $s, t$ 求偏导。

求面积即是：

$$\iint_R \lVert \phi_s(u) \times \phi_t(u) \rVert \mathrm{d}s\mathrm{d}t$$

### 基本形式
我们定义 $E(u) = \phi_s(u) \cdot \phi_s(u), F(u) = \phi_s(u) \cdot \phi_t(u), G(u) = \phi_t(u) \cdot \phi_t(u)$. 记**第一基本形式**：

$$g = E\mathrm{d}s^2 + 2F\mathrm{d}s\mathrm{d}t + G\mathrm{d}t^2$$

这是一个内禀量，代表曲面作为二维黎曼流形的全部度量信息。

正交参数是指 $F = 0$, 等温参数是指正交且 $E = G$.

---

我们记单位法向量：

$$\mathbf{n} = \frac{\phi_s \times \phi_t}{\lVert \phi_s \times \phi_t \rVert}$$

定义：

$$
\begin{cases}
	L = \phi _{ss} \cdot \mathbf{n} = -\phi_s \cdot \mathbf{n} _s \cr
	M = \phi _{st} \cdot \mathbf{n} = -\phi_s \cdot \mathbf{n} _t = -\phi _t \cdot \mathbf{n} _s \cr
	N = \phi _{tt} \cdot \mathbf{n} = -\phi_t \cdot \mathbf{n} _t
\end{cases}
$$

及**第二基本形式**：

$$h = L\mathrm{d}s^2 + 2M\mathrm{d}s\mathrm{d}t + N\mathrm{d}t^2$$

### 曲率
我们令**平均曲率**：

$$H = \frac{1}{2} \cdot \frac{LG-2MF+NE}{EG-F^2}$$

令 **Gauss 曲率**：

$$K = \frac{LN-M^2}{EG-F^2}$$

称 $\lambda^2 - 2H\lambda + K = 0$ 的两根 $\kappa_1, \kappa_2$ 为主曲率。

{% admonition(type="tip", title="几何直观") %}
考察曲面上的曲线 $\gamma(r) = \phi(u(r))$ 是弧长参数的。由于 $\ddot\gamma \bot \dot\gamma$ 存在分解：

$$\ddot\gamma = \kappa_n\mathbf{n} + \kappa_g\mathbf{n} \times \dot\gamma$$

其中 $\kappa_n$ 称为**法曲率**，$\kappa_g$ 称为**测地曲率**。我们算得：

$$\ddot\gamma = (\dot s^2 \phi_{ss} + 2\dot s \dot t \phi_{st} + \dot t^2 \phi_{tt}) + (\ddot s \phi_s + \ddot t \phi_t)$$

对两式均进行两边同乘 $\mathbf{n}$, 即得 $\kappa_n = L\dot s^2 + 2M\dot s \dot t + N\dot t^2$.

{% admonition(type="note", title="理解") %}
我们可以选取适当参数，使 $\phi$ 在该处的第一基本形式为 $\mathrm{d}s^2 + \mathrm{d}t^2$. 此时可推出 $\kappa_1, \kappa_2$ 是 $\kappa_n$ 的两个极值。对应线向就是主方向。
{% end %}

{% end %}

考察 Gauss 映射：

$$
\begin{aligned}
\mathcal{G} \colon U & \to \mathbb{S}^2 \cr
    u & \mapsto \mathbf{n}(u)
\end{aligned}
$$

我们定义 Weingarten 映射 $W$ 从 $\mathbf{n}(u)^\bot$ 映到自身（更准确地说从 $T_uU$ 映到自身）：对 $\mathbf{v} \in \mathbf{n}(u)^\bot$ 任取弧长参数曲线 $\gamma(r) = \phi(u(r))$ 使得 $\dot\gamma(0) = \mathbf{v}$. 此时令：

$$W(v) = -\frac{\mathrm{d}(\mathcal{G} \circ \gamma)}{\mathrm{d}r}\Big|_{r=0}$$

取基 $\phi_s, \phi_t$, 有 $W(\phi_s) = -\mathbf{n}_s, W(\phi_t) = -\mathbf{n}_t$. 在基上的矩阵为：

$$\begin{pmatrix} E & F \cr F & G \end{pmatrix}^{-1} \begin{pmatrix} L & M \cr M & N \end{pmatrix}$$

可见 $H = \frac{1}{2} \operatorname{tr} W, K = \det W$, 两个主曲率是其特征值，对应的特征线向为**主方向**。两个主方向是正交的。

称 $\kappa_1 = \kappa_2$ 的点为脐点，切向量处处为主方向的曲线为**曲率线**；法曲率处处为零的曲线称为渐近曲线；测地曲率处处为零的曲线称为测地线。

### 曲面论基本方程
我们考虑：

$$\begin{pmatrix} \phi_s \cr \phi_t \cr \mathbf{n} \end{pmatrix} \cdot \begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix} _{st} = \begin{pmatrix} \phi_s \cr \phi_t \cr \mathbf{n} \end{pmatrix} \cdot \begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix} _{ts}$$

得到的矩阵首行中间列就是 Gauss 方程，前两行最右列就是 Codazzi 方程。

{% admonition(type="theorem", title="Gauss 绝妙定理") %}
空间曲面的第一基本形式完全决定 Gauss 曲率。
{% end %}

即证 $LN - M^2$ 可以由 $E, F, G$ 完全表示。

计算非常琐碎，我们可以定义：

$$
P = \begin{pmatrix} \phi_s \cr \phi_t \cr \mathbf{n} \end{pmatrix} \cdot \begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix}
= \begin{pmatrix} E & F & 0 \cr F & G & 0 \cr 0 & 0 & 1 \end{pmatrix}
$$

$$\begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix}_s = \begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix} A$$

$$\begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix}_t = \begin{pmatrix} \phi_s & \phi_t & \mathbf{n} \end{pmatrix} B$$

特别地，当 $F = 0$ 时可以使用 $(\phi_s, \phi_t, \mathbf{n})$ 构成正交标架来简化计算。

### 曲面论基本定理
{% admonition(type="theorem", title="曲面论基本定理") %}
若 $D \subset \R^2$ 上 $g, h$ 相容（即满足 Gauss-Codazzi 方程），则实现的曲面片局部存在，且在刚体运动下唯一。
{% end %}

使用一些 PDE 之类的东西。

整体解不一定存在，是因为转一圈后可能出问题。

### 保长对应
设 $\phi: U \to \mathbb{E}^3, \tilde{\phi}: \tilde{U} \to \mathbb{E}^3$ 是局部正则的参数曲面片。设 $\tau: \tilde{U} \to U$ 是光滑同胚。令 Jacobi 矩阵：

$$
J = \begin{pmatrix}
	\frac{\partial s}{\partial \tilde{s}} & \frac{\partial s}{\partial \tilde{t}} \cr
	\frac{\partial t}{\partial \tilde{s}} & \frac{\partial t}{\partial \tilde{t}}
\end{pmatrix}
$$

通过看 $\tilde{U} \to \mathbb{E}^3$ 及使用多元微积分，得：

{% admonition(type="theorem", title="保长对应") %}
$\tau$ 保长当且仅当：

$$
J^\top \begin{pmatrix}
	E \circ \tau & F \circ \tau \cr
	F \circ \tau & G \circ \tau
\end{pmatrix} J = \begin{pmatrix}
	\tilde{E} & \tilde{F} \cr
	\tilde{F} & \tilde{G}
\end{pmatrix}
$$
{% end %}

{% admonition(type="theorem", title="保角对应") %}
$\tau$ 保角当且仅当存在恒正 $\rho: \tilde{U} \to \R$ 使得：

$$
J^\top \begin{pmatrix}
	E \circ \tau & F \circ \tau \cr
	F \circ \tau & G \circ \tau
\end{pmatrix} J = \rho \cdot \begin{pmatrix}
	\tilde{E} & \tilde{F} \cr
	\tilde{F} & \tilde{G}
\end{pmatrix}
$$
{% end %}

{% admonition(type="theorem", title="保积对应") %}
$\tau$ 保积当且仅当：

$$\sqrt{(EG-F^2) \circ \tau} \cdot |\det J| = \sqrt{\tilde{E}\tilde{G}-\tilde{F}^2}$$
{% end %}

## 往年题
{% admonition(type="question", title="2022 P3") %}
给定空间曲面 $S_1: x^2+y^2+z^2 = 9$ 和 $S^2: x^2−2y = 0$，记 $c = S_1 \cap S_2$ 为它们的交线。求：点 $P(2, 2, 1)$ 处 $c$ 关于 $S_1$ 的法曲率。
{% end %}

设 $c$ 的弧长参数方程 $\gamma(s)$ 满足 $\gamma(0) = (2, 2, 1), x'(0) > 0$. 通过对 $S_1$ 求两次导及弧长参数知：

$$2x''(0) + 2y''(0) + z''(0) = -1$$

又法向量 $(\frac{2}{3}, \frac{2}{3}, \frac{1}{3})$ 知法曲率 $\gamma''(0) \cdot \mathbf{n} = -\frac{1}{3}$.

{% admonition(type="question", title="2022 P4") %}
求证：如果空间正则曲面 $S \subset \mathbb{E}^3$ 包含一条直线 $l \subset S$，那么 $S$ 在 $l$ 上任意点的 Gauss 曲率都小于或等于 $0$.
{% end %}

因为主曲率将满足 $\kappa_1 \leq 0 \leq \kappa_2$.

{% admonition(type="question", title="2022 P5") %}
假设 $\Omega$ 是 $\mathbb{E}^3$ 中的区域，包含于单位球内部，且边界 $S = \partial \Omega$ 是正则曲面。求证：存在 $S$ 上某点，Gauss 曲率大于或等于 $1$.
{% end %}

注意到单位球上 Gauss 曲率为 $1$.

我们取与原点距离最远的点 $p$, 设主方向对应法截线 $\gamma(s)$ 满足 $\gamma(0) = p$.

$$\gamma(s) = \gamma(0) + s\gamma'(0) + \frac{1}{2}s^2\gamma''(0) + o(s^2)$$

由极大性知 $\gamma(0) \bot \gamma'(0)$, 由弧长参数的特性 $\gamma'(0) \bot \gamma''(0)$, 进而：

$$\lVert \gamma(s) \rVert^2 = \lVert \gamma(0) \rVert^2 + s^2 + s^2 \gamma(0) \cdot \gamma''(0) + o(s^2)$$

从而：

$$1 + \gamma(0) \cdot \gamma''(0) + o(1) \leq 0$$

从而：

$$\lVert \gamma''(0) \rVert \geq 1$$

{% admonition(type="question", title="2023 P6") %}
设 $\phi: \R^2 \to \mathbb{E}^3$ 是正则（浸入）参数曲面片，且假设 $\phi(s, t) = \phi(s+1, t) = \phi(s, t+1)$ 对所有 $(s, t) \in \R^2$ 成立。问：$\phi$ 的 Gauss 映射是否一定满射单位球面？加以论证。
{% end %}

否，例子大约是：

$$\mid 8$$

这里“8”绕着轴转一圈，通过定向说明法向量只能在正向上、正向下中取其一。
