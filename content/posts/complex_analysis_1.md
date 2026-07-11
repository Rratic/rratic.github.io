+++
title = "复变与复平面基本概念"
date = 2025-08-28

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "分析学"]
+++

本文为经过重新整理得到，为之后的主要的内容提供定义。双曲几何部分这里极为简略，因为市面上的相关材料已经十分充足了（甚至存在 HyperRogue 这样的双曲游戏），读者也可参考[几何学](@/posts/geometry_1_final.md)及[模形式](@/posts/modular_forms_1.md)的对应部分。

<!-- more -->

## 基本定义
复值函数就是定义域为 $\Complex$ 的子集，陪域为 $\Complex$ 的函数。可以按 $f = u + \mathrm{i}v$ 拆分成实值函数的组合。普通的复值函数相当于是多元的实函数，并不是这里研究的主题。我们之后会看一些性质更好的函数。

$\Complex$ 上的曲线被定义为：

$$\gamma: [\alpha, \beta] \to \Complex$$

若 $\gamma'(t)$ 存在且连续，称该弧是可微的。若再有 $\gamma'(t) \neq 0$，称该弧是正则的（使得折线不被认为是狭义的“曲线”）。当 $f(\alpha) = f(\beta)$ 时称其为闭曲线。

按惯例，我们称**区域**是指 $\Complex$ 上道路连通的开集。[**单连通**](@/posts/geometry_2_final.md)在这里解释为：区域内的任一条 Jordan 曲线（不自交、连续、闭合的曲线）均为某个子区域的边界。[^jordan-curve]

### 全纯与解析
在复数域 $\Complex$ 上，极限的表述可以参照数学分析中的表述：

{% admonition(type="definition", title="极限") %}
称 $\lim_{z \to z_0} f(z) = w$，如果对任意 $\omega > 0$，存在 $\delta > 0$，对任意 $0<|z-z_0|<\delta$ 有 $|f(z)-w|<\omega$.
{% end %}

以类似的方式，我们可以定义导数。

易知，复值函数 $f = u + \mathrm{i}v$ 在 $x+y\mathrm{i}$ 处可导的充要条件是 $\partial u / \partial x$，$\partial u / \partial y$，$\partial v / \partial x$ 与 $\partial v / \partial y$ 在 $z$ 处存在，且满足如下 Cauchy-Riemann 条件：

$$
\begin{cases}
	\partial u / \partial x = \partial v / \partial y \cr
	\partial u / \partial y = -\partial v / \partial x
\end{cases}
$$

定义全微分是指：

$$\mathrm{d}f = \mathrm{d}u + \mathrm{i}\cdot\mathrm{d}v = \frac{\partial f}{\partial x}\mathrm{d}x + \frac{\partial f}{\partial y}\mathrm{d}y$$

现在，我们可以定义：若函数 $f$ 在区域 $D$ 内每一点都是复可导的，则称其为 $D$ 上的**全纯函数**。用定义即知全纯函数是保角的，也就是说 $\gamma_1(t)$ 与 $\gamma_2(t)$ 的夹角等于 $f(\gamma_1(t))$ 与 $f(\gamma_2(t))$ 的夹角。

若 $f$ 在区域 $D$ 内每一点 $z_0$，都可在它的邻域上展开为 $z-z_0$ 的幂级数，则称它在 $D$ 上是**解析**的。如果进一步在 $D$ 上是单的，则称它是**单叶解析**的。此时如果还是满的且逆 $f^{-1}$ 也是解析的，则称为**解析同胚/共形映射**。

我们在[之后](@/posts/complex_analysis_2.md)会说明，全纯函数的导数仍然是全纯的，并且全纯函数和解析函数是等价的条件。

### 初等函数
现在我们希望定义复数版的 $e^z$，有三种定义方式：
1. 使用实函数：$e^{x + \mathrm{i}y} = e^x(\cos y + \mathrm{i}\sin y)$
2. 令 $e^z = \lim_{n\to\infty} \sum_{k=0}^n z^k / k!$，收敛性易证
3. 定义为满足 $f'(z)=f(z)$ 及 $f(0)=1$ 的微分方程的解，解存在且唯一

易知它们是等价的，其中 (2) 与 (3) 均可以推出 $e^{z+w}=e^z\cdot e^w$，分别是因为：

$$\left(\sum_{k=0}^\infty \frac{z^k}{k!}\right)\left(\sum_{k=0}^\infty \frac{w^k}{k!}\right) = \sum_{k=0}^\infty \frac{1}{k!} \sum_{i+j=k}\frac{k!}{i!j!}z^iw^j = \sum_{k=0}^\infty \frac{1}{k!} (z+w)^k$$

$$(e^z\cdot e^{a-z})' = e^z\cdot e^{a-z}+e^z\cdot (-e^{a-z}) = 0$$

从而可以定义 $\cos z = (e^{\mathrm{i}z} + e^{-\mathrm{i}z}) / 2$ 与 $\sin z = (e^{\mathrm{i}z} - e^{-\mathrm{i}z}) / 2\mathrm{i}$ 为对应的复值函数。

### 调和函数
我们称满足 $\Delta u = 0$ 的函数 $u$ 为**调和函数**，其中 $\Delta$ 是 Laplace 算子：

$$\Delta = \frac{\partial^2}{\partial x^2} + \frac{\partial^2}{\partial y^2}$$

如果 $f$ 在区域 $D$ 上是解析的，则（我们会在之后说明）它属于 $C^2(D)$，并有：

$$
\begin{cases}
	\Delta u = \partial^2 u / \partial x^2 + \partial^2 u / \partial y^2 = 0 \cr
	\Delta v = \partial^2 v / \partial x^2 + \partial^2 v / \partial y^2 = 0
\end{cases}
$$

我们称 $u, v$ 是共轭调和的，如果它们是调和的，并且：

$$
\begin{cases}
	\partial u / \partial x = \partial v / \partial y \cr
	\partial u / \partial y = -\partial v / \partial x
\end{cases}
$$

{% admonition(type="theorem", title="共轭调和函数") %}
对单连通区域 $\Omega$ 及调和函数 $u$，存在函数 $v$ 为它的共轭调和，且在差一个常数下唯一。
{% end %}

我们定义 $\omega = (\partial u / \partial x) \mathrm{d}y - (\partial u / \partial y) \mathrm{d}x$，就有：

$$\mathrm{d}\omega = \left(\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2}\right)\mathrm{d}x\wedge \mathrm{d}y = 0$$

从而对区域 $D \subseteq \Omega$，有：

$$\int_{\partial D} \omega = \int_{D}\mathrm{d}\omega = 0$$

在差一个常数下，只需取下式即可：

$$v(x, y) = \int_{(x_0, y_0)}^{(x, y)} \omega$$

{% admonition(type="theorem", title="存在 $\ln f$") %}
$f$ 为区域 $D$ 上的处处非零全纯函数，则存在 $g$ 使得 $e^g = f$.
{% end %}

令 $u = \ln |f|$，然后取 $v$ 为它的共轭调和，平移调整即可。

{% admonition(type="theorem", title="存在 $\sqrt[n]{m}$") %}
$f$ 为区域 $D$ 上的处处非零全纯函数，则存在 $g$ 使得 $g^n = f$.
{% end %}

取 $\exp(\ln(f) / n)$ 即可。

### 幂级数
{% admonition(type="theorem", title="Lucas 定理") %}
对多项式 $P$，$P'$ 的零点在 $P$ 的零点组成的凸包内。
{% end %}

这依赖于代数基本定理。我们设 $P(z) = k\prod_{i=1}^n (z-z_i)$，则有：

$$\frac{P'(z)}{P(z)} = \sum_{i=1}^n \frac{1}{z - z_i}$$

设 $w$ 是 $P'$ 的零点而不是 $P$ 的零点，代入上式，如果 $P$ 的零点均在半平面 $H: \operatorname{Im} (z - a) / b < 0$ 内，则 $\operatorname{Im} bP'(w) / P(w) = \sum_{i=1}^n \operatorname{Im} \frac{b}{w - z_i} = 0$，存在 $\operatorname{Im} (w - z_i) / b < 0$，从而 $w$ 在 $H$ 内。

{% admonition(type="theorem", title="幂级数的收敛圆") %}
对幂级数 $a_0 + a_1z + a_2z^2 + \cdots$，存在 $r \in [0, +\infty]$ 满足：
1. 对 $|z| < r$，级数收敛
2. 对 $|z| > r$，级数发散
3. $|z| < r$ 内级数的和是解析函数，导数可由逐项微分得到，对应的收敛半径相同
{% end %}

参考[数学分析](@/posts/analysis_2_midterm.md)的相关结论，这里有：

$$\frac 1 r = \lim_{n \to \infty} \sup \sqrt[n]{|a_n|}$$

### 多值函数
当我们不严格地说 $F: D \to \Complex$ 可能有多值，是一个**多值函数**时，实际上可以将值看作复数构成的集合。

例如，因为复数上的 $\exp$ 不是单的，它的反函数形如：

$$\mathrm{Ln}(r\cdot e^{\mathrm{i}\theta}) = \mathrm{Ln}(r) + \mathrm{i}(\theta + 2k\pi),\quad k \in \Z$$

一般来说，我们希望找到多值函数的一个单值分支 $f$，即在 $D$ 上有 $f(z) \in F(z)$ 且 $f$ 是解析的。例如，在 $\Complex$ 去掉一个原点引出的射线这一区域上，$\mathrm{Ln}$ 可以定义单值分支。

另一种方式是不局限于 $\Complex$，定义它的 Riemann 曲面（将是 $1$ 维的连通复流形），使函数在其上是单值的。[^riemann-surface]

## 复平面
### 扩充复平面
扩充复平面是指 $\bar{\Complex} = \Complex \cup \set{\infty}$，并不妨定义以无穷为圆心的邻域：

$$D(\infty, \varepsilon) = \set{z\in\bar{\Complex} | \lVert z \rVert > \frac 1 \varepsilon}$$

通过球极投影可以建立它与二维球面 $\mathbb{S}^2$ 的一一映射，显然它们的拓扑是一样。从而，可以在球面中看到扩充复平面的极限定义，并看到扩充复平面是 $\Complex$ 的一个紧致化。

同时，可以将它看作复射影直线 $\Complex\mathrm{P}^1$，其中 $z = z_1 / z_2$ 可写作 $(z_1 : z_2)$，这体现了它的[射影性质](@/posts/geometry_1_final.md)。

### 分式线性变换
分式线性变换是重要的解析函数例子，指代：

$$z\mapsto \frac{az + b}{cz + d}$$

又称 Möbius 变换。有时我们也会考虑如下共轭分式线性变换，它们均可将扩充复平面映到自身：

$$z\mapsto \frac{a\bar{z} + b}{c\bar{z} + d}$$

分式线性变换以 $\Complex\mathrm{P}^1$ 视角看作射影变换（可由平移、位似、伊朗式反演复合得到）：

$$\begin{pmatrix} z_1 \cr z_2 \end{pmatrix} \mapsto \begin{pmatrix} a & b \cr c & d \end{pmatrix} \begin{pmatrix} z_1 \cr z_2 \end{pmatrix}$$

{% admonition(type="theorem", title="由三点的像确定") %}
对不同的三点 $z_i$ 及不同的三点 $w_i$，存在唯一的分式线性变换满足 $f(z_i) = w_i$.
{% end %}

因为可以将 $1, 0, \infty$ 映到任意指定的不同 $z_1, z_2, z_3$，且分式线性变换可逆。

对扩充平面上三个不同的点 $z_2, z_3, z_4$，将它们变成 $1, 0, \infty$ 的变换将 $z_1$ 映到其交比 $(z_1, z_2; z_3, z_4)$，它是射影不变量。

---

[^jordan-curve]: 等价性来自于 Jordan 曲线定理。
[^riemann-surface]: 这实际上是一个（分歧）[复叠](@/posts/geometry_2_final.md)。
