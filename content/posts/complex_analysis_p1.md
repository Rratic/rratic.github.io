+++
title = "复分析速通指南（一）：基本概念"
description = "复变函数基本概念的速通，从复值函数写到 Cauchy 公式及其推论。"
date = 2025-07-27
updated = 2025-11-16

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析", "复分析"]
+++

> 复分析会告诉你：性质好的函数性质有多好。

参考阅读
- 《复变函数简明教程》
- Lars V. Ahlfors *Complex Analysis*

前置知识
- 数学分析
- [点集拓扑](/_misc/topology/)

## 基本定义
复值函数是定义域为 $\mathbb{C}$ 的子集，陪域为 $\mathbb{C}$ 的函数，例如 $x+y\mathrm{i}\mapsto x^2+(x+y)\mathrm{i}$

一般可以写作 $f(z) = u(z) + \mathrm{i}v(z)$，其中 $u, v$ 都是实值的。

这类函数相当于是多元的实函数，并不是这里研究的主题。我们之后会看一些性质更好的函数。

### 定义说明
我们用连续映射 $z=f(t), t\in [\alpha, \beta]$ 表示曲线。若 $z'(t)$ 存在且连续，称该弧是可微的。若再有 $z'(t)\neq 0$，称该弧是正则的（使得折线不被认为是狭义的“曲线”）。若 $f(\alpha)=f(\beta)$ 则称其为闭曲线。

在复分析中，我们称**区域**是指 $\mathbb{C}$ 上道路连通的开集。

有时我们会提到**单连通**，这是指：区域的内的任一条 Jordan 曲线（不自交、连续、闭合的曲线）均为某个子区域的边界。

之后的许多函数性质都是在一个区域上说的。

### 微分
在复数域 $\mathbb{C}$ 上，极限的表述可以参照数学分析中的表述。

{% admonition(type="abstract", title="极限") %}
称 $\lim_{z \to z_0} f(z) = w$，如果对任意 $\omega > 0$，存在 $\delta > 0$，对任意 $0<|z-z_0|<\delta$ 有 $|f(z)-w|<\omega$.
{% end %}

以类似的方式，我们可以定义导数。

易知，复值函数 $f = u + \mathrm{i}v$ 在 $x+y\mathrm{i}$ 处可导的充要条件是：$\frac{\partial u}{\partial x}, \frac{\partial u}{\partial y}, \frac{\partial v}{\partial x}, \frac{\partial v}{\partial y}$ 在 $z$ 处存在，且

$$
\left\\{\begin{matrix}
\frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}\\\\
\frac{\partial u}{\partial y} = -\frac{\partial v}{\partial x}
\end{matrix}\right.
$$

后者称为 Cauchy-Riemann 条件。

定义全微分是指：

$$\mathrm{d}f = \mathrm{d}u + \mathrm{i}\cdot\mathrm{d}v = \frac{\partial f}{\partial x}\mathrm{d}x + \frac{\partial f}{\partial y}\mathrm{d}y$$

### 性质良好函数
若函数 $f$ 在区域 $D$ 内每一点都是复可导的，则称其为 $D$ 上的**全纯函数**。

若 $f$ 在区域 $D$ 内每一点 $z_0$，都可在它的邻域上展开为 $z-z_0$ 的幂级数，则称它在 $D$ 上是**解析**的。

我们在之后将说明，全纯函数的导数仍然是全纯的，并且全纯函数和解析函数是等价的条件。

区域 $\Omega$ 上的解析函数 $f$ 称为**单叶解析**的，如果它在 $\Omega$ 上是单的。

区域到区域的映射 $f: \Omega_1\to\Omega_2$ 称为**解析同胚/共形映射**，如果 $f$ 是单叶解析的，并且 $f^{-1}$ 也是解析的。

### 函数定义
现在我们希望定义复数版的 $\exp: z\mapsto e^z$，有三种定义方式：
1. 定义 $e^{x+\mathrm{i}y} = e^x(\cos y + \mathrm{i}\sin y)$，其中使用的都是实函数
2. 定义 $z_n = \sum_{k=0}^n \frac{z^k}{k!}, e^z = \lim_{n\to\infty} z_n$，由于 $z_n$ 是 Cauchy 列，它是收敛的
3. 定义它是满足 $f'(z)=f(z)$ 及 $f(0)=1$ 的微分方程的解，我们知道解存在且唯一

易知它们是等价的，其中 (3) 可以推出 $e^{z+w}=e^z\cdot e^w$，因为有 $(e^z\cdot e^{a-z})' = e^z\cdot e^{a-z}+e^z\cdot (-e^{a-z}) = 0$

我们可以进一步定义 $\cos z = \frac{e^{iz}+e^{-iz}}{2}, \sin z = \frac{e^{iz}-e^{-iz}}{2i}$

### 调和函数
我们称满足 $\Delta u = 0$ 的函数 $u$ 为**调和函数**。

其中 $\Delta$ 是 Laplace 算子（算子特指函数空间到函数空间的映射）

$$\Delta = \frac{\partial^2}{\partial x^2} + \frac{\partial^2}{\partial y^2}$$

如果 $f$ 在区域 $\Omega$ 上是解析的，则（我们会在之后说明）它属于 $C^2(\Omega)$，并有

$$
\left\\{\begin{matrix}
\Delta u = \frac{\partial^2 u}{\partial x^2}+\frac{\partial^2 u}{\partial y^2} = 0\\\\
\Delta v = \frac{\partial^2 v}{\partial x^2}+\frac{\partial^2 v}{\partial y^2} = 0
\end{matrix}\right.
$$

我们称 $u, v$ 是共轭调和的，如果它们是调和的，并且

$$
\left\\{\begin{matrix}
\frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}\\\\
\frac{\partial u}{\partial y} = -\frac{\partial v}{\partial x}
\end{matrix}\right.
$$

{% admonition(type="abstract", title="共轭调和函数") %}
单连通区域 $\Omega$，对任意调和函数 $u$，存在函数 $v$ 为它的共轭调和，且 $v$ 在差一个常数下是唯一的。
{% end %}

定义 $\omega = \frac{\partial u}{\partial x}\mathrm{d}y - \frac{\partial u}{\partial y}\mathrm{d}x$

有 $\mathrm{d}\omega = \left(\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2}\right)\mathrm{d}x\wedge \mathrm{d}y = 0$（我们会在之后给出路径积分的定义）

对区域 $D\subset \Omega$，有 $\int_{\partial D} \omega = \int_{D}\rm{d}\omega = 0$

取 $v(x, y) = \int_{(x_0, y_0)}^{(x, y)}\omega$ 即可（差一个常数下）。

---

这个结论有一些有趣的推论，如：

{% admonition(type="abstract", title="存在 $\ln f$") %}
$f$ 为区域 $\Omega$ 上的处处非 0 全纯函数，则存在 $e^g = f$
{% end %}

令 $u = \ln |f|$，然后取 $v$ 为它的共轭调和。

{% admonition(type="abstract", title="存在 $\sqrt[n]{m}$") %}
$f$ 为区域 $D$ 上的处处非 0 全纯函数，则存在 $g^n = f$
{% end %}

取 $\exp(\frac{1}{n}\ln(f))$ 即可。

{% admonition(type="abstract", title="保角性") %}
全纯函数是保角的，也就是说 $z_1(t)$ 与 $z_2(t)$ 的夹角等于 $f(z_1(t))$ 与 $f(z_2(t))$ 的夹角。
{% end %}

### 幂级数
{% admonition(type="abstract", title="Lucas 定理") %}
若多项式 $P$ 的零点均在某个半平面 $H$ 内，则 $P'$ 的零点也在 $H$ 内。从而，$P'$ 的零点在 $P$ 的零点组成的凸包内。
{% end %}

我们在之后证明代数基本定理。

设 $P(z) = k\prod_{i=1}^n (z-z_i)$，则 $\frac{P'(z)}{P(z)} = \sum_{i=1}^n \frac{1}{z-z_i}$

记半平面 $H\colon \operatorname{Im} \frac{z-a}{b}<0$

分析 $\operatorname{Im} \frac{bP'(z)}{P(z)} = \sum_{i=1}^n \operatorname{Im} \frac{b}{z-z_i}$ 即可。

{% admonition(type="abstract", title="幂级数的收敛圆") %}
对幂级数 $a_0+a_1z+a_2z^2+\cdots$，存在扩展实数 $0\leq r\leq \infty$ 满足
1. 对 $|z|<r$ 级数收敛
2. 对 $|z|>r$ 级数发散
3. 在 $|z|<r$ 内级数的和是解析函数，导数可由逐项微分得到，对应的收敛半径相同。
{% end %}

取 $\frac{1}{r} = \lim_{n \to \infty}\sup\sqrt[n]{|a_n|}$.

### 多值函数
多值函数 $F: \Omega \to \mathbb{C}$ 中的值 $F(z)$ 可以看作复数构成的集合。

例如，因为复数上的 $\exp$ 不是单的，它的反函数形如 $\mathrm{Ln} (r\cdot e^{\mathrm{i}\theta}) = \mathrm{Ln} r + \mathrm{i}\theta$

一般来说，我们希望找到它的一个单值分支 $f$，即在 $\Omega$ 上有 $f(z)\in F(z)$ 且它是解析的。

例如，在 $\mathbb{C}$ 去掉一个原点引出的射线这一区域上，$\mathrm{Ln}$ 是可以定义单值分支的。

对特定的函数，我们可以定义它的 Riemann 曲面（是 $1$ 维的连通复流形），使函数在其上是单值的。

## 复平面
### 扩充复平面
扩充复平面是指 $\bar{\mathbb{C}} = \mathbb{C}\cup \\{\infty\\}$

可以通过球极投影建立它与二维球面 $\mathbb{S}^2$ 的一一映射，使它们的拓扑是一样。

从而，可以在球面中看到扩充复平面的极限定义。

扩充复平面是 $\mathbb{C}$ 的一个紧致化。

### 分式线性变换
分式线性变换是重要的解析函数例子，指

$$z\mapsto \frac{az+b}{cz+d}$$

又称 Möbius 变换。有时我们也会考虑共轭分式线性变换 $z\mapsto \frac{a\bar{z}+b}{c\bar{z}+d}$，它们均可将扩充复平面映到自身。

若将 $z$ 视作 $\frac{z_1}{z_2}$（或视作齐次坐标），并写为 $z = \begin{pmatrix}z_1\\\\ z_2\end{pmatrix}$ 则前者为：

$$
z\mapsto \begin{pmatrix}a&b\\\\ c&d\end{pmatrix} z
$$

可由典型变换平移、位似、伊朗式反演复合得到。

{% admonition(type="abstract", title="唯一确定") %}
对不同的三点 $z_i$ 及不同的三点 $w_i$，恰存在一个分式线性变换使 $f(z_i) = w_i$
{% end %}

因为可以做到将 $1, 0, \infty$ 映到任意不同的 $z_1,z_2,z_3$

对扩充平面上三个不同的点 $z_2, z_3, z_4$，将它们变成 $1, 0, \infty$ 的变换将 $z_1$ 映到**交比** $(z_1, z_2, z_3, z_4)$

交比满足：线性变换 $S$ 使得 $(S(z_1), S(z_2), S(z_3), S(z_4)) = (z_1, z_2, z_3, z_4)$

---

我们可以得到
* 所有单位圆盘到自身的分式线性变换是
	$$e^{i\theta}\frac{z-a}{1-\bar{a}z}$$
* 所有上半平面到单位圆盘的分式线性变换是
	$$e^{i\theta}\frac{z-a}{z-\bar{a}}$$
* 所有上半平面到自身的分式线性变换可取 $\mathrm{SL}(2, \mathbb{R})$

## 复积分
### 路径积分
在一个实区间上的定积分可以直接由实积分推广得到。

对给定的连续曲线 $\gamma$，作 Riemann 和，极限可取到时记为路径积分 $\int_{\gamma} f(z) \mathrm{d}z$

或者，设 $\gamma: [a, b]\to \mathbb{C}$，让

$$\int_{\gamma} f(z) dz = \int_{[a, b]} f(\gamma(z)) \mathrm{d}\gamma(z) = \int_{[a, b]} f(\gamma(z))\gamma'(z) \mathrm{d}z$$

或者，

$$\int_{\gamma} (u+\mathrm{i}v)(\mathrm{d}x+\mathrm{i}\cdot\mathrm{d}y) = \int_{\gamma} (u\mathrm{d}x-v\mathrm{d}y) + \mathrm{i}\int_{\gamma} (u\mathrm{d}y+v\mathrm{d}x)$$

现在给一些具体的例子，

对闭曲线 $\gamma$ 和 $n\neq -1$，有

$$\oint_{\gamma}z^n \mathrm{d}z = \int_{\gamma}\frac{1}{n+1} \mathrm{d}z^{n+1} = 0$$

$n = -1$ 时考虑曲线 $\gamma: [0, 2\pi]\to\mathbb{C}, \theta\mapsto re^{\mathrm{i}\theta}$，换元知积分结果 $2\pi\mathrm{i}$；若原点不在曲线内部，则化为 $\oint \mathrm{d}\ \mathrm{Ln}z = 0$；通过曲线加减法可以推知，对所有把 0 包含在内部的闭曲线，积分结果均为 $2\pi\mathrm{i}$

我们来说明一个更一般的结论

{% admonition(type="abstract", title="Cauchy 定理") %}
有界区域 $\Omega$ 以有限段光滑曲线为边界，对 $f$ 在 $\bar{\Omega}$ 上连续，在 $\Omega$ 内解析，则有

$$\int_{\partial \Omega} f(z)\mathrm{d}z = 0$$

这将说明路径积分的值只与起点、终点有关，而与选取的路径无关。
{% end %}

证明的大致思想是说，可以把一个三角形上的路径积分转化为它按中点分割成的四个三角形上的路径积分之和。

{% admonition(type="abstract", title="Cauchy 公式") %}
有界区域 $\Omega$ 以有限段光滑曲线为边界，对 $f$ 在 $\bar{\Omega}$ 上连续，在 $\Omega$ 内解析，则对 $z\in\Omega$

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{\partial \Omega}\frac{f(w)}{w-z}\mathrm{d}w$$
{% end %}

这是因为我们可以取 $\varepsilon>0$ 使 $\bar{D(z, \varepsilon)}\subset\Omega$

故 $\mathrm{RHS} = \frac{1}{2\pi\mathrm{i}}\int_{|w-z|=\varepsilon}\frac{f(w)}{w-z}\mathrm{d}w$

令 $f(w) = f(z) + f'(z) + \rho(w, z)(w-z)$，其中 $\lim_{w\to z}\rho(w, z) = 0$

有 $\int_{|w-z|=\varepsilon}f'(z)\mathrm{d}w = 0$，而 $\int_{|w-z|=\varepsilon}\rho(w, z)\mathrm{d}w$ 是常数且随 $\varepsilon\to 0$ 趋向于 0，原式得证。

### 等价性
我们现在可以证明全纯函数和解析函数是等价的条件了。

解析推全纯是容易的，因为幂级数在收敛半径内全纯。

若已知全纯，对 $z_0\in\Omega$，取充分小的 $r$ 使 $\bar{D(z_0, r)}\subset\Omega$，则由 Cauchy 公式，

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = r}\frac{f(w)}{w-z}\mathrm{d}w$$

而由

$$\frac{1}{w-z} = \frac{1}{w-z_0}\cdot\sum_{n=0}^{+\infty}\left(\frac{z-z_0}{w-z_0}\right)^n$$

且该级数是一致收敛的，可写成

$$f(z) = \sum_{n=0}^{+\infty} \frac{1}{2\pi\mathrm{i}}(z-z_0)^n\int_{|w-z_0| = r}\frac{f(w)}{(w-z_0)^{n+1}}\mathrm{d}w$$

即所求的幂级数。可喜可贺！

进一步地，若 $f$ 是全纯的，则 $u, v$ 都是实解析的。

{% admonition(type="abstract", title="Morera 定理") %}
对区域 $\Omega$ 与其上的连续函数 $f$，一下两个条件等价
1. $f$ 在 $\Omega$ 内解析
2. 对 $\Omega$ 内任意逐段光滑曲线围成的有界区域 $D$ 使 $\bar{D}\subset\Omega$，有 $\int_{\partial D}f(w)\mathrm{d}w = 0$
{% end %}

1 推 2 是 Cauchy 定理。

对于2 推 1，我们考虑任意 $z_0\in\Omega$ 及 $\varepsilon>0$ 使 $D(z_0, \varepsilon)\subset\Omega$ 让 $f$ 在上面是解析的。

对该邻域中的点 $z$，令 $F(z) = \int_{z_0}^z f(w)\mathrm{d}w$，它是良定义的。

可证 $F$ 在该邻域内可导，且 $F'=f$，从而 $F$ 是解析的，从而 $f$ 是解析的。

其中的一部分结论整理如下：

{% admonition(type="abstract", title="存在原函数") %}
对单连通区域 $D$ 和它上的解析函数 $f$，则存在它上的函数 $F$ 使 $F'=f$
{% end %}

对非单连通区域，有反例：$\mathbb{C}-\\{0\\}$ 上的 $f: z\mapsto \frac{1}{z}$

### 幂级数工具
对一个指定的区域 $\Omega$ 上的解析函数 $f$，令 $S = \\{z\in\Omega\mid f'(z) = f''(z) = \cdots = 0 \\}$

由连续性知 $\\{f^{(n)}(z) = 0\\}$ 为闭集，从而 $S$ 为闭集。

又，对 $z_0\in S$ 有 $f$ 在其一个邻域内可展成幂级数，对应的 $D(z_0, \varepsilon)\subset S$，知 $S$ 为开集。

从而，若 $S$ 非空，那么它就是整个 $\Omega$，从而 $f$ 在 $\Omega$ 上是常值的。

这就给出了结论：对区域 $\Omega$ 上的非常值解析函数 $f$ 及任一点 $z_0$，存在一个自然数 $m$，使
* $f'(z_0) = \cdots = f^{(n-1)}(z_0) = 0$
* $f^{(n)}(z_0) \neq 0$

此时称 $z_0$ 为 $m$ 阶零点。

从而，在 $z_0$ 的某个邻域上有 $f(z) - f(z_0) = (z-z_0)^m g(z)$，其中 $g$ 解析且 $g(z_0) \neq 0$

这将给出如下结论：

{% admonition(type="abstract", title="零点孤立性") %}
对区域 $\Omega$ 上的非常值解析函数 $f$，其零点是孤立的。
{% end %}

我们取 $\varepsilon>0$ 使 $g$ 在对应的圆盘上处处不为 0.

---

进而，我们有：

{% admonition(type="abstract", title="解析函数的刚性/唯一性定理") %}
区域 $\Omega$ 上的解析函数 $f, g$，若存在点列 $\\{z_n\\}$ 且它有 $\Omega$ 内的极限点，则 $\Omega$ 上 $f\equiv g$.
{% end %}

这是因为该极限点不是 $f-g$ 的孤立零点。

如果极限点不在 $\Omega$ 内，则有反例 $f: z\mapsto \sin \frac{1}{z}, g: z\mapsto 0, z_n = \frac{1}{n\pi}$

---

接下来，我们将解析函数看作平面区域到平面区域的映射。

由于对 $f(z)-f(z_0)$ 的 $m$ 阶零点有 $f(z) - f(z_0) = (z-z_0)^m g(z)$，我们可进一步取 $h^m = g$ 使 $f(z) - f(z_0) = ((z-z_0) h(z))^m$.

令 $\varphi(z) = (z-z_0) h(z)$，则 $\varphi'(z_0) = h(z_0)\neq 0$，故存在 $z_0$ 的邻域及 $\varphi(z_0) = 0$ 的邻域使 $\varphi$ 为解析同胚。

{% admonition(type="abstract", title="开映射定理") %}
区域 $\Omega$ 上的非常值解析函数 $f$，将 $\Omega$ 中开集映到开集。
{% end %}

只需 $f(\Omega)$ 开。对 $w_0 = f(z_0)$，其中 $z_0$ 为 $f(z)-f(z_0)$ 的 $m$ 阶零点，存在 $w_0$ 的邻域 $O$，使其中点在 $z_0$ 邻域内有 $m$ 个原像，故 $O\subset f(\Omega)$.

进而我们知道对单叶解析函数 $f$，它是 $\Omega\to f(\Omega)$ 的解析同胚。

### 代数基本定理
我们来使用开映射定理：

{% admonition(type="abstract", title="最大模原理") %}
对区域 $\Omega$ 上的非常值解析函数 $f$，$|f(z)|$ 在 $\Omega$ 内无最大值点。
{% end %}

因为 $f(z_0)$ 是 $f(D(z_0, \varepsilon))$ 的内点。

{% admonition(type="abstract", title="代数基本定理") %}
$n$ 次多项式 $P(z) = a_nz^n+\cdots+a_0$ 在 $\mathbb{C}$ 中有零点。
{% end %}

法一：
> 由 $\lim_{n\to\infty} |P(z)| = +\infty$，存在 $R$ 使 $\min \\{|P(z)|\mid |z|=R\\}>|P(0)|$
>
> 设 $z_0$ 是 $|P(z)|$ 在 $\bar{D(0, R)}$ 的最小值点，则 $P(z_0)$ 为 $P(D(0, R))$ 的内点，故 $|P(z_0)| = 0$

法二：
> 将 $P$ 视作 $\bar{\mathbb{C}}\to\bar{\mathbb{C}}$ 的连续映射。
>
> 其不为常数，故 $P(\bar{\mathbb{C}})$ 为开集，而 $\bar{\mathbb{C}}$ 是紧的，故 $P(\bar{\mathbb{C}})$ 为闭集。从而值域是 $\bar{\mathbb{C}}$

### 积分工具
{% admonition(type="abstract", title="Cauchy 不等式") %}
区域 $\Omega$ 上的解析函数 $|f|\leq M$，则 $\forall z_0\in\Omega, 0<r\leq \mathrm{dist}(z_0, \partial\Omega)$ 有

$$|f^{(n)}(z_0)|\leq \frac{n!M}{r^n}$$
{% end %}

对 $0<r<\mathrm{dist}(z_0, \partial\Omega)$，由

$$f^{(n)}(z_0) = \frac{n!}{2\pi\mathrm{i}}\int_{|w-z_0| = r}\frac{f(w)}{(w-z_0)^{n+1}}\mathrm{d}w$$

有

$$|f^{(n)}(z_0)| = \frac{n!}{2\pi\mathrm{i}}\int_{|w-z_0| = r}\left|\frac{f(w)}{(w-z_0)^{n+1}}\right||\mathrm{d}w|\leq \frac{n!M}{r^n}$$

再让 $r\to\mathrm{dist}(z_0, \partial\Omega)$ 即可。

这表明，我们可以用 $|f(z)|$ 在区域内的最大值控制任意的 $|f^{(n)}(z)|$.

{% admonition(type="abstract", title="Liouville 定理") %}
解析函数 $f$ 在 $\mathbb{C}$ 上有界，则必为常数。
{% end %}

因为可以取 $r\to+\infty$ 使 $|f'(z_0)|\leq 0$.

这表明：在解析同胚意义下，$\mathbb{C}$ 中单连通区域恰有 $\mathbb{C}$ 和 $D(0, 1)$ 两类。

我们将 $\mathbb{C}$ 上的解析函数称为**整函数**，其中不为多项式的称为**超越整函数**。

可以给出 Liouville 定理的推广如下：

{% admonition(type="abstract", title="Weierstrass 定理") %}
对非常值的整函数 $f$，有 $f(\mathbb{C})$ 在 $\mathbb{C}$ 中稠密。
{% end %}

假设结论不成立，存在 $z_0\in\mathbb{C}-\overline{f(\mathbb{C})}$ 为开集，有 $g(z)=\frac{1}{f(z)-z_0}$ 有界，矛盾。

---

我们期待对于定义在 $\mathbb{C}$ 上的超越整函数，能够如多项式一样证明它的值域是 $\mathbb{C}$；而事实上我们有相当接近的结论：

{% admonition(type="abstract", title="Picard 小定理") %}
对 $\mathbb{C}$ 上的超越整函数 $f$，有 $\mathbb{C}-f(\mathbb{C})$ 至多包含一个点。
{% end %}

此证明将略过。

---

{% admonition(type="abstract", title="平均值定理") %}
区域 $\Omega$ 上的解析函数 $f$，对任意 $z_0\in\Omega, 0<r<\mathrm{dist}(z_0, \partial \Omega)$，有：

$$f(z_0) = \frac{1}{2\pi}\int_0^{2\pi} f(z_0+re^{i\theta})\mathrm{d}\theta$$
{% end %}

这可直接由 Cauchy 公式得到。

这给出**平均值不等式**：

$$|f(z_0)| = \frac{1}{2\pi}\int_0^{2\pi} |f(z_0+re^{i\theta})|\mathrm{d}\theta$$

进一步有：

$$f(z_0) = \frac{1}{\pi R^2}\iint\limits_{D(z_0, R)} f(z) \mathrm{d}S$$

### 平方可积解析函数
这一节中我们假定区域 $\Omega$ 以有限条逐段光滑曲线为边界。

对其上的复值函数 $f$，称它为**平方可积函数**，若 $|f(z)^2|$ 在 $\Omega$ 上广义可积，其全体记作 $A^2(\Omega)$.

若 $f, g\in A^2(\Omega)$，就有 $|f(z)+g(z)|^2\leq 2 (|f(z)|^2 + |g(z)|^2)$，即 $f+g\in A^2(\Omega)$，从而易知 $A^2(\Omega)$ 是复线性空间。

我们进一步在其上定义**内积**如下：

$$(f, g) = \iint\limits_\Omega f(z)\overline{g(z)} \mathrm{d}S$$

它满足对称性 $(f, g) = \overline{(g, f)}$ 及线性性、正定性。

我们有 Cauchy 不等式 $(f, g)^2 < (f, f) (g, g)$.

现在可以给出一个最大模原理的新证明：
> 设 $z_0\in\Omega$ 是 $|f(z)|$ 最大值点，有
> $$\iint\limits_{D(z_0, \epsilon)} (|f(z_0)|^2 - |f(z)|^2) \mathrm{d}S\geq 0$$
> 使用 Cauchy 不等式，得到 $|f(z)|^2 \equiv |f(z_0)|^2$.

有了内积，我们可以进一步典范地定义范数 $\\|f\\| = \sqrt{(f, f)}$、度量，它是完备的。

我们称两函数**正交**，如果 $(f, g) = 0$，**单位正交函数系**是指一族 $(\varphi_i, \varphi_j) = \delta_{ij}$，如果不存在函数与它的元素都正交，则进一步称为**完备单位正交函数系**。

我们考虑函数 $f$ 对于一族完备单位正交函数系的形式级数：

$$f(z) \sim \sum_{n=1}^\infty (f, \varphi_n)\varphi_n(z)$$

称为 $f$ 关于 $\\{\varphi_n\\}$ 的 Fourier 级数。它均方收敛且内闭收敛于 $f$.

### 非欧几何
{% admonition(type="abstract", title="Schwarz 引理") %}
$f$ 是单位圆盘到自身的解析映射，且 $f(0) = 0$，则：
1. 对圆盘内任一点，有 $|f(z)|\leq |z|$，这会使得 $|f'(0)|\leq 1$
2. 存在 $z_0\neq 0$ 使 $|f(z_0)| = |z_0|$ 或 $|f'(0)| = 1$ 的充要条件是 $f(z)=e^{\mathrm{i}\theta}z$
{% end %}

考虑 $f$ 在 $z=0$ 处展开成的幂级数 $a_0+a_1z+\cdots$，常数项为 $0$，故 $\frac{f(z)}{z} = a_1+a_2z+\cdots$ 在单位圆盘内解析。

对 $z_0$，取 $|z_0|<r<1$，由最大模原理有 $\left|\frac{f(z)}{z}\right|\leq \left|\frac{f(r)}{r}\right|< \frac{1}{r}$，令 $r\to 1$ 即有 $|f(z_0)|\leq |z_0|$.

---

我们可以用 Schwarz 引理给出单位圆盘的解析自同胚群：因为 $f$ 与 $f'$ 均可经分式线性变换转为满足 Schwarz 引理的条件，它是保欧氏距离的，从而只能是分式线性变换，形如：

$$f(z) = e^{\mathrm{i}\theta}\frac{z-z_0}{1-\bar{z_0}z}$$

由于这些变换可以将单位圆盘中指定的一点变成另一指定的点，可以得到：

{% admonition(type="abstract", title="更一般形式的 Schwarz 引理") %}
$f$ 是单位圆盘到自身的解析映射，则对任意 $z_1, z_2\in D(0, 1)$ 有：

$$\left|\frac{f(z_1)-f(z_2)}{1-\overline{f(z_1)}f(z_2)}\right|\leq \left|\frac{z_1-z_2}{1-\bar{z_1}z_2}\right|$$
{% end %}

其微分形式为：

$$\frac{|\mathrm{d}f(z)|}{1-|f(z)|^2}\leq \frac{|\mathrm{d}z|}{1-|z|^2}$$

我们在单位圆盘上定义新的弧长微元（称为 Poincaré 度量）：

$$\mathrm{d}s = \frac{|\mathrm{d}z|}{1-|z|^2}$$

就可定义分段光滑曲线的非欧长度，并依次定义非欧距离和测地线，此过程与一般的光滑流形上的做法相同。
