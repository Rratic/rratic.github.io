+++
title = "复分析（一）：速通指南"
description = "复变函数重要概念的速通，计划写到 Riemann 单值化定理。"
date = 2025-07-27
updated = 2025-10-10

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

复值函数 $f = u + \mathrm{i}v$ 在 $x+y\mathrm{i}$ 处可导的充要条件是

$\frac{\partial u}{\partial x}, \frac{\partial u}{\partial y}, \frac{\partial v}{\partial x}, \frac{\partial v}{\partial y}$ 在 $z$ 处存在，且

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

若 $f$ 在区域 $D$ 内每一点都可展开为幂级数，则称它是**解析**的。

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

{% admonition(type="abstract", title="性质") %}
单连通区域 $\Omega$，对任意调和函数 $u$，存在函数 $v$ 为它的共轭调和，且 $v$ 在差一个常数下是唯一的。
{% end %}

定义 $\omega = \frac{\partial u}{\partial x}\mathrm{d}y - \frac{\partial u}{\partial y}\mathrm{d}x$

有 $\mathrm{d}\omega = \left(\frac{\partial^2 u}{\partial x^2} + \frac{\partial^2 u}{\partial y^2}\right)\mathrm{d}x\wedge \mathrm{d}y = 0$

对区域 $D\subset \Omega$，有 $\int_{\partial D} \omega = \int_{D}\rm{d}\omega = 0$

这个结论说明路径积分只与起点、终点有关，而与选取的路径无关（我们会在之后给出路径积分的定义）

有 $v(x, y) = \int_{(x_0, y_0)}^{(x, y)}\omega$ 即可（差一个常数下）。

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
全纯函数是保角的，也就是说 $z_1(t)$ 与 $z_2(t)$ 的夹角等于 $f(z_1(t))$ 与 $f(z_2(t))$ 
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
不严格地说，多值函数 $F: \Omega \to \mathbb{C}$ 中的值是复数构成的集合。

例如，因为复数上的 $\exp$ 不是单的，它的反函数形如 $\mathrm{Ln} (r\cdot e^{\mathrm{i}\theta}) = \mathrm{Ln} r + \mathrm{i}\theta$

一般来说，我们希望找到它的一个单值分支 $f$，即在 $\Omega$ 上有 $f(z)\in F(z)$ 且它是解析的。

例如，在 $\mathbb{C}$ 去掉一个原点引出的射线这一区域上，$\mathrm{Ln}$ 是可以定义单值分支的。

TODO
- Riemann 面

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

若将 $z$ 视作 $\frac{z_1}{z_2}$，并写为 $z = \begin{pmatrix}z_1\\\\ z_2\end{pmatrix}$ 则前者为：

$$
z\mapsto \begin{pmatrix}a&b\\\\ c&d\end{pmatrix} z
$$

可由典型变换平移、位似、伊朗式反演复合得到。

{% admonition(type="abstract", title="性质") %}
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

我们在之前已说明，如果函数在单连通区域上都有定义，则路径积分只与起点、终点有关，而与选取的路径无关。

{{ todo() }}
