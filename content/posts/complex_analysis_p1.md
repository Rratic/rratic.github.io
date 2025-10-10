+++
title = "复分析（一）：速通"
description = "复变函数重要概念的速通，计划写到 Riemann 单值化定理。"
date = 2025-07-27
updated = 2025-08-12

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

前置知识
- 数学分析
- [点集拓扑](/_misc/topology/)

## 复函数
### 解析函数
在复数域 $\mathbb{C}$ 上，极限的表述可以参照数学分析中的表述。

{% admonition(type="abstract", title="极限") %}
称 $\lim_{z \to z_0} f(z) = w$，如果对任意 $\omega > 0$，存在 $\delta > 0$，对任意 $0<|z-z_0|<\delta$ 有 $|f(z)-w|<\omega$.
{% end %}

更一般地，可以参考拓扑中的紧性相关定义。

以类似的方式，我们可以定义导数。

复函数 $f(z) = u(z) + \mathrm{i}v(z)$ 在 $x+y\mathrm{i}$ 处可导的充要条件是

$\frac{\partial u}{\partial x}, \frac{\partial u}{\partial y}, \frac{\partial v}{\partial x}, \frac{\partial v}{\partial y}$ 在 $z$ 处存在，且

$$
\left\\{\begin{matrix}
\frac{\partial u}{\partial x} = \frac{\partial v}{\partial y}\\\\
\frac{\partial u}{\partial y} = -\frac{\partial v}{\partial x}
\end{matrix}\right.
$$

后者称为 Cauchy-Riemann 条件。

{% admonition(type="abstract", title="全纯函数") %}
若函数 $f$ 在开集 $D$ 内每一点都是复可导的，则称其为 $D$ 上的全纯函数。
{% end %}

我们在之后将说明，全纯函数的导数仍然是全纯的，并且它是解析函数。

由此，

$$
\left\\{\begin{matrix}
\Delta u = \frac{\partial^2 u}{\partial x^2}+\frac{\partial^2 u}{\partial y^2} = 0\\\\
\Delta v = \frac{\partial^2 v}{\partial x^2}+\frac{\partial^2 v}{\partial y^2} = 0
\end{matrix}\right.
$$

我们称满足 $\Delta u = 0$ 的函数 $u$ 为调和函数。

{% admonition(type="abstract", title="Lucas 定理") %}
若多项式 $P$ 的零点均在某个半平面 $H$ 内，则 $P'$ 的零点也在 $H$ 内。从而，$P'$ 的零点在 $P$ 的零点组成的凸包内。
{% end %}

我们在之后证明代数基本定理。

设 $P(z) = k\prod_{i=1}^n (z-z_i)$，则 $\frac{P'(z)}{P(z)} = \sum_{i=1}^n \frac{1}{z-z_i}$

记半平面 $H\colon \operatorname{Im} \frac{z-a}{b}<0$

分析 $\operatorname{Im} \frac{bP'(z)}{P(z)} = \sum_{i=1}^n \operatorname{Im} \frac{b}{z-z_i}$ 即可。

{% admonition(type="abstract", title="收敛圆") %}
对幂级数 $a_0+a_1z+a_2z^2+\cdots$，存在扩展实数 $0\leq r\leq \infty$ 满足
1. 对 $|z|<r$ 级数收敛
2. 对 $|z|>r$ 级数发散
3. 在 $|z|<r$ 内级数的和是解析函数，导数可由逐项微分得到，对应的收敛半径相同。
{% end %}

取 $\frac{1}{r} = \lim_{n \to \infty}\sup\sqrt[n]{|a_n|}$.

---

解析函数是保角的，也就是说 $z_1(t)$ 与 $z_2(t)$ 的夹角等于 $f(z_1(t))$ 与 $f(z_2(t))$ 

### 函数定义
定义 $\exp$ 为满足 $f'(z)=f(z)$ 及 $f(0)=1$ 的解。

有 $(e^z\cdot e^{a-z})' = e^z\cdot e^{a-z}+e^z\cdot (-e^{a-z}) = 0$ 故 $e^{z+w}=e^z\cdot e^w$

定义 $\cos z = \frac{e^{iz}+e^{-iz}}{2}, \sin z = \frac{e^{iz}-e^{-iz}}{2i}$

### 连续映射 {#continuous-mapping}
点集拓扑告诉我们，连续映射把紧集映成紧集。因而对紧集到 $\mathbb{R}$ 的映射 $f$，像是紧集，从而是闭集，从而有最大、最小值。一般来说，可以对 $z\mapsto |f(z)|$ 使用这个定理。

称两个度量空间间的函数 $f$ 一致连续，如果 $\forall\epsilon>0: \exists\delta>0, \forall (x_1, x_2): d(x_1, x_2)<\delta \implies d'(f(x_1), f(x_2))<\epsilon$

由数学分析易知紧集上连续函数是一致连续的。

## 复平面
### 曲线
用连续映射 $z=f(t), t\in [\alpha, \beta]$ 表示曲线。若 $z'(t)$ 存在且连续，称该弧是可微的。若再有 $z'(t)\neq 0$，称该弧是正则的（使得折线不被认为是狭义的“曲线”）。若 $f(\alpha)=f(\beta)$ 则称其为闭曲线。

### 线性变换
分式线性变换是指 $z\mapsto \frac{az+b}{cz+d}$ 及 $z\mapsto \frac{a\bar{z}+b}{c\bar{z}+d}$，均可将扩充平面映到自身。

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

### 定义说明
在复分析中，我们称**区域**是指 $\mathbb{C}$ 上道路连通的开集。

有时我们会提到“单连通”，这是指：区域的内的任一条 Jordan 曲线（不自交、连续、闭合的曲线）均为某个子区域的边界。

之后的许多函数性质都是在一个区域上说的。

区域 $\Omega$ 上的解析函数 $f$ 称为**单叶解析**的，如果它在 $\Omega$ 上是单的。

区域到区域的映射 $f: \Omega_1\to\Omega_2$ 称为**解析同胚/共形映射**，如果 $f$ 是单叶解析的，并且 $f^{-1}$ 也是解析的。

## 复积分
解析函数的许多性质最好用复积分证明。

在一个实区间上的定积分可以直接由实积分推广得到。

对给定的连续曲线 $\gamma$，作 Riemann 和，极限可取到时记为路径积分 $\int_{\gamma} f(z) \mathrm{d}z$

或者，设 $\gamma: [a, b]\to \mathbb{C}$，让

$$\int_{\gamma} f(z) dz = \int_{[a, b]} f(\gamma(z)) \mathrm{d}\gamma(z) = \int_{[a, b]} f(\gamma(z))\gamma'(z) \mathrm{d}z$$

对区域 $D$，由 Stokes 定理有 $\int_{\partial D} \omega = \int_{D}\rm{d}\omega = 0$

从而路径积分只与起点、终点有关，而与选取的路径无关。

{% admonition(type="abstract", title="ln(f)") %}
$f$ 为区域 $D$ 上的处处非 0 全纯函数，则存在 $e^g = f$
{% end %}

取 $g$ 的实部为 $\ln |f|$

{% admonition(type="abstract", title="n sqrt") %}
$f$ 为区域 $D$ 上的处处非 0 全纯函数，则存在 $g^n = f$
{% end %}

取 $g=\exp(\frac{1}{n}\ln(f))$

{{ todo() }}
