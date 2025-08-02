+++
title = "【草稿】复分析（一）：速通"
description = "重要概念的速通。"
date = 2025-07-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["讲义", "数学", "分析", "复分析"]
+++

> 复分析会告诉你：性质好的函数性质有多好。

前置知识
- 数学分析

## 复函数
### 解析函数
在复数域 $\mathbb{C}$ 上，极限的表述可以参照数学分析中的表述。

{% admonition(type="abstract", title="极限") %}
称 $\lim_{z \to z_0} f(z) = w$，如果对任意 $\omega > 0$，存在 $\delta > 0$，对任意 $0<|z-z_0|<\delta$ 有 $|f(z)-w|<\omega$.
{% end %}

更一般地，可以参考[拓扑](/posts/index-topology/)中的紧性相关定义。

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

{% admonition(type="abstract", title="解析/全纯函数") %}
若函数 $f$ 在开集 $D$ 内每一点都是复可导的，则称其为 $D$ 上的解析/全纯函数。
{% end %}

我们在之后将说明，解析函数的导数仍然是解析的。

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

### 函数定义
定义 $\exp$ 为满足 $f'(z)=f(z)$ 及 $f(0)=1$ 的解。

有 $(e^z\cdot e^{a-z})' = e^z\cdot e^{a-z}+e^z\cdot (-e^{a-z}) = 0$ 故 $e^{z+w}=e^z\cdot e^w$

定义 $\cos z = \frac{e^{iz}+e^{-iz}}{2}, \sin z = \frac{e^{iz}-e^{-iz}}{2i}$
