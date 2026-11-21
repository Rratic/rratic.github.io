+++
title = "数学分析Ⅲ期末复习笔记"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "分析学"]
+++

数分课程的最后一个学期，主要是处理掉一些剩下的琐碎东西。

<!-- more -->

## 含参变量积分
### 含参变量常义积分
若 $f(x, y)$ 在 $[a, b] \times [c, d]$ 中有定义，且对 $x \in [a, b]$，积分 $\int_c^d f(x, y) \mathrm dy$ 存在，则称此积分 $I(x)$ 为含参变量的定积分。

结合以下两个定理即可得到，若对每个 $y$ 有 $f(\cdot, y)$ 在 $x_0$ 连续，则 $I(x)$ 在 $x_0$ 连续。

{% <theorem title="Heine 定理"> %}
$\lim_{x \to x_0} f(x) = A$ 当且仅当对任意 $x_n \to x_0, x_n \neq x_0$ 有 $\lim_{n \to \infty} f(x_n) = A$.
{% </theorem> %}

{% <theorem title="Arzelà 有界收敛定理"> %}
在 $[a, b]$ 上，设 $f_n(x)$ 逐点收敛到 $f(x)$，且 $f_n(x)$ 与 $f(x)$ 均可积，若 $f_n(x)$ 一致有界，则：

$$\lim_{n \to +\infty} \int_a^b f_n(x) \mathrm{d}x = \int_a^b f(x) \mathrm{d}x$$
{% </theorem> %}

这个连续性也就是说，我们有：

$$\lim_{x \to x_0} \int_c^d f(x, y) \mathrm dy = \int_c^d \lim_{x \to x_0} f(x, y) \mathrm dy$$

如果 $f, f_x'$ 连续，且 $\varphi(x), \psi(x)$ 在 $[a, b]$ 可微，则我们还有：

$$\left(\int_{\varphi(x)}^{\psi(x)} f(x, t) \mathrm dt\right)' = \int_{\varphi(x)^{\psi(x)}} f_x'(x, t) \mathrm dt + f(x, \psi(x))\psi'(x) - f(x, \varphi(x)\varphi'(x))$$

对 $f$ 在 $[a, b] \times [c, d]$ 有界，$f(x, \cdot)$ 与 $f(\cdot, y)$ 可积且以下积分存在，则它们相等：

$$\int_c^d \mathrm dy \int_a^b f(x, y) \mathrm dx = \int_a^b \mathrm dx \int_c^d f(x, y) \mathrm dy$$

### 含参变量广义积分
考虑 $\int_c^{+\infty} f(x, y) \mathrm dy$ 等广义积分，易定义其一致收敛。对应有 Cauchy 原理、Weierstrass 判别法、Dirichlet 判别法、Abel 判别法。

一致收敛及 $[a, b] \times [c, +\infty)$ 上的连续性可以使得：

$$\int_a^b \mathrm dx \int_c^{+\infty} f(x, y) \mathrm dy = \int_c^{+\infty} \mathrm dy \int_a^b f(x, y) \mathrm dx$$

故在 $f_x'$ 连续时，求导与积分可交换（并且有类似 Dini 定理的结论）：

$$\frac{\mathrm d}{\mathrm dx} \int_c^{+\infty} f(x, y) \mathrm dy = \int_c^{+\infty} f_x'(x, y) \mathrm dy$$

若 $f$ 在 $[a, +\infty) \times [c, +\infty)$ 上连续，$I(x)$ 关于 $x$，$J(y)$ 关于 $y$ 内闭一致收敛，且下式有一侧的绝对值版本存在，则它们相等：

$$\int_c^{+\infty} \left[\int_a^{+\infty} f(x, y) \mathrm dx\right] \mathrm dy = \int_a^{+\infty} \left[\int_c^{+\infty} f(x, y) \mathrm dy\right] \mathrm dx$$
