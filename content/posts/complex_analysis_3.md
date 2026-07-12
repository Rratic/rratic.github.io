+++
title = "单复变 Weierstrass 级数理论"
date = 2025-12-04

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "分析学"]
+++

> 一般认为 Cauchy 积分理论、Weierstrass 级数理论和共形映射理论是单复变函数理论中的三个最重要的组成部分。

本章内容为 Laurent 级数与留数。

<!-- more -->

## Laurent 级数
### 定义
我们使用 Laurent 级数来研究奇点附近的性质。定义为：

$$\sum_{-\infty}^{+\infty} a_n (z - z_0)^n$$

其中 $n\leq -1$ 部分称为**主部**，$n\geq 0$ 部分称为**正则部分**。

在较好的情形下，Laurent 级数在一个圆环形区域 $D(z_0, r, R)$ 上收敛（两个幂级数分别收敛）。

对圆环形区域上的解析函数，令 $r < r' < |z-z_0| < R' < R$，使用 Cauchy 公式，有：

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = R'}\frac{f(w)}{w-z}\mathrm{d}w - \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = r'}\frac{f(w)}{w-z}\mathrm{d}w$$

其中减号左边部分可以写成正则部分，从而是 $D(0, R)$ 上的解析函数，右边部分则可写成主部。

### 孤立奇点分类
如果 $f$ 在 $z_0$ 处未定义，而在它的一个去心邻域上解析，则称 $z_0$ 是它的**孤立奇点**。

例如，对 $f(z) = 1 / \sin (1/z)$，有 $1 / k\pi$ 与 $\infty$ 是它的孤立奇点，但 $0$ 不是。你可以在 [Classification of Singularities](https://complex-analysis.com/content/classification_of_singularities.html) 看到很多与奇点有关的函数的彩色绘制；也可阅读[基于 GLSL 的色彩与数学绘制](@/posts/shader_2.md)中对应的内容。

{% admonition(type="definition", title="孤立奇点分类") %}
如果存在 $g$ 补上 $z_0$ 处的值，使得它在 $z_0$ 的邻域上解析，则称 $f$ 可以解析开拓到 $z_0$，且 $z_0$ 称为它的**可去奇点**。否则，若 $1 / f(z)$ 可解析开拓到 $z_0$，则称 $z_0$ 为它的**极点**。

其余情况称 $z_0$ 为**本性奇点**。
{% end %}

{% admonition(type="theorem", title="可去奇点") %}
以下条件等价：
1. $z_0$ 是可去奇点
2. $\lim_{z \to z_0} f(z)$ 在 $\Complex$ 上存在
3. $f(z)$ 在 $z_0$ 邻域上有界
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部为 $0$
{% end %}

其中 (3) 推 (4) 是不平凡的。设展式为 $f(z) = \sum_{-\infty}^{+\infty} a_n (z-z_0)^n$，取 $\varepsilon$ 则：

$$a_n = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = \varepsilon}\frac{f(w)}{(w-z_0)^{n+1}}\mathrm{d}w$$

设 $f$ 在 $z_0$ 的（去心）邻域上有界 $M$，则有 $a_{-n} \leq M \cdot \varepsilon^n$，令 $\varepsilon\to 0$ 知主部为零。

{% admonition(type="theorem", title="极点") %}
以下条件等价：
1. $z_0$ 是极点
2. $z_0$ 是 $1 / f(z)$ 的零点
3. $\lim_{z\to z_0} f(z) = \infty$
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有且仅有有限项非 $0$
{% end %}

证明 (3) 推 (4) 只需要考虑，对 $z_0$ 是 $1 / f(z)$ 的 $m$ 阶零点（补充定义后），设在邻域内 $(z - z_0)^m g(z)$，则由 $1 / g(z)$ 解析即可。

{% admonition(type="theorem", title="本性奇点") %}
以下条件等价：
1. $z_0$ 是本性奇点
2. $\lim_{z\to z_0} f(z)$ 在 $\bar{\Complex}$ 中不存在
3. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有无穷多项非 $0$
{% end %}

这是根据前两个结论得到的。

{% admonition(type="theorem", title="Weierstrass 定理") %}
若 $z_0$ 是 $f$ 的一个本性奇点，则对任意 $\varepsilon > 0$，$f(\mathring{D}(z_0, \varepsilon))$ 在 $\Complex$ 中稠密。
{% end %}

我们知道 $\Complex \setminus \overline{f(\mathring{D}(z_0, \varepsilon))}$ 是开集，存在 $D(z^\ast, \delta)$ 在其内。

令 $g(z) = 1 / (f(z) - z^\ast)$，有 $|g(z)| \leq 1 / \delta$，从而 $z_0$ 是 $g$ 的可去奇点，只能是 $f$ 的可去奇点或极点，与条件矛盾。

{% admonition(type="theorem", title="Picard 大定理") %}
若 $z_0$ 是 $f$ 的一个本性奇点，则对任意 $\varepsilon > 0$，$\Complex \setminus f(\mathring{D}(z_0, \varepsilon))$ 至多包含一个点。
{% end %}

证明略。

{% admonition(type="theorem", title="Julia 定理") %}
$z_0$ 是 $f$ 的一个本性奇点，则存在 $\theta\in [0, 2\pi)$，对任意 $\varepsilon > 0$ 及 $\omega \in \Complex$（至多存在一个例外），在 $|\arg (z-z_0) - \theta| < \varepsilon$ 中 $f(z) = \omega$ 有无穷多解。
{% end %}

证明略。

{% admonition(type="theorem", title="全纯自同胚") %}
$f: \Complex \to \Complex$ 是全纯自同胚当且仅当它形如 $az + b$（$a$ 非零）。
{% end %}

只需证左推右。我们知道 $f$ 是整函数，且 $\infty$ 是孤立奇点。

由 $f$ 无界知 $\infty$ 不是可去奇点，又由 Weierstrass 定理知它不是本性奇点，从而是极点。

从而它是多项式，由代数学基本定理知是一次的。

### 亚纯函数
对 $\bar{\Complex}$ 中区域 $\Omega$ 上的函数 $f$，若除了（可能）有极点外处处解析，则称它是**亚纯**的。记亚纯函数全体构成的域为 $m(\Omega)$.

{% admonition(type="theorem", title="Mittag-Leffler 问题的 $\Complex$ 版本") %}
设 $\\{z_n\\}$ 是无重复项的点列，且 $\lim_{n \to +\infty} z_n = \infty$，每个 $n$ 对应一个：

$$L_n(z) = \frac{a_{n_1}}{z-z_n} + \frac{a_{n_2}}{(z-z_n)^2} + \cdots + \frac{a_{n_{m_n}}}{(z-z_n)^{m_n}}$$

则存在 $\Complex$ 上亚纯函数 $f$，使极点集为 $\\{z_n\\}$，且在 $z_n$ 处的 Laurent 展式的主部为 $L_n(z)$.
{% end %}

不妨设 $z_n \in D(0, n, n+1)$，取序列 $\\{a_n\\}$ 使 $a_n > 0$ 且和收敛，设对 $z_1, \dots, z_{n-1}$ 已取到 $P_1(z), \dots, P_{n-1}(z)$，使对任意 $k = 1, 2, \dots, n-1$ 有：

$$\max_{z \in \overline{D(0, k)}} \set{|L_k(z)-P_k(z)|} < a_k$$

有 $L_n(z)$ 在 $\overline{D(0, n)}$ 的邻域上解析，从而它在 $z=0$ 展开的幂级数在 $\overline{D(0, n)}$ 上一致收敛于它，可用多项式一致逼近，从而可取出 $P_n(z)$.

令 $f(z) = \sum_{k=1}^{+\infty} [L_k(z) - P_k(z)]$，它即是所求。

{% admonition(type="theorem", title="有理函数") %}
$\bar{\Complex}$ 上的亚纯函数都是有理函数。
{% end %}

设亚纯函数 $f$，有极点 $z_1, \dots, z_l, \infty$，且对应的 Laurent 展式主部：

$$L_k(z) = \frac{a_{k_1}}{z-z_k} + \frac{a_{k_2}}{(z-z_k)^2} + \cdots + \frac{a_{k_{m_k}}}{(z-z_k)^{m_k}}$$

$$L_\infty(z) = b_1z + b_2z^2 + \cdots + b_mz^m$$

则 $f(z) - \sum_{k=1}^l L_k(z) - L_\infty(z)$ 是 $\bar{\Complex}$ 上的全纯函数，因而是常数。从而 $f$ 是有理函数。

利用此可以说明 $\bar{\Complex}$ 到 $\bar{\Complex}$ 的全纯自同胚只能是分式线性变换，这是因为考虑 $P(z)/Q(z)$，一一映射要求 $P$ 与 $Q$ 都是一次的。

{% admonition(type="theorem", title="Cousin 问题 2 的 $\Complex$ 版本") %}
$\\{z_n\\}$ 是无重复项的点列，且 $\lim_{n\to+\infty}z_n = \infty$，又正整数列 $\\{m_n\\}$，则存在解析函数 $f$，使 $f$ 所有零点是 $\\{z_n\\}$，且在 $z_n$ 处零点的阶数是 $m_n$.
{% end %}

证明过程用到无穷乘积，此处略过。

它的一个推论是：对 $\bar{\Complex}$ 上亚纯函数 $f$，存在解析函数 $g$ 与 $h$ 使得 $f(z) = g(z) / h(z)$.

## 留数
### 基本概念
我们希望把 Cauchy 定理和 Cauchy 公式推广到有孤立奇点的函数上。

设 $z_0$ 为 $f$ 的孤立奇点，在 $\mathring{D}(z_0, R)$ 上解析，则对该去心圆盘中一个包含了 $z_0$ 的简单闭曲线，有：

$$\int_\Gamma f(z) \mathrm{d}z = \int_{|z-z_0|=\rho} f(z) \mathrm{d}z$$

将 $f$ 在该去心圆盘上展成 Laurent 级数 $\sum_{n=-\infty}^{+\infty} a_n(z - z_0)^n$，由一致收敛，可以逐项积分得：

$$\frac{1}{2\pi\mathrm{i}} \int_{|z-z_0|=\rho} f(z)\mathrm{d}z = a_{-1}$$

定义上式为 $f$ 在 $z_0$ 处的**留数**，记作 $\mathrm{Res}_{z=z_0} f(z)\mathrm{d}z$，另定义无穷处的留数为：

$$-\frac 1 {2\pi\mathrm{i}} \int_{|z| = \rho} f(z) \mathrm{d}z$$

由于我们是对 $1$-形式 $f(z)\mathrm{d}z$ 定义的，这不依赖坐标选取。有时也省略 $\mathrm{d}z$.

在 $z_0$ 为 $m$ 阶零点时，记 $f(z) = (z - z_0)^m g(z)$，设 $g$ 在 $z_0$ 处的 Taylor 展开式为：

$$\sum_{n=0}^{+\infty} \frac 1 {n!} g^{(n)}(z_0)(z - z_0)^n$$

这就给出以下计算方法：

$$\mathrm{Res}_{z = z_0} f(z)\mathrm{d}z = \frac 1 {(m - 1)!}g^{(m - 1)}(z_0)$$

{% admonition(type="theorem", title="留数定理") %}
$\Omega$ 是 $\bar{\Complex}$ 中以有限条逐段光滑曲线为边界的区域且 $\infty\notin\partial\Omega$，其内部有点 $z_1, z_2, \dots, z_n$，设 $f$ 在 $\Omega$ 中除这些点之外解析，在 $\bar{\Omega}$ 中除这些点之外连续，则：

$$\int_{\partial\Omega} f(z)\mathrm{d}z = 2\pi\mathrm{i}\sum_{i=1}^n\mathrm{Res}_{z=z_k}f(z)$$
{% end %}

可从 Cauchy 公式推出。

一种补充情况是，若 $f$ 在 $\bar{\Complex}$ 内除 $z_1, z_2, \dots, z_n$ 外解析，则

$$\sum_{i=1}^n\mathrm{Res}_ {z=z_k}f(z) + \mathrm{Res}_ {z=\infty}f(z) = 0$$

### 辐角原理
{% admonition(type="theorem", title="辐角原理") %}
$f$ 在区域 $D$ 内亚纯，$\Gamma = \partial \Omega, \Omega\subseteq D$ 是可求长简单闭曲线，且 $f$ 在 $\Gamma$ 上没有零点和极点，则

$$\frac{1}{2\pi\mathrm{i}}\int_\Gamma \frac{f'(z)}{f(z)} \mathrm{d}z$$

等于 $f$ 在 $\Gamma$ 内的零点个数减去极点个数（记重数）。
{% end %}

由 $f$ 在 $\Gamma$ 上没有零点和极点，在 $\Omega$ 内只有有限个零点和极点，设零点 $z_1, \dots, z_n$，极点 $w_1, \dots, w_k$，取充分小的 $r$ 使所有的 $D(z_i, r)$ 及 $D(w_j, r)$ 两两不交即可。

实际上此定理说的是这个值等于 $w$ 沿着 $\gamma = f(\Gamma)$ 前进的辐角改变量 $\Delta_\gamma \operatorname{Arg} w$ 除以 $2\pi$.

{% admonition(type="theorem", title="Rouché 定理") %}
$f$ 与 $g$ 在区域 $D$ 内解析，$\Gamma = \partial \Omega,\ \Omega\subseteq D$ 是可求长简单闭曲线，且在 $\Gamma$ 上 $|g(z)|<|f(z)|$，则 $f$ 与 $f+g$ 在 $\Gamma$ 内的零点个数（记重数）相同。
{% end %}

因为 $h = f + g$ 的零点个数减去 $f$ 的零点个数为：

$$\frac{1}{2\pi\mathrm{i}}\int_\Gamma \left[\frac{h'(z)}{h(z)}-\frac{f'(z)}{f(z)}\right] \mathrm{d}z = \frac{1}{2\pi\mathrm{i}}\int_\Gamma \frac{(h/f)'(z)}{(h/f)(z)} \mathrm{d}z = \frac{1}{2\pi} \Delta_\Gamma \operatorname{Arg} \frac{h}{f} = 0$$

{% admonition(type="theorem", title="分歧覆盖定理") %}
$f$ 在区域 $D$ 内解析，$z_0\in D$ 的像是 $w_0$，$z_0$ 是 $f(z)-w_0$ 的 $m$ 阶零点，则存在 $\rho, \delta > 0$，对任意 $w\in \mathring{D}(w_0, \rho)$ 有 $f(z)-w$ 在 $\mathring{D}(z_0, \delta)$ 内恰有 $m$ 个一阶零点。
{% end %}

由零点孤立性，可取出 $\delta$ 使 $\mathring{D}(z_0, \delta)$ 上 $f(z) - w_0$ 与 $f'(z)$ 无零点，再取 $\rho = \min_{|z-z_0|=\delta} |f(z) - w_0| > 0$.

有 $|z-z_0|=\delta$ 时下式成立，使用 Rouché 定理即可。

$$|(f(z) - w) - (f(z) - w_0)| < |f(z) - w_0|$$

这意味着，$f(z) - w_0$ 在 $z_0$ 附近的性质与 $z^m$ 在 $0$ 附近的性质近似。

### 定积分计算
许多 $\R$ 上难以处理的定积分问题可以放到 $\Complex$ 上来算。

一个直接的例子是：

$$\int_0^{2\pi} \frac{\mathrm{d}\theta}{a + b\cos\theta} = \frac{2}{\mathrm{i}}\int_{|z|=1} \frac{\mathrm{d}z}{bz^2 + 2az + b} = 4\pi \mathrm{Res}_{z = (-a + \sqrt{a^2 - b^2}) / b} \left(\frac{\mathrm{d}z}{bz^2 + 2az + b}\right) = \frac{2\pi}{\sqrt{a^2 - b^2}}$$

---

计算下式需要手动建立回路：

$$\int_0^{+\infty} \frac{1}{(1 + z^2)^{n + 1}} \mathrm{d}x$$

令 $\gamma$ 是从 $R$ 顺时针转到 $-R$ 的路径，则：

$$\int_{-R}^{R} f(z)\mathrm{d}z + \int_\gamma f(z)\mathrm{d}z = 2\pi\mathrm{i}\cdot\mathrm{Res}_{z=\mathrm{i}} f(z)\mathrm{d}z = \frac{(2n-1)!!}{(2n)!!} \pi$$

令 $R \to +\infty$ 有 $\left|\int_\gamma f(z)\mathrm{d}z\right|\to 0$，故结果为 $\pi (2n-1)!! / 2(2n)!!$.

---

对下式我们选取半环形回路：

$$\int_0^{+\infty} \frac{\sin x}{x} \mathrm{d}x$$

![Residue Half Ring](/images/complex/res_half_ring.svg)

对下式（$0 < p < 1$）我们选取经典的“钥匙孔”回路：

$$\int_0^{+\infty} \frac{x^{p - 1}}{1 + x} \mathrm{d}x$$

![Residue Key Hole](/images/complex/res_keyhole.svg)
