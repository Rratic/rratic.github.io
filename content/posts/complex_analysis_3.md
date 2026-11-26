+++
title = "单复变（三）：Weierstrass 级数理论"
date = 2025-12-04
updates = 2026-11-26

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "分析学"]
+++

使用 Cauchy 定理时就可注意到 $z^k$ 绕着原点积一圈只有在 $k = -1$ 时非零。此种观察给出留数定理，我们正式地用 Laurent 级数去建立理论。

<!-- more -->

## 孤立奇点
### Laurent 级数
我们使用以下 Laurent 级数来研究奇点附近的性质。其中 $n\leq -1$ 部分称为**主部**/奇异部分，$n\geq 0$ 部分称为**正则部分**：

$$\sum_{n = -\infty}^{+\infty} a_n (z - z_0)^n$$

在较好的情形下，Laurent 级数在一个圆环形区域 $D(z_0, r, R)$ 上收敛（两个幂级数分别收敛）。

对圆环形区域上的解析函数，令 $r < r' < |z-z_0| < R' < R$，使用 Cauchy 公式，有：

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = R'}\frac{f(w)}{w-z}\mathrm{d}w - \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = r'}\frac{f(w)}{w-z}\mathrm{d}w$$

其中减号左边部分可以写成正则部分，从而是 $D(0, R)$ 上的解析函数，右边部分则可写成主部。

### 孤立奇点分类
如果 $f$ 在 $z_0$ 处未定义，而在它的一个去心邻域上解析，则称 $z_0$ 是它的**孤立奇点**。此时可以在一个去心邻域上展开成 Laurent 级数。其中：

$$a_n = \frac 1 {2\pi\mathrm i} \int_{|w - z_0| = r} \frac{f(w)}{(w - z_0)^{n+1}} \mathrm dw$$

例如，对 $f(z) = 1 / \sin (1/z)$，有 $1 / k\pi$ 与 $\infty$ 是它的孤立奇点，但 $0$ 不是。你可以在 [Classification of Singularities](https://complex-analysis.com/content/classification_of_singularities.html) 看到很多与奇点有关的函数的彩色绘制；也可阅读[数学绘制](@/posts/shader_2.md)中对应的内容。

{% <definition title="孤立奇点分类"> %}
如果存在 $g$ 补上 $z_0$ 处的值，使得它在 $z_0$ 的邻域上解析，则称 $f$ 可以解析开拓到 $z_0$，且 $z_0$ 称为它的**可去奇点**。否则，若 $1 / f(z)$ 可解析开拓到 $z_0$，则称 $z_0$ 为它的**极点**（pole）。

其余情况称 $z_0$ 为**本性奇点**（essential singularity）。
{% </definition> %}

称 $\infty$ 是 $f$ 的可去奇点/极点/本性奇点，如果 $0$ 是 $f(z^{-1})$ 的对应可去奇点/极点/本性奇点。

{% <theorem title="可去奇点"> %}
以下条件等价：
1. $z_0$ 是可去奇点
2. $\lim_{z \to z_0} f(z)$ 在 $\Complex$ 上存在
3. $f(z)$ 在 $z_0$ 的去心邻域上有界
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部为 $0$
{% </theorem> %}

只需 (3) 推 (4)，设 $f$ 在 $z_0$ 的去心邻域上有界 $M$，则取半径 $\varepsilon$ 的圆有 $a_{-n} \leq M \cdot \varepsilon^n$，令 $\varepsilon\to 0$ 知主部为零。

{% <theorem title="极点"> %}
以下条件等价：
1. $z_0$ 是极点
2. $z_0$ 是 $1 / f(z)$ 的零点（补充定义后）
3. $\lim_{z\to z_0} f(z) = \infty$
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有且仅有有限项非 $0$
{% </theorem> %}

证明 (3) 推 (4) 只需要考虑，对 $z_0$ 是 $1 / f(z)$ 的 $m$ 阶零点（补充定义后），设在邻域内 $(z - z_0)^m g(z)$，则由 $1 / g(z)$ 解析即可。此时也称 $m$ 为极点的**阶数**，对应最小的正整数使得 $a_{-m}$ 非零。

{% <theorem title="本性奇点"> %}
以下条件等价：
1. $z_0$ 是本性奇点
2. $\lim_{z\to z_0} f(z)$ 在 $\bar{\Complex}$ 中不存在
3. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有无穷多项非 $0$
{% </theorem> %}

这是根据前两个结论得到的。

{% <theorem title="Casorati-Weierstrass 定理"> %}
若 $z_0$ 是 $f$ 的一个本性奇点，则对任意 $\varepsilon > 0$，$f(\mathring{D}(z_0, \varepsilon))$ 在 $\Complex$ 中稠密。
{% </theorem> %}

我们知道 $\Complex \setminus \overline{f(\mathring{D}(z_0, \varepsilon))}$ 是开集，存在 $D(z^\ast, \delta)$ 在其内。

令 $g(z) = 1 / (f(z) - z^\ast)$，有 $|g(z)| \leq 1 / \delta$，从而 $z_0$ 是 $g$ 的可去奇点，只能是 $f$ 的可去奇点或极点，与条件矛盾。

{% <theorem title="Picard 大定理"> %}
若 $z_0$ 是 $f$ 的一个本性奇点，则对任意 $\varepsilon > 0$，$\Complex \setminus f(\mathring{D}(z_0, \varepsilon))$ 至多包含一个点。
{% </theorem> %}

证明略。

{% <theorem title="Julia 定理"> %}
$z_0$ 是 $f$ 的一个本性奇点，则存在 $\theta\in [0, 2\pi)$，对任意 $\varepsilon > 0$ 及 $\omega \in \Complex$（至多存在一个例外），在 $|\arg (z-z_0) - \theta| < \varepsilon$ 中 $f(z) = \omega$ 有无穷多解。
{% </theorem> %}

证明略。

### 整函数的奇点
对于整函数 $f$，若 $\infty$ 是其可去奇点，则有 $f$ 在某个区域 $\set{|z| > R}$ 上有界。由于连续函数将紧集映到紧集，在 $\set{|z| \leq R}$ 上亦有界，用 Liouville 定理知只能是常值。

而若 $\infty$ 是阶数 $m$ 的极点，由 $f(z^{-1})$ 在 $0$ 附近 Laurent 展式得 $f$ 在 $\infty$ 附近 Laurent 展式，取其正则部分 $P(z)$，并令 $g(z) = f(z) - P(z)$，则有 $\lim_{z \to \infty} g(z) = 0$，又 $g$ 整函数，它常值。故 $f(z) = P(z)$，为 $m$ 次多项式。

如果 $f: \Complex \to \Complex$ 是全纯自同胚，则它是整函数，且 $\infty$ 是孤立奇点。由 $f$ 无界知 $\infty$ 不是可去奇点，又由 Weierstrass 定理知它不是本性奇点，从而是极点。从而它是多项式，由代数基本定理知是一次的，即只能形如 $ax + b$.

## 亚纯函数
对 $\bar{\Complex}$ 中区域 $\Omega$，若函数 $f$ 在 $\Omega$ 上除了（可能）有极点外处处解析，则称它是**亚纯**的（meromorphic）。如果 $\Omega$ 是紧的（注意 $\bar{\Complex}$ 是紧的），则易见极点只有有限个。

有理函数（rational function）是指形如多项式之商的函数。类似于整函数与多项式的关系，$\bar{\Complex}$ 上的亚纯函数 $f$ 都是有理函数。不妨设极点 $z_1, \dots, z_n, \infty$，对应的 Laurent 展式主部 $L_k(z)$，则 $f(z) - \sum_{k=1}^n L_k(z) - L_\infty(z)$ 是 $\bar{\Complex}$ 上的全纯函数，因而是常数。从而 $f$ 是有理函数。

进一步使用代数基本定理，知 $\bar{\Complex} \to \bar{\Complex}$ 的全纯自同胚只有分式线性变换。

{% <theorem title="Mittag-Leffler 问题 / Cousin 问题Ⅰ"> %}
设 $\\{z_n\\}$ 是无重复项的点列，且 $\lim_{n \to +\infty} z_n = \infty$，每个 $n$ 对应一个：

$$L_n(z) = \frac{a_{n, 1}}{z - z_n} + \frac{a_{n, 2}}{(z - z_n)^2} + \cdots + \frac{a_{n, {m_n}}}{(z - z_n)^{m_n}}$$

则存在 $\Complex$ 上亚纯函数 $f$，使极点集为 $\\{z_n\\}$，且在 $z_n$ 处的 Laurent 展式的主部为 $L_n(z)$.
{% </theorem> %}

重排点列使得 $|z_n| > R_n > |z_{n-1}|$，有 $L_n(z)$ 在 $\overline{D(0, R_n)}$ 的邻域上解析，从而它在 $z = 0$ 的 Taylor 展开在 $\overline{D(0, R_n)}$ 上内闭一致收敛于它，对 $\varepsilon_n > 0$ 可取出多项式 $P_n(z)$ 使得：

$$\max_{z \in \overline{D(0, R_n)}} |L_n(z) - P_n(z)| < \varepsilon_n$$

让 $\sum \varepsilon_n$ 收敛，下式在任意紧集一致收敛，即为所求。

$$f(z) = \sum_{k=1}^{+\infty} [L_k(z) - P_k(z)]$$

{% <theorem title="Cousin 问题Ⅱ"> %}
$\\{z_n\\}$ 是无重复项的点列，且 $\lim_{n \to +\infty} z_n = \infty$，又正整数列 $\\{m_n\\}$，则存在解析函数 $f$，使 $f$ 所有零点是 $\\{z_n\\}$，且在 $z_n$ 处零点的阶数是 $m_n$.
{% </theorem> %}

证明将在之后给出。

## 留数
### 留数定理
我们希望把 Cauchy 定理和 Cauchy 公式推广到有孤立奇点的函数上。

设 $z_0$ 为 $f$ 的孤立奇点，在 $\mathring{D}(z_0, R)$ 上解析，则对该去心圆盘中一个包含了 $z_0$ 的简单闭曲线，有：

$$\int_\Gamma f(z) \mathrm{d}z = \int_{|z-z_0|=\rho} f(z) \mathrm{d}z$$

将 $f$ 在该去心圆盘上展成 Laurent 级数 $\sum_{n=-\infty}^{+\infty} a_n(z - z_0)^n$，由一致收敛，可以逐项积分得：

$$\frac{1}{2\pi\mathrm{i}} \int_{|z-z_0|=\rho} f(z)\mathrm{d}z = a_{-1}$$

定义上式为 $f$ 在 $z_0$ 处的**留数**，记作 $\mathrm{Res}_{z=z_0} f(z)\mathrm{d}z$，另定义无穷处的留数为：

$$-\frac 1 {2\pi\mathrm{i}} \int_{|z| = \rho} f(z) \mathrm{d}z$$

由于我们是对 $1$-形式 $f(z)\mathrm{d}z$ 定义的，这不依赖坐标选取。有时也省略 $\mathrm{d}z$.

在 $z_0$ 为 $m$ 阶极点时，记 $g(z) = (z - z_0)^m f(z)$，使用 $g$ 在 $z_0$ 处的 Taylor 展开式有：

$$\mathrm{Res}_{z = z_0} f(z)\mathrm{d}z = \frac 1 {(m - 1)!}g^{(m - 1)}(z_0)$$

{% <theorem title="留数定理"> %}
$\Omega$ 是 $\bar{\Complex}$ 中以有限条逐段光滑曲线为边界的区域且 $\infty\notin\partial\Omega$，其内部有点 $z_1, z_2, \dots, z_n$，设 $f$ 在 $\Omega$ 中除这些点之外解析，在 $\bar{\Omega}$ 中除这些点之外连续，则：

$$\int_{\partial\Omega} f(z)\mathrm{d}z = 2\pi\mathrm{i}\sum_{i=1}^n\mathrm{Res}_{z=z_i}f(z)$$
{% </theorem> %}

可从 Cauchy 公式推出。

一种补充情况是，若 $f$ 在 $\bar{\Complex}$ 内除 $z_1, z_2, \dots, z_n$ 外解析，则

$$\sum_{i=1}^n\mathrm{Res}_ {z=z_i}f(z) + \mathrm{Res}_ {z=\infty}f(z) = 0$$

### 定积分计算
许多 $\R$ 上难以处理的定积分问题可以放到 $\Complex$ 上来算。

一个直接使用留数定理的例子是：

$$
\begin{split} \int_0^{2\pi} \frac{\mathrm{d}\theta}{a + b\cos\theta}
&= \int_0^{2\pi} \frac{\mathrm{d}z / \mathrm iz}{a + b(z + 1/z)/2} \\\\
&= \frac 2 {\mathrm i} \int_{|z| = 1} \frac{\mathrm dz}{bz^2 + 2az + b} \\\\
&= 4\pi \mathrm{Res}_{z = (-a + \sqrt{a^2 - b^2}) / b} \left(\frac{\mathrm dz}{bz^2 + 2az + b}\right) \\\\
&= \frac{2\pi}{\sqrt{a^2 - b^2}}
\end{split}
$$

---

计算下式需要手动建立回路：

$$\int_0^{+\infty} \frac 1 {(1 + z^2)^{n + 1}} \mathrm dx$$

令 $\gamma$ 是从 $R$ 顺时针转到 $-R$ 的路径，则：

$$\int_{-R}^R f(z)\mathrm{d}z + \int_\gamma f(z)\mathrm dz = 2\pi\mathrm i \cdot \mathrm{Res}_{z = \mathrm i} f(z)\mathrm dz = \frac{(2n-1)!!}{(2n)!!} \pi$$

令 $R \to +\infty$ 有 $\int_\gamma f(z)\mathrm dz \to 0$，故结果为 $\pi (2n-1)!! / 2(2n)!!$.

---

对下式我们选取半环形回路得到 $\pi / 2$：

$$\int_0^{+\infty} \frac{\sin x}{x} \mathrm{d}x = \operatorname{Im} \int_0^{+\infty} \frac{e^{\mathrm ix}} x \mathrm{d}x$$

![Residue Half Ring](/images/complex/res_half_ring.svg)

对下式（$0 < p < 1$）我们取去掉 $\R_{\geq 0}$ 解析的单值分支，然后选取经典的“钥匙孔”回路得到 $\pi / \sin(\pi/p)$：

$$\int_0^{+\infty} \frac{x^{p - 1}}{1 + x} \mathrm{d}x$$

![Residue Key Hole](/images/complex/res_keyhole.svg)

或者，代换 $t = x^p$ 将它转成下式，然后取扇形的回路：

$$\frac 1 p \int_0^{+\infty} \frac 1 {1 + t^{1/p}} \mathrm{d}t$$

## 辐角原理
{% <theorem title="辐角原理"> %}
$f$ 在区域 $D$ 内亚纯，其内 $\Gamma = \partial \Omega$ 是可求长简单闭曲线，且 $f$ 在 $\Gamma$ 上没有零点和极点，则以下积分等于 $f$ 在 $\Gamma$ 内的零点个数减去极点个数（计重数）。

$$\frac 1 {2\pi\mathrm i} \int_\Gamma \frac{f'(z)}{f(z)} \mathrm dz$$
{% </theorem> %}

由 $f$ 在 $\Gamma$ 上没有零点和极点，在 $\Omega$ 内只有有限个零点和极点，设零点 $z_1, \dots, z_n$，极点 $w_1, \dots, w_k$，取充分小的邻域使得两两不交即可。

想象沿着 $\Gamma$ 定义“$\log f(z)$”，则定理说的是，绕一圈后值增加 $2\pi\mathrm ik$，即虚部增加了辐角的改变量 $\Delta_\gamma \operatorname{Arg} w$（可以采用黎曼面的观点）。

{% <theorem title="Rouché 定理"> %}
$f$ 与 $g$ 在区域 $D$ 内解析，其内 $\Gamma = \partial \Omega$ 是可求长简单闭曲线，且在 $\Gamma$ 上 $|g(z)| < |f(z)|$，则 $f$ 与 $f + g$ 在 $\Gamma$ 内的零点个数（记重数）相同。
{% </theorem> %}

因为 $h = f + g$ 的零点个数减去 $f$ 的零点个数为：

$$\frac 1 {2\pi\mathrm i} \int_\Gamma \left[\frac{h'(z)}{h(z)} - \frac{f'(z)}{f(z)}\right] \mathrm dz = \frac 1 {2\pi\mathrm i} \int_\Gamma \frac{(h/f)'(z)}{(h/f)(z)} \mathrm dz = \frac 1 {2\pi} \Delta_\Gamma \operatorname{Arg} \frac h f = 0$$

这里用到的是 $h/f = 1 + g/f$，值域 $\subset \Complex \setminus (-\infty, 0]$，可以取 $\log$ 的一支为原函数。

{% <theorem title="分歧覆盖定理"> %}
$f$ 在区域 $D$ 内解析，$z_0 \in D$ 的像是 $w_0$，$z_0$ 是 $f(z) - w_0$ 的 $m$ 阶零点，则存在 $\rho, \delta > 0$，对任意 $w \in \mathring{D}(w_0, \rho)$ 有 $f(z) - w$ 在 $\mathring{D}(z_0, \delta)$ 内恰有 $m$ 个零点（记重数）；当 $w \neq w_0$ 时，这些零点均为一阶零点。
{% </theorem> %}

由零点孤立性，可取出 $\delta$ 使 $\mathring{D}(z_0, \delta)$ 上 $f(z) - w_0$ 与 $f'(z)$ 无零点，再取 $\rho = \min_{|z - z_0| = \delta} |f(z) - w_0| > 0$.

有 $|z - z_0| = \delta$ 时下式成立，使用 Rouché 定理即可。

$$|(f(z) - w) - (f(z) - w_0)| < |f(z) - w_0|$$

这意味着，$f(z) - w_0$ 在 $z_0$ 附近的性质与 $z^m$ 在 $0$ 附近的性质近似。
