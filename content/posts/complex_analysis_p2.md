+++
title = "【草稿】复分析速通指南（二）"
description = "计划写到 Riemann 单值化定理。"
date = 2025-10-16
updated = 2025-10-25

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析", "复分析"]
+++

## Laurent 级数
### 定义
在实际中，有些函数会有奇点，我们使用 Laurent 级数来研究奇点附近的性质。

我们将它定义为

$$\sum_{-\infty}^{+\infty} a_n (z-z_0)^n$$

其中 $n\leq -1$ 部分称为**主部**，$n\geq 0$ 部分称为**正则部分**。

在较好的情形下，Laurent 级数在一个圆环形区域 $D(z_0, r, R)$ 上收敛（两个幂级数分别收敛）。

对圆环形区域上的解析函数，令 $r<r'<|z-z_0|<R'<R$，使用 Cauchy 公式，有：

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = R'}\frac{f(w)}{w-z}\mathrm{d}w - \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = r'}\frac{f(w)}{w-z}\mathrm{d}w$$

其中减号左边部分可以写成正则部分，从而是 $D(0, R)$ 上的解析函数，右边部分则可写成主部。

### 孤立奇点的分类
若 $z_0$ 使得 $f$ 在它处未定义，而在它的一个去心邻域上解析，则称 $z_0$ 是它的**孤立奇点**。

例如，对 $f(z) = \frac{1}{\sin \frac{1}{z}}$，有 $\frac{1}{k\pi}$ 与 $\infty$ 是它的孤立奇点，但 $0$ 不是。

{% admonition(type="abstract", title="孤立奇点分类") %}
如果存在 $g$ 补上 $z_0$ 处的值，使得它在 $z_0$ 的邻域上解析，则称 $f$ 可以解析开拓到 $z_0$，且 $z_0$ 称为它的**可去奇点**。

否则，若 $\frac{1}{f(z)}$ 可解析开拓到 $z_0$，则称 $z_0$ 为它的**极点**。

其余情况称 $z_0$ 为**本性奇点**。
{% end %}

我们有以下结论：

{% admonition(type="info", title="孤立奇点") %}
以下条件等价：
1. $z_0$ 是可去奇点。
2. $\lim_{z\to z_0} f(z)$ 在 $\mathbb{C}$ 上存在。
3. $f(z)$ 在 $z_0$ 邻域上有界。
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部为 $0$.
{% end %}

我们来证明不平凡的 (3) => (4) 如下：

设展式为 $f(z) = \sum_{-\infty}^{+\infty} a_n (z-z_0)^n$，取 $\varepsilon$ 则

$$a_n = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = \varepsilon}\frac{f(w)}{(w-z_0)^{n+1}}\mathrm{d}w$$

且设 $f$ 在 $z_0$ 的（去心）邻域上有界 $M$，则有 $a_{-n}\leq M\cdot\varepsilon^n$

令 $\varepsilon\to 0$ 知主部为 $0$.

{% admonition(type="info", title="极点") %}
以下条件等价：
1. $z_0$ 是极点。
2. $z_0$ 是 $\frac{1}{f(z)}$ 的零点。
3. $\lim_{z\to z_0} f(z) = \infty$.
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有且仅有有限项非 $0$.
{% end %}

证明不平凡的 (3) => (4) 如下：

设 $z_0$ 是 $\frac{1}{f(z)}$ 的 $m$ 阶零点，在邻域内展为 $(z-z_0)^m g(z)$，再由 $\frac{1}{g(z)}$ 解析即可。

{% admonition(type="info", title="本性奇点") %}
以下条件等价：
1. $z_0$ 是本性奇点。
2. $\lim_{z\to z_0} f(z)$ 在 $\bar{\mathbb{C}}$ 中不存在。
3. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有无穷多项非 $0$.
{% end %}

这是根据前两个结论得到的。

在 <https://complex-analysis.com/content/classification_of_singularities.html> 你可以看到很多与奇点有关的函数的彩色绘制，你也可以阅读 [着色器（二）](/posts/shader-p2/) 中对应的内容。

{% admonition(type="abstract", title="Weierstrass 定理") %}
若 $z_0$ 是 $f$ 的一个本性奇点，则对任意 $\varepsilon>0$，$f(\mathring{D}(z_0, \varepsilon))$ 在 $\mathbb{C}$ 中稠密。
{% end %}

我们知道 $\mathbb{C}-\overline{f(D_0(z_0, \varepsilon))}$ 是开集，若它非空，存在 $z^*$ 及 $\delta>0$ 使 $D(z^*, \delta)$ 在其内。

令 $g(z) = \frac{1}{f(z)-z^*}$，有 $|g(z)|\leq \frac{1}{\delta}$，从而 $z_0$ 是 $g$ 的可去奇点，只能是 $f$ 的可去奇点或极点，与条件矛盾。

更进一步，我们有 Picard 大定理：
{% admonition(type="abstract", title="Picard 大定理") %}
$z_0$ 是 $f$ 的一个本性奇点，且 $f$ 在 $\mathring{D}(z_0, \varepsilon)$ 上解析，则对任意 $0<\varepsilon<R$，集合 $\mathbb{C}-f(\mathring{D}(z_0, \varepsilon))$ 至多包含一个点。
{% end %}

甚至还有更强的结论 Julia 定理。

{% admonition(type="abstract", title="全纯自同胚") %}
$f:\mathbb{C}\to\mathbb{C}$ 是全纯自同胚当且仅当它形如 $az+b\ (a\neq 0)$.
{% end %}

只需证左推右。我们知道 $f$ 是整函数，且 $\infty$ 是孤立奇点。

由 $f$ 无界知 $\infty$ 不是可去奇点，又由 Weierstrass 定理知它不是本性奇点，从而是极点。

从而它是多项式，由代数学基本定理知是一次的。

### 亚纯函数
对 $\bar{\mathbb{C}}$ 中区域 $\Omega$ 上的函数 $f$，若除了（可能）有极点外处处解析，则称它是**亚纯**的。

一个区域上的亚纯函数全体构成一个域，记作 $m(\Omega)$.

{{ todo() }}

## 留数
{{ todo() }}
