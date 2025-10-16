+++
title = "【草稿】复分析速通指南（二）"
description = "计划写到 Riemann 单值化定理。"
date = 2025-10-16
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

对可去奇点，我们有结论：

{% admonition(type="info", title="孤立奇点") %}
以下条件等价：
1. $z_0$ 是可去奇点。
2. $\lim_{z\to z_0} f(z)$ 在 $\mathbb{C}$ 上存在。
3. $f(z)$ 在 $z_0$ 邻域上有界。
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部为 $0$.
{% end %}

对极点，我们有结论：
{% admonition(type="info", title="极点") %}
以下条件等价：
1. $z_0$ 是极点。
2. $z_0$ 是 $\frac{1}{f(z)}$ 的零点。
3. $\lim_{z\to z_0} f(z) = \infty$.
4. $f(z)$ 在 $z_0$ 的 Laurent 展式的主部有且仅有有限项非 $0$.
{% end %}

{{ todo() }}

## 留数
{{ todo() }}
