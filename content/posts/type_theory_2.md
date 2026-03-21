+++
title = "【类型论】同伦类型论"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "类型论"]
+++

回顾在代数拓扑中，我们说道路 $f, g: X_1 \to X_2$ 之间的同伦是连续映射：

$$H: X_1 \times [0, 1] \to X_2$$

其中 $H(x, 0) = f(x), H(x, 1) = g(x)$. 我们也称 $H$ 是道路之间的道路。

同理我们也可以说 homotopy between homotopies, homotopy between homotopies between homotopies 之类的东西，这些所有东西构成的是一个 (weak) $\infty$-groupoid.

## 类型是高阶广群
先来证明一些与道路有关的基本结论：

{% admonition(type="theorem", title="引理 1.1") %}
对类型 $A$ 及 $x, y: A$ 存在函数 $(x = y) \to (y = x)$.
{% end %}

也就是实现一个类型：

$$\prod_{(A: \mathcal{U})} \prod_{(x, y: A)} (x = y) \to (y = x)$$
