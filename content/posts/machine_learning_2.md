+++
title = "【草稿】机器学习（二）"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["计算机"]
+++

## 分类
### 数学理解
与回归不同的是，分类的 label 是“类型”（category），这里用整数指代：

$$[c] = \set{1, \dots, c}$$

分类误差（classification error）是：

$$\min_\theta \frac 1 n \sum_{i=1}^n l_{\text{0-1}}(h_\theta(x_i), y_i)$$

这里 $l_{\text{0-1}}(y, y') = 1 - \delta_{y, y'}$，但这个损失函数不适合计算，因此通常使用一些代理损失函数（surrogate loss）$l$，要求的是 $l$ 光滑及 $l$ 小推出 $l_{\text{0-1}}$ 小。

### 概率模型
概率模型考虑的是软标签（soft label），即每个 $p(x)$ 在 $c-1$ 维单形中：

$$p(x) = (p_1(x), \dots, p_n(x)) \in \Delta^{c-1}$$

$$y \sim \mathrm{Categorical}(p(x))$$

我们需要选定一个将“逻辑值”（logits）$f_\theta$ 转为 $F_\theta$ 的函数。

$$
F_\theta: \mathcal X \to \Delta^{c-1} \\\\
f_\theta: \mathcal X \to \R^c
$$

最常见的选择是 softmax，很多分类任务是比较有确定性的，指数强调了这一点：

$$\mathrm{softmax}(z) = \left(\frac{e^{z_1 / T}}{\sum e^{z_i / T}}, \dots, \frac{e^{z_c / T}}{\sum e^{z_i / T}}\right)$$

我们的优化目标就是：

$$\min_\theta \frac 1 n \sum_{i=1}^n d(p(x_i), F_\theta(x_i))$$

记 one-hot label $e_y = (0, \dots, 0, 1, 0, \dots, 0)$，我们实际上的优化目标是：

$$\min_\theta \frac 1 n \sum_{i=1}^n d(e_{y_i}, F_\theta(x_i))$$

就像回归中的标签也有噪声，只不过无偏一样，用 $e_{y_i}$ 没有什么问题。有时采取 label smoothing $(\delta, \dots, \delta, 1 - (c-1) \delta, \delta, \dots, \delta)$.

这里的 $d$ 是“散度”（divergence），并不完全是距离。常用的是 KL 散度：

$$d_{\text{KL}}(p, q) = \sum p_i \log \frac{p_i}{q_i}$$

使用此，优化目标就是：

$$\min -\frac 1 n \sum_{i=1}^n \left(\sum_{j=1}^c y_{i, j} \log F_j(x_i; \theta)\right)$$

这实际上就是极大似然估计（Maximize Likelihood Estimation, MLE）。

现在热门的 Next-token prediction (NTP) 本质上也就是此：

$$\max_\theta \sum_{j=1}^n \sum_{i=1}^l \log F_{x_{i+1}^{(j)}}(x_{\leq i}^{(j)}; \theta)$$

### 几何模型：支持向量机
想象空间中有两团数据，我们用一个超平面把它们分开。超平面即：

$$H_\theta = \set{x | w^\top x + b = 0}$$

除了将两团分开外，还有一个直观是：扰动后数据仍不容易越过超平面。因此考虑最大化 $\min d(x_i, H_\theta)$（不去最大化 $\frac 1 n \sum d(x_i, H_\theta)$，因为这对于不均衡的数据等情况结果不太合理）。

但这个最大化不好求解

support vector $(\hat w x_i + \hat b) y_i = 1$
