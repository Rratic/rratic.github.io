+++
title = "《概率论》期末复习笔记"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "概率论"]
+++

## 数学期望
### 定义
对于 $X \geq 0$ 的随机变量，我们定义其**期望**是：

$$\mathbb EX = \int X \mathrm dP$$

对于一般的情形，我们让 $x^+ = \max\set{x, 0}$ 正部，$x^- = \max\set{-x, 0}$ 负部，当 $\mathbb EX^+$ 与 $\mathbb EX^-$ 之一有限时，可以让：

$$\mathbb EX = \mathbb EX^+ - \mathbb EX^-$$

称 $\mathbb EX^k$ 为 $X$ 的 $k$ 阶矩，$\mathbb E(X - \mathbb EX)^k$ 为 $X$ 的 $k$ 阶中心矩。

易见期望具有线性性，且对 $X, Y$ 独立且 $\mathbb EX, \mathbb EY$ 存在，有：

$$\mathbb E(XY) = (\mathbb EX) (\mathbb EY)$$

{% <theorem title="Markov 不等式"> %}
非负随机变量 $X$，有：

$$P(X \geq \varepsilon) \leq \frac{\mathbb EX}{\varepsilon} \quad (\varepsilon > 0)$$
{% </theorem> %}

{% <theorem title="Jensen 不等式"> %}
设 $n$ 维随机向量 $\vec X$，凸函数 $f: \R^n \to \R$，$\mathbb E\vec X$ 与 $\mathbb E f(\vec X)$ 存在且有限，则：

$$f(\mathbb E\vec X) \leq \mathbb Ef(\vec X)$$
{% </theorem> %}

### 条件期望
条件期望记作 $\mathbb E(Y|X)$ 或 $\mathbb E_X(Y)$，对于离散型或连续型随机变量 $(X, Y)$，按如下定义的 $\varphi$ 给出的 $\varphi(X)$，为随机变量：

$$
\varphi(x_i) \coloneqq \mathbb E(Y | X=x_i) = \sum_j y_j P(Y=y_j | X=x_i) \\\\
\varphi(x_i) \coloneqq \int_{-\infty}^{\infty} y p_{Y|X}(y|x) \mathrm dy
$$

条件期望有 $X, Y$ 独立时 $\mathbb E(Y|X) = \mathbb EY \text{ a.s.}$、线性性、$\mathbb E(f(X) \cdot Y|X) = f(X) \cdot \mathbb E(Y|X)$、重期望公式（$\mathbb EY$ 存在且有限）：

$$\mathbb EY = \mathbb E\mathbb E(Y|X)$$

### 方差
**方差**即二阶中心矩，记作 $\mathrm{Var}(X)$，其平方根记作 $\sigma_X$，称为**标准差**。我们有：

$$\mathrm{Var}(X) = \mathbb EX^2 - (\mathbb EX)^2$$

| 分布 | 方差 |
| :--: | :--: |
| $B(n, p)$ | $np(1 - p)$ |
| $P(\lambda)$ | $\lambda$ |
| $U(a, b)$ | $(b - a)^2 / 12$ |
| $\mathrm{Exp}(\lambda)$ | $1/\lambda^2$ |
| $N(\mu, \sigma^2)$ | $\sigma^2$ |

设有 $n$ 个事件 $A_1, \dots, A_n$，$X_i = \mathbf 1_{A_i}$，$X = X_1 + \cdots + X_n$，则有下式（可用于随机图中的方差计算等）：

$$\mathrm{Var}(X) = \sum_i p_i(1 - p_i) + \sum_{i \neq j} (p_{ij} - p_ip_j)$$

{% <theorem title="Chebyshev 不等式"> %}
设 $\mathbb EX^2$ 有限，对 $\varepsilon > 0$ 有：

$$P(|X - \mathbb EX| \geq \varepsilon) \leq \frac{\mathrm{Var}(X)}{\varepsilon^2}$$
{% </theorem> %}

这可以由 Markov 不等式给出。

### 协方差
对于 $\mathbb EX^2, \mathbb EY^2$ 存在且有限，定义 $X, Y$ 的**协方差**（也记作 $\sigma_{XY}$）：

$$\mathrm{Cov}(X, Y) \coloneqq \mathbb E(X - \mathbb EX)(Y - \mathbb EY) = \mathbb E(XY) - (\mathbb EX)(\mathbb EY)$$

协方差为零时，称 $X, Y$ 不相关；大于等于零时称正相关，反之称负相关。

对 $n$ 个随机变量，还可以定义协方差矩阵，记作 $\Sigma$.

若 $X, Y$ 的方差非零且有限，定义其相关系数是：

$$\rho_{XY} = \frac{\mathrm{Cov}(X, Y)}{\sqrt{\mathrm{Var}(X) \cdot \mathrm{Var}(Y)}}$$
