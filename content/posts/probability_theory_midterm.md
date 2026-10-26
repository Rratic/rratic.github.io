+++
title = "概率论期中复习笔记"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "概率论"]
+++

基于课程安排，参考 Durrett, *Probability Theory and Examples* 加入一些测度论风格。

古典概率模型和几何概率模型略去。

## 基本定义
### 概率空间
回忆**概率空间**是三元组 $(\Omega, \mathcal F, P)$，其中 $\mathcal F$ 是 $\sigma$-代数，$P: \mathcal F \to [0, 1]$ 是概率测度。这里 $(\Omega, \mathcal F)$ 上的**测度**是非负、可数可加的 $\mu: \mathcal F \to \R$，在 $\mu(\Omega) = 1$ 时称为概率测度。

我们用符号 $A_i \uparrow A$ 表示 $A_1 \sub A_2 \sub \dots$ 且 $\bigcup A_i = A$，符号 $\downarrow$ 反之。易见 $A_i \uparrow A \implies \mu(A_i) \uparrow \mu(A)$.

在 $\R^d$ 上用 $\mathcal R^d$ 表示 Borel 集（包含所有开集的最小 $\sigma$-field）。

{% <theorem title="一维的测度"> %}
每个 Stieltjes 测度函数（不降、右连续）$F$ 对应唯一的一个 $(R, \mathcal R)$ 上的测度 $\mu((a, b]) = F(b) - F(a)$.
{% </theorem> %}

证明略。在 $F(x) = x$ 时对应的就是 Lebesgue 测度。

{% <theorem title="d 维的测度"> %}
$F: \R^d \to [0, 1]$ 满足以下 4 个条件时对应唯一概率测度 $\mu(A) = \Delta_A F$：
- 对所有分量不降
- 对所有分量右连续
- $x_n \downarrow -\infty \implies F(x_n) \downarrow 0$ 及 $x_n \uparrow +\infty \implies F(x_n) \uparrow 1$
- 对所有矩形 $A$ 有 $\Delta_A F \geq 0$
{% </theorem> %}

### 随机变量
$X: \Omega \to \R$ 称为**随机变量**，如果对任意 $B \in \mathcal R$ 有 $X^{-1}(B) \in \mathcal F$. 对离散概率空间所有函数都是随机变量；另一个例子是取 $A \in \mathcal F$，示性函数 $1_A$ 是随机变量。

随机变量会诱导一个随机测度 $\mu = P \circ X^{-1}$，称为**分布**。我们用 $P(X \in A)$ 指代 $P(\set{\omega | X(\omega) \in A})$，通常用（累积）**分布函数** $F(x) = P(X \leq x)$ 刻画随机变量。不降、右连续、两端分别趋向 $0, 1$ 的（累积）分布函数一定对应某个随机变量。相应地 $G(x) = P(X > x)$ 称为尾分布函数。

我们说 $X, Y$ **同分布** $X \stackrel d = Y$，如果它们诱导的分布函数相同。若 $P(\set{\omega | X(\omega) = Y(\omega)}) = 1$，则称它们几乎必然相等 $X = Y \text{ a.s.}$，这是概率论风格的“几乎处处”。

当（累积）分布函数有如下形式时，我们称 $X$ 有**密度函数** $f$：

$$F(x) = \int_{-\infty}^x f(t) \mathrm dt$$

| 分布名称 | 记号 | $P(X = k)$ 表达式 |
| :-----: | :--: | :--------------: |
| 超几何分布 | $H(N, M, n)$ | $\binom{M}{k} \binom{N-M}{n-k} / \binom{N}{n}$ |
| 二项分布 | $B(n, p)$ | $C_n^k p^k (1-p)^{n-k}$ |
| 几何分布 | $G(p)$ | $(1-p)^{k-1} p$ |
| 负二项分布 | $NB(r, p)$ | $C_{r+k-1}^k (1-p)^k p^r$ |
| 泊松分布 | $P(\lambda)$ | $\lambda^k e^{-\lambda} / k!$ |

| 分布名称 | 记号 | $f(x)$ 表达式 |
| :-----: | :--: | :--------------: |
| 指数分布 | $\mathrm{Exp}(\lambda)$ | $\begin{cases} \lambda e^{-\lambda x} & x > 0 \\\\ 0 & x \leq 0 \end{cases}$ |
| 正态分布 | $N(\mu, \sigma^2)$ | $e^{- (x - \mu)^2 / 2 \sigma^2} / \sqrt{2\pi \sigma^2}$ |

当我们有 $X$ 的密度函数，对 $Y = f(X)$，如果 $f$ 分段严格单调且可微，则在 $Y$ 的取值范围内有密度函数：

$$p_Y(y) = \sum_{f(x_i) = y} \frac{p_X(x_i)}{|f'(x_i)|}$$

一个概率测度 $P$ 称为**离散**的，如果存在一个可数集 $S$ 使得 $P(S^\complement) = 0$.

称一个分布函数绝对连续，如果它有密度；称奇异，如果对应的测度关于 Lebesgue 测度奇异（称两个测度奇异 $\mu \perp \lambda$，如果存在集合 $N$ 使得 $\lambda(N) = \mu(N^\complement) = 0$）。奇异分布的例子如：

{% <example title="Cantor 集上的均匀分布"> %}
考虑 Cantor 集，让 $F(x)$ 在 $[1/3, 2/3]$ 是 $1/2$；在 $[1/9, 2/9]$ 是 $1/4$，在 $[7/9, 8/9]$ 是 $3/4$；以此类推，然后用单调性定义。
{% </example> %}

### 随机向量
稍微扩展一下随机变量的定义，考虑一般的 $X: \Omega \to S$，称为可测映射。后者是 $(\R^d, \mathcal R^d)$ 时，称它是**随机向量**。后者是扩展实数系时，称为广义随机变量。

可测映射的复合是可测映射。

{% <note> %}
或许我们采取范畴看法会更舒服。
{% </note> %}

{% <theorem> %}
对随机变量 $X_1, \dots, X_n$，有 $X_1 + \cdots + X_n$ 是随机变量。
{% </theorem> %}

首先，$(X_1, \dots, X_n)$ 是随机向量。而 $f(x_1, \dots, x_n) = x_1 + \cdots + x_n$ 是可测映射，这里只需验证 $(-\infty, a)$ 的原像，而这个原像一定是开集。

同理知 $\inf X_n, \sup X_n, \liminf X_n, \limsup X_n$ 也是随机变量。我们称 $X_n$ 几乎必然收敛，如果：

$$P(\set{\omega | \exists \lim_{n \to \infty} X_n}) = 1$$

我们称联合分布列是指 $p_{ij} = P(X = i, Y = j)$，联合密度函数等定义同理。对可测区域就有：

$$P((X, Y) \in D) = \iint p_{X, Y}(x, y) \mathrm d\mu$$

二维正态分布 $(X, Y) \sim N(\mu_1, \mu_2, \sigma_1^2, \sigma_2^2, \rho)$ 的联合密度函数：

$$\frac 1 {2\pi \sigma_1 \sigma_2 \sqrt{1 - \rho^2}} \exp \left[- \frac {(x - \mu_1)^2 - 2\rho(x - \mu_1)(y - \mu_2) + (y - \mu_2)^2} {2 (1 - \rho^2) \sigma_1^2 \sigma_2^2}\right]$$

### 独立性
对 $\vec X = (X_1, \dots, X_n)$，对 $m < n$ 称 $\vec Y = (X_{i_1}, \dots, X_{i_m})$ 是 $\vec X$ 的**边缘**。对应分布/密度称边缘（联合）分布/密度。

称 $X_1, \dots, X_n$ **相互独立**，如果对任意 $x_1, \dots, x_n$，

$$P(X_1 \leq x_1, \dots, X_n \leq x_n) = P(X_1 \leq x_1) \cdots P(X_n \leq x_n)$$

相互独立可以推出两两独立。若对所有 $i$ 有 $X_i \stackrel d = X_1$，则称它们独立同分布。

对两个概率分布 $\mu_1, \mu_2$，若 $\mu$ 是联合分布，以 $\mu_1, \mu_2$ 为边缘分布，则称 $\mu$ 是 $\mu_1, \mu_2$ 的耦合。

## 琐碎定义
所谓条件密度函数是为：

$$p_{Y|X}(y|x) = \frac{p_{X, Y}(x, y)}{p_X(x)}$$

定义分布函数 $F$ 的广义反函数：

$$
\begin{aligned}
F^{-1}: (0, 1) &\longrightarrow \R \cr
    u &\longmapsto \inf \set{x \in \R | F(x) \geq u}
\end{aligned}
$$

设 $(\R, \mathcal R)$ 上两个分布 $\mu, \nu$，取 $X \sim \mu, Y \sim \nu$ 相互独立，则：

$$X + Y \sim \mu \ast \nu = \int_{-\infty}^{+\infty} \mu(t) \nu(z - t) \mathrm dt$$

这里 $\ast$ 是卷积；若分布族满足对 $\mu_\alpha, \mu_\beta$ 存在 $\mu_\alpha \ast \mu_\beta = \mu_\gamma$，则称该分布族有可加性/再生性。

随机向量的变换

顺序统计量

随机变量序列
