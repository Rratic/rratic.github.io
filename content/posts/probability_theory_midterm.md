+++
title = "《概率论》期中复习笔记"
date = 2026-11-01

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "概率论"]
+++

26秋的概率论课程期中复习。教材的材料组织形式令人不悦，这里参考 Durrett, *Probability Theory and Examples* 作一些整理。

<!-- more -->

## 初等理论
称**随机试验**的每个结果为**样本**/样本点，样本构成的集合 $\Omega$ 称为**样本空间**，称其某些子集为**事件**。在古典概率模型中，$\Omega$ 是有限集，每个可能结果等可能出现。即：

$$P(A) \coloneqq \frac{|A|}{|\Omega|}$$

几何概率模型中，$\Omega$ 是某个 $d$ 维区域，

$$P(A) \coloneqq \frac{\mathrm{Vol}(A)}{\mathrm{Vol}(\Omega)}$$

{% <example title="Buffon 投针问题"> %}
平面上一组等距的平行线，相邻直线距离 $a$，随机投一根长度 $l$ 的针，求针与平行线相交概率。
{% </example> %}

考虑的建模是：针的中点与平行线距离 $r$，针与平行线所夹锐角 $\theta$，

$$
\Omega = \set{(r, \theta) | r \in [0, a/2], \theta \in [0, \pi/2]} \\\\
A = \set{(r, \theta) | r \leq (l \sin \theta) / 2}
$$

算得 $P(A) = 2l / a\pi$；投针问题的变体可以类似地计算（如投正三角形可以考虑竖直方向上的投影）。

可以讨论此建模的合理性。但实际上几何概率模型可能产生不同的“随机”理解：

{% <example title="Bertrand 悖论"> %}
在单位圆内任意画一条弦，弦长超过 $\sqrt{3}$ 的概率是多少？

存在以下不同的答案：用弦中点确定，它只能在一个小圆内，结果为 $1 / 4$；用弦中点与圆心距离确定，结果为 $1 / 2$；用两个端点之间的弧长确定，结果为 $1 / 3$……
{% </example> %}

对于 $P(A) > 0$，我们定义在 $A$ 发生的条件下 $B$ 的**条件概率**是：

$$P(B|A) = P_A(B) \coloneqq \frac{P(AB)}{P(A)}$$

于是有全概率公式、Bayes 公式；可以定义事件的**独立**是 $P(AB) = P(A)P(B)$；定义重复独立试验。

{% <question title="习题 1.7.5"> %}
坛中有若干黑球、红球。定义操作：从坛中随机取一个球（称为“甲”），记下颜色并扔掉；然后，每次从剩下的球中随机取一个，若颜色与甲相同，则扔掉并进行下一次取球；否则放回坛中，结束操作。假设初始有 $b$ 黑球 $r$ 红球，重复操作直到没有球，求最后一个被扔掉的是黑球的概率。
{% </question> %}

记概率 $P(b, r)$ 关于两个参数对称，有：

$$P(b, r) = \sum_{j=1}^b P(b-j, r) \cdot \frac{A_b^j}{A_{b+r}^j} \cdot \frac{r}{b+r-j} + \sum_{j=1}^r P(b, r-j) \cdots$$

我们想要说明的是下式，因此可以简化只讨论是否 $j = b, j = r$，并归纳：

$$
P(b, r) = \begin{cases}
    1 & r = 0 \\\\
    0 & b = 0 \\\\
    \frac 1 2 & \text{otherwise}
\end{cases}
$$

## 概率空间
通过 Kolmogorov 公理，正式地定义**概率空间**是三元组 $(\Omega, \mathcal F, P)$，其中 $\mathcal F$ 是 $\sigma$-代数，$P: \mathcal F \to [0, 1]$ 是概率测度。

这里 $\sigma$-代数的要求是 $\Omega \in \mathcal F \subseteq \mathscr P(\Omega)$，且 $\mathcal F$ 对补、可列并封闭。在 $\R^d$ 上用 $\mathcal R^d$ 表示 Borel 集（包含所有开集的最小 $\sigma$-代数）。$(\Omega, \mathcal F)$ 上的**测度**是非负、可列可加的 $\mu: \mathcal F \to \R$，在 $\mu(\Omega) = 1$ 时称为概率测度。

我们用符号 $A_i \uparrow A$ 表示 $A_1 \sub A_2 \sub \dots$ 且 $\bigcup A_i = A$，符号 $\downarrow$ 反之。易见：

$$
A_i \uparrow A \implies P(A_i) \uparrow P(A) \\\\
A_i \downarrow A \implies P(A_i) \downarrow P(A)
$$

{% <theorem title="一维的测度"> %}
每个 Stieltjes 测度函数（不降、右连续）$F$ 对应唯一的一个 $(R, \mathcal R)$ 上的测度 $\mu((a, b]) = F(b) - F(a)$.
{% </theorem> %}

证明略。在 $F(x) = x$ 时对应的就是 Lebesgue 测度。

{% <theorem title="d 维的测度"> %}
$F: \R^d \to [0, 1]$ 满足以下条件时对应唯一概率测度 $\mu(A) = \Delta_A F$：
- 对所有分量不降
- 对所有分量右连续
- $x_n \downarrow -\infty \implies F(x_n) \downarrow 0$
- $x_n \uparrow +\infty \implies F(x_n) \uparrow 1$
- 对所有矩形 $A$ 有 $\Delta_A F \geq 0$
{% </theorem> %}

如果不要求最后一个条件则存在反例。

## 随机变量
### 随机变量与分布
$X: \Omega \to \R$ 称为**随机变量**，如果对任意 $B \in \mathcal R$ 有 $X^{-1}(B) \in \mathcal F$. 对离散概率空间所有函数都是随机变量；另一个例子是取 $A \in \mathcal F$，示性函数 $1_A$ 是随机变量。实际上，在验证时只需要考虑所有的 $B = (-\infty, a]$，还可以只考虑其中 $a \in \mathbb Q$ 不过一般不会更容易。

随机变量会诱导一个随机测度 $\mu = P \circ X^{-1}$，称为**分布**。我们用 $P(X \in A)$ 指代 $P(\set{\omega | X(\omega) \in A})$，通常用（累积）**分布函数** $F(x) = P(X \leq x)$ 刻画随机变量。相应地 $G(x) = P(X > x)$ 称为尾分布函数。

易知：随机变量的分布函数单调不降、右连续、两端分别趋向 $0, 1$；反之这样的函数也一定决定一个唯一的分布。

我们说 $X, Y$ **同分布** $X \stackrel d = Y$ 或 $X \sim Y$，如果它们诱导的分布函数相同。若 $P(\set{\omega | X(\omega) = Y(\omega)}) = 1$，则称它们几乎必然（almost surely）相等 $X = Y \text{ a.s.}$，这是概率论风格的“几乎处处（almost everywhere）”。其它的“依测度 $\mu$ 某某”亦类似此理解。

对概率分布 $Y$，称 $X$ 服从于分布 $Y$（$X \sim Y$），如果 $X$ 的分布等于 $Y$.

### 离散型与连续型
**离散型随机变量**要求 $X$ 只有可数的取值。分布的例子如下：

| 分布名称 | 记号 | $P(X = k)$ 表达式 |
| :-----: | :--: | :--------------: |
| 超几何分布[^hypergeometric] | $H(N, M, n)$ | $\binom{M}{k} \binom{N-M}{n-k} / \binom{N}{n}$ |
| 二项分布[^binomial] | $B(n, p)$ | $C_n^k p^k (1-p)^{n-k}$ |
| 几何分布[^geometric] | $G(p)$ | $(1-p)^{k-1} p$ |
| 负二项分布[^negative-binomial] | $NB(r, p)$ | $C_{r+k-1}^k (1-p)^k p^r$ |
| Poisson 分布 | $P(\lambda)$ | $\lambda^k e^{-\lambda} / k!$ |

**连续型随机变量**要求分布函数有如下形式。此时称 $X$ 有**密度函数** $f$：

$$F(x) = \int_{-\infty}^x f(t) \mathrm dt$$

$f$ 也常被更清晰地写成 $p_X(x)$. 例子如下：

| 分布名称 | 记号 | $p_X(x)$ 表达式 |
| :-----: | :--: | :--------------: |
| 均匀分布 | $U(a, b)$ | $\begin{cases} 1 / (b-a) & x \in [a, b] \\\\ 0 & \text{otherwise} \end{cases}$ |
| 指数分布 | $\mathrm{Exp}(\lambda)$ | $\begin{cases} \lambda e^{-\lambda x} & x > 0 \\\\ 0 & x \leq 0 \end{cases}$ |
| 正态分布 | $N(\mu, \sigma^2)$ | $e^{- (x - \mu)^2 / 2 \sigma^2} / \sqrt{2\pi \sigma^2}$ |
| $\Gamma$ 分布 | $\Gamma(\alpha, \lambda)$ | $\begin{cases} \frac{\lambda^\alpha}{\Gamma(\alpha)} x^{\alpha-1} e^{-\lambda x} & x > 0 \\\\ 0 & \text{otherwise} \end{cases}$ |
| Cauchy 分布[^cauchy] | $C(\mu, \alpha)$ | $\alpha / \pi((x - \mu^2)^2 + \alpha^2)$ |

当我们有 $X$ 的密度函数，对变换 $Y = f(X)$，如果 $f$ 分段严格单调且可微，则在 $Y$ 的取值范围内有密度函数：

$$p_Y(y) = \sum_{f(x_i) = y} \frac{p_X(x_i)}{|f'(x_i)|}$$

---

注意存在既不是离散型也不是连续型的随机变量。实际上可以非常奇怪：称一个分布函数奇异，如果对应的测度关于 Lebesgue 测度奇异（称两个测度奇异 $\mu \perp \lambda$，如果存在集合 $N$ 使得 $\lambda(N) = \mu(N^\complement) = 0$）。奇异分布的例子如：

{% <example title="Cantor 集上的均匀分布"> %}
考虑 Cantor 集，让 $F(x)$ 在 $[1/3, 2/3]$ 是 $1/2$；在 $[1/9, 2/9]$ 是 $1/4$，在 $[7/9, 8/9]$ 是 $3/4$；以此类推，然后用单调性定义。
{% </example> %}

### 可测映射
稍微扩展一下随机变量的定义，考虑一般的 $X: \Omega \to S$，称为可测映射。后者是 $(\R^d, \mathcal R^d)$ 时，称它是**随机向量**。后者是扩展实数系时，称为广义随机变量。

可测映射的复合是可测映射。

{% <note> %}
或许我们采取范畴看法会更舒服。
{% </note> %}

{% <example> %}
对随机变量 $X_1, \dots, X_n$，有 $X_1 + \cdots + X_n$ 是随机变量。

首先，$(X_1, \dots, X_n)$ 是随机向量。而 $f(x_1, \dots, x_n) = x_1 + \cdots + x_n$ 是可测映射，这里只需验证 $(-\infty, a)$ 的原像，而这个原像一定是开集。
{% </example> %}

同理知 $\inf X_n, \sup X_n, \liminf X_n, \limsup X_n$ 也是随机变量。

我们称 $X_n$ 几乎必然收敛，如果：

$$P(\set{\omega | \exists \lim_{n \to \infty} X_n}) = 1$$

## 随机向量
### 基本定义
对离散型随机向量我们称“联合分布列”，记作：

$$p_{ij} = P(X = i, Y = j)$$

对连续性随机向量我们称“联合密度函数”，对可测区域有：

$$P((X, Y) \in D) = \iint p_{X, Y}(x, y) \mathrm d\mu$$

二维正态分布 $(X, Y) \sim N(\mu_1, \mu_2, \sigma_1^2, \sigma_2^2, \rho)$ 的联合密度函数：

$$\frac 1 {2\pi \sigma_1 \sigma_2 \sqrt{1 - \rho^2}} \exp \left[- \frac {(x - \mu_1)^2 - 2\rho(x - \mu_1)(y - \mu_2) + (y - \mu_2)^2} {2 (1 - \rho^2) \sigma_1^2 \sigma_2^2}\right]$$

对 $\vec X = (X_1, \dots, X_n)$，对 $m < n$ 称 $\vec Y = (X_{i_1}, \dots, X_{i_m})$ 是 $\vec X$ 的**边缘**。对应分布/密度称边缘（联合）分布/密度。对两个概率分布 $\mu_1, \mu_2$，若 $\mu$ 是联合分布，以 $\mu_1, \mu_2$ 为边缘分布，则称 $\mu$ 是 $\mu_1, \mu_2$ 的耦合。

使用随机变量可以更方便地表述：称 $X_1, \dots, X_n$ **相互独立**，如果对任意 $x_1, \dots, x_n$，

$$P(X_1 \leq x_1, \dots, X_n \leq x_n) = P(X_1 \leq x_1) \cdots P(X_n \leq x_n)$$

相互独立可以推出两两独立。若对所有 $i$ 有 $X_i \stackrel d = X_1$，则称它们独立同分布，

$$X_i \stackrel {\text{iid}} \sim X_1$$

### 条件分布
对连续型随机向量，我们通过极限来给出定义：

$$F_{Y|X}(y|x) \coloneqq \lim_{\varepsilon \to 0^+} P(Y \leq y | x - \varepsilon < X \leq x + \varepsilon)$$

在涉及函数足够连续时，所谓条件密度函数是为：

$$p_{Y|X}(y|x) = \frac{p_{X, Y}(x, y)}{p_X(x)}$$

一般地定义在 $X = x$ 的条件下，$Y$ 关于 $X$ 的条件分布函数是 $F_{Y|X}(\cdot|x)$，其中在 $y$ 是间断点时用右连续补全：

$$F_{Y|X}(y|x) \coloneqq \lim_{z \to y^+} F_{Y|X}(z|x)$$

{% <question title="书例 2.6.4"> %}
在单位球面 $\mathbb S^2$ 上随机取四个点，求这四个点不共半球的概率。
{% </question> %}

设 $\xi_i \stackrel {\text{iid}} \sim U(\mathbb S^2)$，用 $\Delta_{\alpha\beta\gamma}$ 表示三条大圆上的劣弧围成的三角形。令 $A = \set{\xi_4 \in \Delta_{-\xi_1, -\xi_2, -\xi_3}}$，所求即 $P(A)$.

取与 $\xi_1, \xi_2, \xi_3$ 相互独立的随机变量 $W_1, W_2, W_3$ 是 $P(W_i = 1) = P(W_i = -1) = 1/2$，令 $\eta_i = W_i\xi_i$，$B = \set{\xi_4 \in \Delta_{\eta_1, \eta_2, \eta_3}}$，由同分布 $P(A) = P(B)$.

对固定点 $\alpha_1, \alpha_2, \alpha_3$，有：

$$P(B|\xi_1 = \alpha_1, \xi_2 = \alpha_2, \xi_3 = \alpha_3) = 1/8$$

用全概率公式，就有 $P(B) = 1/8$，即为所求。

### 变换
定义分布函数 $F: \R \to [0, 1]$ 的广义反函数：

$$
\begin{aligned}
F^{-1}: (0, 1) &\longrightarrow \R \cr
    u &\longmapsto \inf \set{x \in \R | F(x) \geq u}
\end{aligned}
$$

{% <question title="2025 P5"> %}
设 $F(x)$ 分布函数，构造随机向量 $(X, Y)$ 使边缘分布函数都是 $F(x)$，且 $\max \set{X, Y}$ 的分布函数是 $\max \set{2F(x) - 1, 0}$.
{% </question> %}

可以令 $F(x, y) = \max \set{F(x) + F(y) - 1, 0}$，更一般的做法是让 $U \sim U(0, 1)$，取：

$$X = F^{-1}(U),\\ Y = F^{-1}(1 - U)$$

---

回忆 $X + Y$ 是随机变量。对 $X, Y$ 独立的连续型随机变量，易见：

$$p_{X+Y}(z) = \int_{-\infty}^{+\infty} p_X(z-y) p_Y(y) \mathrm dy$$

对 $(\R, \mathcal R)$ 上两个分布 $\mu, \nu$，$X \sim \mu, Y \sim \nu$ 相互独立，则称 $X + Y$ 的分布为 $\mu$ 与 $\nu$ 的**卷积** $\mu \ast \nu$.

若分布族 $\\{\mu_\alpha\\}$ 满足对 $\mu_\alpha, \mu_\beta$ 存在 $\mu_\alpha \ast \mu_\beta = \mu_\gamma$，则称该分布族有可加性/再生性。例如：
- 二项分布 $B(n, p) \ast B(m, p) = B(n + m, p)$
- Poisson 分布 $P(\lambda_1) \ast P(\lambda_2) = P(\lambda_1 + \lambda_2)$
- 正态分布 $N(\mu_1, \sigma_1^2) + N(\mu_2, \sigma_2^2) = N(\mu_1 + \mu_2, \sigma_1^2 + \sigma_2^2)$
- $\Gamma$ 分布 $\Gamma(\alpha_1, \lambda) \ast \Gamma(\alpha_2, \lambda) = \Gamma(\alpha_1 + \alpha_2, \lambda)$

---

Borel 函数是指 $\R^n \to \R^m$ 的可测映射。对于 $n$ 维随机向量 $\vec X \sim \vec Y$ 与 Borel 函数 $f$，有 $f(\vec X) \sim f(\vec Y)$.

一个常见的问题是对 $\vec Y = f(\vec X) \in \R^n$，用 $\vec X$ 的联合密度求 $Y$ 的联合密度。若某个邻域上 $f: D \to D'$ 一一且 Jacobi 行列式处处非零，则对 $y \in D'$，

$$p_{\vec Y}(y) = p_{\vec X}(f^{-1}(y)) \cdot |\det J(f^{-1})|$$

若 $f: D \to D'$ 是多对一的，则只需要把各个反函数对应的结果加起来。

对一般的 $\vec Y \in \R^m$，其中 $m < n$ 时，可以通过补全到 $n$ 维然后取边缘分布来计算。

{% <example title="顺序统计量"> %}
设 $X_i$ 独立同分布，$X_1(\omega), \dots, X_n(\omega)$ 从小到大排列得到 $X_{(1)}(\omega) \leq \cdots \leq X_{(n)}(\omega)$，求 $(X_{(1)}, \dots, X_{(n)})$ 的联合密度函数。

分解 $\set{X_{(i)} \leq x_i}$ 为 $\set{X_{(i)} \leq x_{i-1}} \cup \set{x_{i-1} < X_{(i)} \leq x_i}$，讨论可知：

$$p(\vec x) = n! p(x_1) \cdots p(x_n) \mathbf 1_{\set{x_1 < \cdots < x_n}}$$
{% </example> %}

### 随机变量序列
我们还可以考虑无穷维的随机向量。于是可以考虑一族随机变量 $\mathbf X = \set{X_\alpha | \alpha \in I}$，在 $I \subseteq \R$ 时称为**随机过程**。

设 $\xi_1, \dots$ 独立同分布，则称 $\set{S_n | n \geq 0}$ 为**随机游走**。

{% <example title="Poisson 过程"> %}
设 $\xi_i \sim \mathrm{Exp}(\lambda)$，令 $X_t = \\#\set{n \geq 1 | S_n \leq t}$，则 $X_t \sim P(\lambda t)$.
{% </example> %}

## 数学期望
对于 $X \geq 0$ 的随机变量，我们定义其**期望**是：

$$\mathbb EX = \int X \mathrm dP$$

对于一般的情形，我们让 $x^+ = \max\set{x, 0}$ 正部，$x^- = \max\set{-x, 0}$ 负部，当 $\mathbb EX^+$ 与 $\mathbb EX^-$ 之一有限时，可以让：

$$\mathbb EX = \mathbb EX^+ - \mathbb EX^-$$

称 $\mathbb EX^k$ 为 $X$ 的 $k$ 阶矩，$\mathbb E(X - \mathbb EX)^k$ 为 $X$ 的 $k$ 阶中心矩。

{% <theorem title="Markov 不等式"> %}
非负随机变量 $X$，有：

$$P(X \geq \varepsilon) \leq \frac{\mathbb EX}{\varepsilon} \quad (\varepsilon > 0)$$
{% </theorem> %}

---

[^hypergeometric]: $N$ 个产品中 $N$ 个次品，随机选出 $n$ 个，次品数 $X$.
[^binomial]: 硬币正面概率 $p$，抛 $n$ 次正面数 $X$.
[^geometric]: 硬币正面概率 $p > 0$，首次正面时抛的次数 $X$.
[^negative-binomial]: 硬币正面概率 $p > 0$，$r$ 此正面时抛过反面次数 $X$.
[^cauchy]: 对于独立的 $X \sim N(\mu, \sigma_1^2)$，$Y \sim N(\mu, \sigma_2^2)$，有 $X/Y \sim C(\mu, \sigma_1/\sigma_2)$.
