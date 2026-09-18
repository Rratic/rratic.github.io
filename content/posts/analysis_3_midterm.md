+++
title = "数学分析Ⅲ期中复习笔记"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "分析学"]
+++

这半学期的内容是多元函数积分学。我们还是用 Riemann 积分的引申来做，出于与“测度”的区分及教师的习惯，这里的用词是“体积”。

<!-- more -->

## 多元函数积分学
### 重积分
对 $\R^n$ 的非空有界集 $E$，称（面平行于坐标面的）长方体集 $\mathcal A_E = \\{A_k\\}$ 是其恰当覆盖，如果 $\circ A_i \cap \circ A_j = \emptyset$ 且是覆盖。

令 $m(\mathcal A_E)$ 是 $\mathcal A_E$ 中完全含于 $E$ 的长方体体积之和，$M(\mathcal A_E)$ 是与 $E$ 有交的长方体体积之和；

$$m(E) = \sup m(\mathcal A_E) \\\\ M(E) = \inf M(\mathcal A_E)$$

{% <definition title="体积"> %}
有界集合 $E$ 在 $m(E) = M(E)$ 时称它可求体积，此时这个值定义为它的体积，记作 $V(E)$.
{% </definition> %}

易证 $E \subset \R^n$ 可求体积当且仅当 $V(\partial E) = 0$，其推论是，有界集合 $E$ 可求体积当且仅当 $\bar E$ 可求体积，且此时体积相等。

{% <definition title="重积分"> %}
设 $f$ 是 $\R^n$ 中可求体积有界闭区域 $D$ 上函数，称 $\Delta = \set{\Delta D_1, \dots, \Delta D_k}$ 为一个分割，若 $\Delta D_i$ 均是可求体积闭子集，两两交集体积零，且并是 $D$ 整体。

若存在常数 $I$，使得对任意 $\varepsilon > 0$ 存在 $\delta > 0$，对任意分割 $\Delta$ 及取点 $\xi_i \in \Delta D_i$ 在 $\max \operatorname{diam} \Delta D_i$，使得：

$$\left|\sum f(\xi_i)V(\Delta D_i) - I\right| < \varepsilon$$

则称 $I$ 为 $f$ 在 $D$ 上的 $n$ 重积分，记作：

$$\underbrace{\iint \cdots \int_D}_n f(\mathbf x) \mathrm d\sigma$$
{% </definition> %}

$\mathrm dx_1 \cdots \mathrm dx_n$ 是 $\mathrm d\sigma$ 的一种记法。

类似于一维的 Riemann 和，我们可以用 Darboux 和来分析，给出 Lebesgue 定理：$f$ 在可求体积的有界闭区域 $D$ 上有界，则 $f \in R(D)$ 当且仅当其间断点集体积为零。

{% <theorem title="转化为累次积分"> %}
设 $f(x, y) \in R([a, b] \times [c, d])$，若对任意 $x \in [a, b]$ 有 $\int_c^d f(x, y) \mathrm dy$ 存在，则：

$$\int_a^b \mathrm dx \int_c^d f(x, y) \mathrm dy = \iint_{[a, b] \times [c, d]} f(x, y) \mathrm d\sigma$$
{% </theorem> %}

更多重积分同理。若定义域不规则，可以补成一个矩形区域并令未定义区域值为零。

{% <theorem title="变量替换"> %}
设 $E$ 可求体积有界闭区域，$f \in R(E)$，$\mathbf y = \varphi(\mathbf x)$ 是 $C^1$ 同胚，其 Jacobi 矩阵 $J = \frac{\partial(y_1, \dots, y_n)}{\partial(x_1, \dots, x_n)}$，则：

$$\int_E f(\varphi(\mathbf x)) |\det J| \mathrm dx = \int_{\varphi(E)} f(\mathbf y) \mathrm dy$$
{% </theorem> %}

通过琐碎的分析。如，

$$\int_0^{2\pi} \mathrm d\theta \int_0^R f(r\cos\theta, r\sin\theta) r \mathrm dr = \iint_{x^2+y^2 \leq R^2} f(x, y) \mathrm d\sigma$$

### 广义重积分
称一列有界可求面积闭集 $\\{D_n\\}_{n=1}^\infty$ 为闭集 $D$ 的穷竭，如果 $D_1 \subset D_2 \subset \cdots \subset D$，且任意有界闭集 $F \subset D$ 存在 $D_i \supset F$.
