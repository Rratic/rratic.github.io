+++
title = "数学分析Ⅲ期中复习笔记"
draft = true

[extra]
math = true

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

{% <definition title="广义重积分"> %}
“内闭可积”即在任意可求面积有界闭子集上可积。在此及任意 $B(0, R) \cap D$ 可求面积的基础上，如果对 $D$ 的任意穷竭列 $\set{D_n}$ 都有右式存在且唯一，则定义：

$$\iint_D f(\mathbf x) \mathrm d\sigma \coloneqq \lim_{n \to \infty} \iint_{D_n} f(\mathbf x) \mathrm d\sigma$$
{% </definition> %}

任意 $B(0, R) \cap D$ 可求面积即有界范围内边界集零体积。绝对收敛的定义易见。

注意这和[之前的一元函数广义积分定义](@/posts/analysis_2_midterm.md)不同，有以下结论：

{% <theorem> %}
在前一定义条件下，广义重积分收敛当且仅当其绝对收敛。
{% </theorem> %}

证明略。

在前一定义条件下，若 $f(x, y) \geq 0$，则广义重积分收敛当且仅当存在一个穷竭列对应极限存在。且存在时二者相等。

累次广义积分和广义重积分的关系略去。

## 曲线与曲面积分
### 曲线积分
回忆可以用折线逼近的方法定义参数曲线的长度，有限时它是可求长曲线。可以证明参数曲线可求长当且仅当对每个分量是有界变差函数；函数是有界变差函数当且仅当可以写成两个单调递增函数之差。

对可求长参数曲线及有界实值函数，仿照 Riemann 和容易定义Ⅰ型曲线积分：

$$\int_{\Gamma} f(\mathbf x) \mathrm ds = \lim_{\lVert\Delta\rVert \to 0} \sum_{i=1}^n f(\gamma(\xi_i)) \Delta s_i$$

两端点重合时，也记作：

$$\oint_{\Gamma} f(\mathbf x) \mathrm ds$$

分段光滑曲线是可求长的。对可求长简单曲线 $\Gamma$ 及 $f \in C(\Gamma)$ 有曲线积分存在。

对向量场 $\mathbf F$，定义Ⅱ型曲线积分是：

$$\int_\gamma \mathbf F \mathrm ds = \int_\gamma f_1 \mathrm dx_1 + \cdots + f_1 \mathrm dx_n$$

对于分段 $C^1$ 曲线，可以转化为 Riemann 积分：

$$\int_\gamma \mathbf F \mathrm ds = \int_\alpha^\beta \mathbf F(\gamma(t)) \cdot \gamma'(t) \mathrm dt$$

### 曲面积分
对于 $\varphi: \Omega \to \R^n$ 为 $C^1$ 的参数曲面，$f$ 在 $\varphi(\Omega)$ 上连续，定义Ⅰ型曲面积分：

$$\int_{\varphi(\Omega)} f \mathrm d\sigma = \iint \cdots \int_\Omega f \circ \varphi |\det (J\varphi)| \mathrm du_1 \cdots \mathrm du_n$$

对于 $\mathbb E^3$ 中的可定向曲面 $\Sigma$ 及连续的单位法向量场 $\mathbf n$，定义连续向量值函数 $\mathbf X$ 的Ⅱ型曲面积分：

$$\int_\Sigma \mathbf X \mathrm d\sigma = \int_\Sigma \mathbf X \cdot \mathbf n \mathrm d\sigma$$

使用[微分形式](@/posts/smooth_manifolds_1.md)的语言它等于：

$$\int_\Sigma X_x \mathrm dy \wedge \mathrm dz + X_y \mathrm dz \wedge \mathrm dx + X_z \mathrm dx \wedge \mathrm dy$$

### 关联
{% <theorem title="散度定理"> %}
设 $\Omega$ 是 $\R^n$ 中有 $C^1$ 边界的有界开集，$\mathbf X$ 是 $\bar \Omega$ 上的 $C^1$ 向量场，$\mathbf n$ 是边界上的单位外法向量场，则：

$$\int_\Omega \mathrm{div}(\mathbf X) \mathrm d\mathbf x = \int_{\partial \Omega} \mathbf X \cdot \mathbf n \mathrm d\sigma$$
{% </theorem> %}

取 $\mathbf X = (Q, -P)$ 就可得到 Green 公式：

$$\int_\Omega \left(\frac{\partial Q}{\partial x} - \frac{\partial P}{\partial y}\right) \mathrm dx\mathrm dy = \int_{\partial \Omega} P \mathrm dx + Q \mathrm dy$$
