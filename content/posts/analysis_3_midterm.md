+++
title = "数学分析Ⅲ期中复习笔记"
date = 2026-11-09

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "分析学"]
+++

这半学期的内容是多元函数积分学。我们还是用 Riemann 积分的引申来做，出于与 Lebesgue 测度的区分及教师的习惯，这里的用词是“体积”，实际上是 Jordan 测度。

<!-- more -->

## 多元函数积分学
### 重积分
对 $\R^n$ 的非空有界集 $E$，称（面平行于坐标面的）长方体集 $\mathcal A_E = \\{A_k\\}$ 是其恰当覆盖，如果 $\mathring A_i \cap \mathring A_j = \emptyset$ 且是覆盖。

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
设 $E$ 可求体积有界闭区域，$f \in R(E)$，$\mathbf y = \varphi(\mathbf x)$ 是 $C^1$ 同胚，其 Jacobi 矩阵 $J_{ij} = \partial y_i / \partial x_j$，则：

$$\int_E f(\varphi(\mathbf x)) |\det J\varphi| \mathrm d\mathbf x = \int_{\varphi(E)} f(\mathbf y) \mathrm d\mathbf y$$
{% </theorem> %}

先证明对初等线性变换成立，从而对仿射变换成立，然后分析。典型的例子是：

$$\int_0^{2\pi} \mathrm d\theta \int_0^R f(r\cos\theta, r\sin\theta) r \mathrm dr = \iint\limits_{x^2+y^2 \leq R^2} f(x, y) \mathrm d\sigma$$

$$\int_0^{\pi} \mathrm d\theta \int_0^{2\pi} \mathrm d\varphi \int_0^R f(r\cos\theta, r\sin\theta\cos\varphi, r\sin\theta\sin\varphi) r^2\sin\theta \mathrm dr = \iiint\limits_{x^2+y^2+z^2 \leq R^2} f(x, y, z) \mathrm dv$$

### 广义、瑕重积分
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

---

简单考虑瑕积分。如果 $f$ 有瑕点 $p$，若恒非负则可简单定义为：

$$\iint_A f = \lim_{\varepsilon \to 0^+} \iint_{A \setminus B_{\varepsilon}(p)} f$$

若不恒非负，可以考虑 $f^+$ 与 $f^-$ 的瑕积分，若都收敛，定义为其差。

## 曲线与曲面积分
### 曲线积分
回忆可以用折线逼近的方法定义参数曲线的长度，有限时它是可求长曲线。可以证明分段光滑曲线是可求长的；参数曲线可求长当且仅当对每个分量是有界变差函数；函数是有界变差函数当且仅当可以写成两个单调递增函数之差。

对可求长参数曲线及有界实值函数，仿照 Riemann 和容易定义Ⅰ型曲线积分：

$$\int_{\gamma} f(\mathbf x) \mathrm ds \coloneqq \lim_{\lVert\Delta\rVert \to 0} \sum_{i=1}^n f(\gamma(\xi_i)) \Delta s_i$$

对可求长简单曲线 $\gamma$ 及 $f \in C(\gamma)$，该极限存在。

---

对向量场 $\mathbf F$，定义Ⅱ型曲线积分是：

$$\int_\gamma f_1 \mathrm dx_1 + \cdots + f_n \mathrm dx_n = \int_\gamma \mathbf F \cdot \mathrm d\vec s \coloneqq \lim_{\lVert\Delta\rVert \to 0} \sum_{i=1}^n \mathbf F(\gamma(\xi_i)) \cdot [\gamma(t_i) - \gamma(t_{i-1})]$$

对于分段 $C^1$ 曲线，可以转化为 Riemann 积分：

$$\int_\gamma \mathbf F \cdot \mathrm d\vec s = \int_\alpha^\beta \mathbf F(\gamma(t)) \cdot \gamma'(t) \mathrm dt$$

两端点重合时，约定了方向后，积分值与起点选取无关，记作（此时 $\gamma$ 常用点集表示）：

$$\oint_{\gamma} f_1 \mathrm dx_1 + \cdots + f_n \mathrm dx_n$$

### 曲面积分
对于 $\varphi: \Omega \to \R^n$ 为 $C^1$ 的参数曲面（$\Omega$ 在 $m \leq n$ 维中），$f$ 在 $\varphi(\Omega)$ 上连续，定义Ⅰ型曲面积分：

$$\iint_{\varphi(\Omega)} f \mathrm d\sigma = \iint \cdots \int_\Omega f \circ \varphi \sqrt{\det (J\varphi)^\top (J\varphi)} \mathrm du_1 \cdots \mathrm du_n$$

对于 $\mathbb E^3$ 中的可定向曲面 $\Sigma$ 及连续的单位法向量场 $\mathbf n$，定义连续向量值函数 $\mathbf X$ 的Ⅱ型曲面积分：

$$\iint_\Sigma \mathbf X \cdot \mathrm d\vec \sigma = \iint_\Sigma \mathbf X \cdot \mathbf n \mathrm d\sigma$$

使用[微分形式](@/posts/smooth_manifolds_2.md)的语言它等于下式：

$$\int_\Sigma X_x \mathrm dy \wedge \mathrm dz + X_y \mathrm dz \wedge \mathrm dx + X_z \mathrm dx \wedge \mathrm dy$$

这里（对可定向曲面）$\mathbf n$ 的选取有两种，标准定向对应的是向外。题目中可能使用别的表述方式来约定，对此约定使用右手坐标系。

### 关联
{% <theorem title="散度定理"> %}
设 $\Omega$ 是 $\R^n$ 中有 $C^1$ 边界的有界开集，$\mathbf X$ 是 $\bar \Omega$ 上的 $C^1$ 向量场，$\mathbf n$ 是边界上的单位外法向量场，则：

$$\int_\Omega \mathrm{div}(\mathbf X) \mathrm d\mathbf x = \int_{\partial \Omega} \mathbf X \cdot \mathbf n \mathrm d\sigma$$
{% </theorem> %}

这里散度是 $\nabla \cdot \mathbf X$，可以看成通量密度。使用单位分解与重积分化累次积分证明。

---

取 $\mathbf X = (Q, -P)$ 就可得到 Green 公式：

$$\int_\Omega \left(\frac{\partial Q}{\partial x} - \frac{\partial P}{\partial y}\right) \mathrm dx\mathrm dy = \int_{\partial \Omega} P \mathrm dx + Q \mathrm dy$$

这里定向取的是诱导定向。实际上所有的转换都可以使用微分形式的 Stokes 定理直接得到，故这里略去。
