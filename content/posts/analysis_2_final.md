+++
title = "【草稿】数学分析Ⅱ期末复习笔记"
date = 2026-06-08

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析", "微积分学"]
+++

本文用于准备数学分析Ⅱ的期末考试。这半学期的内容是函数项级数（一致收敛）、幂级数与多元函数。

<!-- more -->

## 函数项级数
### 定义
对一函数列，若某一点处极限存在称**收敛点**，收敛点的全体称为**收敛域**。对收敛域上每一指定点收敛的方式称为**逐点收敛**。

{% admonition(type="definition", title="一致收敛") %}
对 $I \subseteq \R$ 上的函数 $f(x), f_n(x)$, 若对任意 $\varepsilon > 0$ 存在 $N$ 使得：

$$|f_n(x) - f(x)| < \varepsilon, \forall n > N, x \in I$$

则称 $\\{f_n(x)\\}$ 在 $I$ 上一致收敛于 $f(x)$, 记作 $f_n(x) \rightrightarrows f(x), x \in I$.
{% end %}

如果在区间 $I$ 中任意闭区间上一致收敛，则称**内闭一致收敛**；如果在 $x_0$ 的一个邻域内一致收敛，则称**局部一致收敛**。

一致 Cauchy 准则略去。容易得到 Weierstrass/M-判别法：若 $|u_n(x)| \leq M_n$ 且 $\sum M_n$ 收敛则一致收敛。

{% admonition(type="theorem", title="Abel 判别法") %}
设函数项级数 $\sum_{n=1}^{+\infty} u_n(x)$ 在 $E \subseteq \R$ 上一致收敛，函数列 $\\{v_n(x)\\}$ 对于每个取定的 $x \in E$ 关于 $n$ 单调且在 $E$ 上一致有界，则 $\sum_{n=1}^{+\infty} u_n(x)v_n(x)$ 在 $E$ 上一致收敛。
{% end %}

{% admonition(type="theorem", title="Dirichlet 判别法") %}
设函数项级数 $\sum_{n=1}^{+\infty} u_n(x)$ 的部分和函数列在 $E \subseteq \R$ 上一致有界，函数列 $\\{v_n(x)\\}$ 对于每个取定的 $x \in E$ 关于 $n$ 单调且在 $E$ 上一致收敛到 $0$，则 $\sum_{n=1}^{+\infty} u_n(x)v_n(x)$ 在 $E$ 上一致收敛。
{% end %}

### 性质
{% admonition(type="theorem", title="连续性") %}
函数列 $\\{f_n(x)\\}$ 在 $E \subseteq \R$ 上一致收敛到 $f(x)$，若 $f_n(x)$ 在 $x_0 \in E$ 处连续，则 $f(x)$ 也在 $x_0 \in E$ 处连续。
{% end %}

分析即可。也可以写成：

$$\lim_{\substack{x \to x_0 \cr x \in E}} \lim_{n \to +\infty} f_n(x) = \lim_{n \to +\infty} \lim_{\substack{x \to x_0 \cr x \in E}} f_n(x)$$

在 $f_n(x) \leq f_{n+1}(x)$ 的条件下，Dini 定理说，若 $f_n(x)$ 在 $[a, b]$ 上连续且逐点收敛到 $f(x)$，则 $f(x)$ 在 $[a, b]$ 上连续的充要条件是在 $[a, b]$ 上一致收敛。

{% admonition(type="definition", title="等度连续") %}
在 $[a, b]$ 上函数列 $\\{f_n(x)\\}$ 满足对任意 $\varepsilon > 0$ 存在 $\delta > 0$ 使得 $|x_1 - x_2| < \delta$ 时 $|f_n(x_1) - f_n(x_2)| < \varepsilon$，则称在 $[a, b]$ 上等度连续。
{% end %}

在 $[a, b]$ 上，等度连续与逐点收敛给出 $f(x)$ 连续。

{% admonition(type="question", title="习题课 W8 6 Arzela-Ascoli 引理") %}
在 $[a, b]$ 上函数列 $\\{f_n(x)\\}$ 等度连续，且一致有界，则存在一致收敛子列。
{% end %}

取 $[a, b]$ 的可数稠密子集 $Q = \set{x_1, x_2, \dots}$.

取 $\\{f_n(x)\\}$ 子列 $\\{f_n^1(x)\\}$ 使得在 $x_1$ 处收敛；再取其子列 $\\{f_n^2(x)\\}$ 使得在 $x_2$ 处收敛……同理我们得到一个在 $Q$ 上收敛的子列 $\\{f_n^n\\}$.

对 $\varepsilon > 0$ 取 $\delta(\varepsilon / 3)$ 满足等度连续定义式。由于 $\bigcup_{x \in Q} B(x, \delta)$ 开覆盖，存在有限子覆盖，设中心是 $y_1, \dots, y_k$. 则存在 $N$ 使得 $n, m > N$ 时：

$$|f_n^n(y_i) - f_m^m(y_j)| < \frac{\varepsilon}{3}$$

{% admonition(type="question", title="习题课 W9 1") %}
构造 $\R$ 上等度连续的函数列 $\\{f_n(x)\\}$ 逐点收敛于 $f(x)$ 但不一致收敛。
{% end %}

$$f_n(x) = \sin \left(\frac{x}{n}\right)$$

{% admonition(type="theorem", title="可积性") %}
函数列 $\\{f_n(x)\\}$ 在 $[a, b]$ 上一致收敛到 $f(x)$，若 $f_n(x)$ 在 $[a, b]$ 上均 Riemann 可积，则 $f(x)$ 也在 $[a, b]$ 上 Riemann 可积，且：

$$\lim_{n \to +\infty} \int_a^b f_n(x) \mathrm{d}x = \int_a^b f(x) \mathrm{d}x$$
{% end %}

{% admonition(type="theorem", title="Arzelà 控制收敛定理") %}
在 $[a, b]$ 上，设 $f_n(x)$ 逐点收敛到 $f(x)$，且 $f_n(x)$ 与 $f(x)$ 均可积，若 $f_n(x)$ 一致有界，则：

$$\lim_{n \to +\infty} \int_a^b f_n(x) \mathrm{d}x = \int_a^b f(x) \mathrm{d}x$$
{% end %}

这个定理要远强于逐项积分定理。

{% admonition(type="theorem", title="可微性") %}
在 $[a, b]$ 上，设 $f_n(x)$ 可微且 $\\{f_n'(x)\\}$ 一致收敛到某 $g(x)$，且某点 $c$ 处 $\\{f_n(c)\\}$ 收敛，则 $f_n(x)$ 必一致收敛于某可微函数 $f(x)$ 且：

$$\lim_{n \to +\infty} f_n'(x) = f'(x)$$
{% end %}

{% admonition(type="question", title="习题 12.3:20") %}
设 $f(x) = \sum_{n=1}^{+\infty} \frac{1}{10^n} \\{10^n x\\}$，其中 $\\{t\\}$ 表示离最近的整数的距离，证明 $f(x)$ 处处连续、不可微。
{% end %}

对 $k \geq 10$，若 $(10^{k-1}x_0, 10^{k-1}x_0 + 1/10)$ 中没有半整数，则 $10^k(f(x_0 + 10^{-k}) - f(x_0))$ 与 $k$ 奇偶性不同；而 $(10^{k-1}x_0 - 1/10, 10^{k-1}x_0)$ 中没有半整数情形亦然。因此取充分大不同奇偶性的 $k$ 即可。

## 幂级数
### 收敛半径
幂级数是指形如 $\sum_{n=0}^{+\infty} a_n (x - x_0)^n$ 的函数项级数（其中规定 $x^0 = 1$）。讨论时通常默认 $x_0 = 0$.

{% admonition(type="theorem", title="Cauchy-Hadamard 定理") %}
对幂级数 $\sum_{n=0}^{+\infty} a_n x^n$ 记：

$$\rho = \varlimsup_{n \to +\infty} |a_n|^{\frac{1}{n}}$$

及 $R = 1 / \rho$ 有：
1. $R = +\infty$ 时，幂级数在 $(-\infty, +\infty)$ 中绝对收敛
2. $R = 0$ 时，幂级数仅在 $0$ 处收敛
3. $0 < R < +\infty$ 时幂级数在 $(-R, R)$ 中绝对收敛，在 $[-R, R]$ 之外发散
{% end %}

称 $R$ 是**收敛半径**，非零时称 $(-R, R)$ 为收敛区间（不一定同于收敛域）。

{% admonition(type="theorem", title="Abel 第二定理") %}
对幂级数 $\sum_{n=0}^{+\infty} a_n x^n$ 收敛半径 $0 < R < +\infty$，则：
1. 幂级数在收敛区间内闭绝对一致收敛
2. 若在 $R$ 处收敛，则在 $(-R, R]$ 内闭一致收敛
3. 若在 $-R$ 处收敛，则在 $[-R, R)$ 内闭一致收敛
{% end %}

### Taylor 展开式
{% admonition(type="definition", title="幂级数展开") %}
称函数 $f(x)$ 在 $x_0$ 附近可以展开成幂级数，是指某个邻域内有：

$$f(x) = \sum_{n=0}^{+\infty} a_n (x - x_0)^n$$
{% end %}

若在区间 $I$ 上每点附近都能展开成幂级数，则称它是 $I$ 上的实解析函数。$I$ 上实解析函数全体记作 $C^\omega(I)$.

{% admonition(type="definition", title="Taylor 级数") %}
设 $f(x)$ 在 $x_0$ 处有任意阶导数，则 $f(x)$ 在 $x_0$ 处的 Taylor 级数：

$$f(x) \sim \sum_{n=0}^{+\infty} \frac{f^{(n)}(x_0)}{n!} (x - x_0)^n$$
{% end %}

如果它在 $x_0$ 的某个邻域收敛于 $f$，则称为 **Taylor 展开式**。

{% admonition(type="question", title="Sophomore's Dream") %}
$$\int_0^1 x^{-x} \mathrm{d}x = \sum_{n=1}^\infty n^{-n}$$
{% end %}

$$x^{-x} = e^{-x \ln x} = \sum_{n=0}^\infty \frac{(-1)^n x^n \ln^n x}{n!}$$

在 $[0, 1]$ 绝对一致收敛，故：

$$
\int_0^1 x^{-x} \mathrm{d}x =
\sum_{n=0}^\infty \int_0^1 \frac{(-1)^n x^n \ln^n x}{n!} \mathrm{d}x =
\sum_{n=0}^\infty \frac{1}{(n+1)^{n+1}}
$$

### 连续函数多项式逼近
{% admonition(type="theorem", title="Weierstrauss 定理") %}
如果 $f(x) \in C[a, b]$，则 $f(x)$ 于 $[a, b]$ 可被多项式一致逼近。
{% end %}

一种证法是，考察 $n$ 阶 Bernstein 多项式（设 $[0, 1]$ 上）：

$$B_n(f, x) = \sum_{k=0}^n f\left(\frac{k}{n}\right) \binom{n}{k} x^k (1-x)^{n-k}$$

---

另一种证法：对 $x \in [-1, 1]$，让 $u_0(x) = 0$ 与 $u_{n+1}(x) = u_n(x) + \frac{1}{2}(x^2 - u_n^2(x))$，使用 Dini 定理知一致收敛到 $|x|$.

类似地可以构造函数一致收敛到 $\lambda|x-c|$，从而可以一致收敛到分段线性函数。

## 多元函数
### 基本概念
关于内积的理论参考[高等代数Ⅱ期末复习笔记](@/posts/linear_algebra_2_final.md)。点集拓扑、$\R^n$ 中点列极限、压缩映射原理略去。

$n - 1$ 个 $n$ 元向量的外积是：

$$
\begin{vmatrix}
\epsilon_1 & \cdots & \epsilon_n \cr
v_1^1 & \cdots & v_n^1 \cr
\vdots & & \vdots \cr
v_1^{n-1} & \cdots & v_n^{n-1}
\end{vmatrix}
$$

{% admonition(type="question", title="习题课 W11 11 Vitali 覆盖") %}
设指标集 $\Lambda$，对 $\lambda \in \Lambda$ 有 $\R^n$ 中的闭球 $B_\lambda = \overline{B(x_\lambda, r_\lambda)}$. 设 $\sup r_\lambda < +\infty$，证明存在可数子集 $\Lambda_0$ 满足：
1. 其中的 $B_i, B_j$ 两两不交
2. $\bigcup_{\lambda \in \Lambda} B_\lambda \subseteq \bigcup_{\lambda \in \Lambda_0} \overline{B(x_\lambda, 5r_\lambda)}$
{% end %}

记 $R = \sup r_\lambda$，令：

$$\tilde{\Lambda_j} = \set{\lambda \in \Lambda | R2^{-j} < r_\lambda \leq R2^{-j+1}}$$

取 $\tilde{\Lambda_1}$ 的可数子集 $\Lambda_1$ 使得 $\tilde{\Lambda_1} \setminus \Lambda_1$ 中的闭球都与 $\Lambda_1$ 中的有交。同理可取出所有可数子集 $\Lambda_k$，取并即可。

### 极限
对于 $f: E \to \R, E \subseteq \R^n$，定义其**极限/重极限** $\lim_{p \to p_0} f(p) = A$，如果 $p_0$ 是 $E$ 的一个聚点，且对任意 $\varepsilon > 0$ 存在 $\delta > 0$ 使得对任意 $p \in B(p_0, \delta) \cap E$ 有 $|f(p) - A| < \varepsilon$. 向量值函数则就是对每个分量求极限。

而像 $\lim_{y \to y_0} \lim_{x \to x_0} f(x, y)$ 这样的则是**累次极限**。

多元函数在 $p_0$ 处连续，就定义成 $\lim_{p \to p_0} f(p) = f(p_0)$. 注意这里需要 $p_0$ 是聚点，我们不认为定义域孤立点处函数连续。

### 微分
以二元函数 $f(x, y)$ 为例。

{% admonition(type="definition", title="偏导数") %}
关于 $x$ 的偏导数是：

$$\lim_{\Delta x \to 0} \frac{f(x_0 + \Delta x, y_0) - f(x_0, y_0)}{\Delta x}$$

记作 $\frac{\partial f(x_0, y_0)}{\partial x}$ 或 $\left.\frac{\partial f}{\partial x}\right|_{(x_0, y_0)}$ 或 $f_x(x_0, y_0)$.
{% end %}

$f(x) \in C^1(E)$ 是指在 $E$ 上各个偏导数都连续。

对 $f \in C^k(E)$，其任意的 $k$ 阶偏导数与求偏导次序无关。

{% admonition(type="definition", title="全微分") %}
类似一元函数微分，全微分即全增量关于自变量增量的线性部分。
{% end %}

若可微，则各个偏导数存在，有：

$$\mathrm{d}f(x_0, y_0) = \frac{\partial f(x_0, y_0)}{\partial x} \mathrm{d}x + \frac{\partial f(x_0, y_0)}{\partial y} \mathrm{d}y$$

$f(x) \in C^1(E)$ 可以说明在 $E$ 上可微。

{% admonition(type="definition", title="方向导数") %}
对 $\lVert l \rVert = 1$ 定义方向导数：

$$\frac{\partial f(x_0, y_0)}{\partial l} \coloneqq \lim_{\rho \to 0^+} \frac{f(p_0 + \rho l) - f(p_0)}{\rho}$$
{% end %}

作为一个例子，考虑函数：

$$
f(x, y) = \begin{cases}
	\frac{xy}{x^2+y^2} & (x, y) \neq (0, 0) \cr
	0 & (x, y) = (0, 0)
\end{cases}
$$

在 $(0, 0)$ 处偏导数存在，但非坐标轴方向的方向导数均不存在。

{% admonition(type="definition", title="梯度") %}
若函数在 $(x_0, y_0)$ 可微，则记此处函数梯度：

$$\operatorname{grad} f(x_0, y_0) \text{ or } \nabla f(x_0, y_0) \coloneqq \left.\left(\frac{\partial f}{\partial x}, \frac{\partial f}{\partial y}\right)\right|_{(x_0, y_0)}$$
{% end %}

方向导数与梯度满足：

$$\frac{\partial f}{\partial l} = \nabla f \cdot l$$

{% admonition(type="theorem", title="Young 定理") %}
设 $f: E \to \R^2$ 及 $(x_0, y_0)$，若 $f$ 在 $(x_0, y_0)$ 附近 $f_x$ 与 $f_y$ 存在且可微，那么：

$$f_{xy}(x_0, y_0) = f_{yx}(x_0, y_0)$$
{% end %}

{% admonition(type="theorem", title="Schwarz 定理") %}
设 $f: E \to \R^2$ 及 $(x_0, y_0)$，若 $f$ 在 $(x_0, y_0)$ 附近 $f_x$ 与 $f_y$ 存在；$f_{xy}$ 存在且在该点连续，则 $f_{yx}$ 也存在且：

$$f_{xy}(x_0, y_0) = f_{yx}(x_0, y_0)$$
{% end %}

{% admonition(type="definition", title="Fréchet 导数") %}
若 $\lVert h \rVert \to 0$ 时 $f(x_0 + h) - f(x_0) = Ah + r(h)$，其中 $\lVert r(h) \rVert = o(\lVert h \rVert)$ 成立，则称 $A$ 为 $f$ 在 $x_0$ 处的 Fréchet 导数。
{% end %}

{% admonition(type="definition", title="Jacobi 矩阵") %}
$$
J f = \begin{pmatrix}
	\frac{\partial f_1}{\partial x_1} & \cdots & \frac{\partial f_1}{\partial x_n} \cr
	\vdots & & \vdots \cr
	\frac{\partial f_m}{\partial x_1} & \cdots & \frac{\partial f_m}{\partial x_n}
\end{pmatrix}
$$
{% end %}

在 $x_0$ 处可微时有：

$$\mathrm{d}f(x) = J f(x_0) \mathrm{d}x$$

对 $h = g \circ f$ 满足 $f$ 在 $x_0$ 处可微，$g$ 在 $f(x_0)$ 处可微，则 $h$ 在 $x_0$ 处可微，且：

$$J h(x_0) = J g(f(x_0)) \cdot J f(x_0)$$

{% admonition(type="theorem", title="多元函数微分中值定理") %}
设 $D \subseteq \R^n$ 为凸域，函数 $f: D \to \R$ 在 $D$ 中处处可微，则任给 $x, y \in D$ 存在 $\lambda \in (0, 1)$ 使得：

$$f(x) - f(y) = \nabla f(\lambda x + (1 - \lambda) y) \cdot (x - y)$$
{% end %}

应用一元函数微分中值定理即可。

{% admonition(type="theorem", title="拟微分中值定理") %}
设 $D \subseteq \R^n$ 为凸域，函数 $f: D \to \R$ 在 $D$ 中处处可微，则任给 $x, y \in D$ 存在 $\xi \in D$ 使得：

$$\lVert f(x) - f(y) \rVert \leq \lVert Jf(\xi) \rVert \cdot \lVert x - y \rVert$$
{% end %}

取一组基，使用 Cauchy-Schwarz 不等式。

{% admonition(type="theorem", title="Taylor 公式") %}
设 $D \subseteq \R^n$ 为凸域，$f \in C^{m+1}(D)$，$a \in D$，则任给 $x \in D$ 存在 $\theta \in (0, 1)$ 使得：

$$
f(x) = \sum_{k=0}^m \sum_{|\alpha|=k} \frac{D^\alpha f(a)}{\alpha!}(x - a)^\alpha +
\sum_{|\alpha|=m+1} \frac{D^\alpha f(a + \theta(x - a))}{\alpha!}(x - a)^\alpha
$$

其中 $\alpha$ 是多重指标。
{% end %}

对 $\varphi(t) = f(a + t(x - a))$ 使用一元函数 Taylor 公式。

{% admonition(type="definition", title="Hesse 矩阵") %}
我们定义 $f$ 的 Hesse 矩阵：

$$\nabla^2 f \coloneqq \left(\frac{\partial^2 f}{\partial x_j \partial x_i}\right)_{n \times n}$$

定义 Laplace 算子 $\Delta = \operatorname{tr} \nabla^2$.
{% end %}

关于极值点、鞍点定义略去。当 $f$ 在 $x_0$ 处取到极小/大值，且在一个邻域内可微、一阶偏导可微，则其 Hesse 矩阵半正/负定。若在邻域内有二阶连续偏导数，则 Hesse 矩阵在邻域内均半正/负定是充分条件（用 Taylor 公式）。

### 隐函数存在定理
{% admonition(type="theorem", title="隐函数存在定理（一元）") %}
设 $u = F(x, y)$ 在 $B((x_0, y_0), \delta)$ 内满足 $F(x_0, y_0) = 0$；$F(x, y)$ 与 $F_y(x, y)$ 连续；$F_y(x_0, y_0) \neq 0$，则存在 $\delta_0 \in (0, \delta)$ 使得存在定义在 $(x_0 - \delta_0, x_0 + \delta_0)$ 上唯一的连续函数 $y = f(x)$ 满足 $y_0 = f(x_0)$ 及 $F(x, f(x)) = 0$.
{% end %}

先取半径 $\delta_1$ 使得 $F_y(x, y)$ 同号，再取 $\delta_0$ 使得 $F_y$ 分别在 $(x_0 - \delta_0, x_0 + \delta_0) \times \set{y_0 - \delta_1}$ 与 $(x_0 - \delta_0, x_0 + \delta_0) \times \set{y_0 + \delta_1}$ 上同号。在构成的矩形每一条竖线上有唯一的 $y(x)$ 使得 $F(x, y(x)) = 0$.

或者使用压缩映射原理。对函数定义距离是区间上差的最大值。取 $k$ 使得 $\Phi(x, y) = y - kF(x, y)$ 是关于 $y$ 的压缩。

多元情形的定理同理。这使得 $x^2 + y^2 + z^2 = a^2 \implies x\mathrm{d}x + y\mathrm{d}y + z\mathrm{d}z$ 之类的事情是合理的。

---

Lagrange 乘数法、最小二乘法等略去。
