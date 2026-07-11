+++
title = "单复变 Cauchy 积分理论"
date = 2025-11-16

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "分析学"]
+++

> 复变会告诉你：性质好的函数性质有多好。

<!-- more -->

## 路径积分
### 路径无关性
路径积分的定义可参考数学分析中作 Riemann 和，或者（对参数化 $\gamma: [a, b] \to \Complex$）考虑：

$$\int_{\gamma} f(z) \mathrm{d}z = \int_a^b f(\gamma(z)) \mathrm{d}\gamma(z) = \int_a^b f(\gamma(z))\gamma'(z) \mathrm{d}z$$

或者直接对微分算子进行：

$$\int_{\gamma} (u+\mathrm{i}v)(\mathrm{d}x+\mathrm{i}\cdot\mathrm{d}y) = \int_{\gamma} (u\mathrm{d}x-v\mathrm{d}y) + \mathrm{i}\int_{\gamma} (u\mathrm{d}y+v\mathrm{d}x)$$

例如说，对闭曲线 $\gamma$ 和 $n\neq -1$，有：

$$\oint_{\gamma}z^n \mathrm{d}z = \int_{\gamma}\frac{1}{n+1} \mathrm{d}z^{n+1} = 0$$

$n = -1$ 时考虑 $\gamma(\theta) = re^{\mathrm{i}\theta}$，换元知积分结果 $2\pi\mathrm{i}$；若原点不在曲线内部，则化为 $\oint \mathrm{d}\ \mathrm{Ln}z = 0$；通过曲线加减法可以推知，对所有把 $0$ 包含在内部的闭曲线，此积分结果均为 $2\pi\mathrm{i}$.

我们来说明一个更一般的结论：

{% admonition(type="theorem", title="Cauchy 定理") %}
有界区域 $\Omega$ 以有限段光滑曲线为边界，对 $f$ 在 $\bar{\Omega}$ 上连续，在 $\Omega$ 内解析，有：

$$\int_{\partial \Omega} f(z)\mathrm{d}z = 0$$

这说明路径积分的值只与起点、终点有关，而与选取的路径无关。
{% end %}

naive 的证明思想是，可以把一个三角形上的路径积分转化为它按中点分割成的四个三角形上的路径积分之和。实际上使用[同伦](@/posts/geometry_2_final.md)容易证明更强的结论：解析函数沿同伦的闭曲线的积分相等。

{% admonition(type="theorem", title="Cauchy 公式") %}
有界区域 $\Omega$ 以有限段光滑曲线为边界，对 $f$ 在 $\bar{\Omega}$ 上连续，在 $\Omega$ 内解析，任取 $z \in \Omega$ 有：

$$f(z) = \frac{1}{2\pi\mathrm{i}} \int_{\partial \Omega} \frac{f(w)}{w - z} \mathrm{d}w$$
{% end %}

取一个邻域 $\overline{D(z, \varepsilon)}$ 在 $\Omega$ 内，则显然可以将 RHS 中沿着 $\partial \Omega$ 积分改为沿着 $|w - z| = \varepsilon$ 积分。使用熟悉的局部线性化然后让 $\varepsilon \to 0$ 即可：

$$f(w) = f(z) + f'(z)(w - z) + o(w - z)$$

### 全纯的等价性
我们现在可以证明全纯函数和解析函数是等价的条件了。其中解析推全纯是容易的，因为幂级数在收敛半径内全纯。

若已知全纯，对 $z_0 \in \Omega$，取它在 $\Omega$ 中充分小的邻域 $\overline{D(z_0, r)}$，则由 Cauchy 公式：

$$f(z) = \frac{1}{2\pi\mathrm{i}}\int_{|w-z_0| = r}\frac{f(w)}{w-z}\mathrm{d}w$$

我们作以下展开：

$$\frac{1}{w-z} = \frac{1}{w-z_0}\cdot\sum_{n=0}^{+\infty}\left(\frac{z-z_0}{w-z_0}\right)^n$$

该级数是[一致收敛](@/posts/analysis_2_final.md)的，从而代入得：

$$f(z) = \sum_{n=0}^{+\infty} \frac{1}{2\pi\mathrm{i}}(z-z_0)^n \int_{|w-z_0| = r} \frac{f(w)}{(w-z_0)^{n+1}} \mathrm{d}w$$

即所求的幂级数。可喜可贺！

进一步地，若 $f$ 是全纯的，则 $u, v$ 都是实解析的。

{% admonition(type="theorem", title="Morera 定理") %}
对区域 $\Omega$ 与其上的连续函数 $f$，若对 $\Omega$ 内任意逐段光滑曲线围成的有界区域 $D$ 使 $\bar{D}\subset\Omega$，有 $\int_{\partial D}f(w)\mathrm{d}w = 0$，则 $f$ 在 $\Omega$ 内解析。
{% end %}

我们考虑任意 $D(z_0, \varepsilon) \subseteq \Omega$，可良定义：

$$F(z) = \int_{z_0}^z f(w)\mathrm{d}w$$

其满足 $F' = f$ 即全纯，从而 $F$ 是解析的，从而 $f$ 是解析的。

{% admonition(type="theorem", title="存在原函数") %}
对单连通区域 $D$ 和它上的解析函数 $f$，则存在它上的函数 $F$ 使 $F' = f$.
{% end %}

证明在前过程中。对非单连通区域，$\Complex \setminus \set{0}$ 上的 $f(z) = 1 / z$ 即为反例。

## 幂级数工具
### 零点阶数
对一个指定的区域 $\Omega$ 上的解析函数 $f$，我们令无穷阶零点集：

$$S = \set{z \in \Omega | f'(z) = f''(z) = \cdots = 0}$$

由连续性知 $\set{f^{(n)}(z) = 0}$ 为闭集，其无限交 $S$ 为闭集。又，对 $z_0\in S$ 有 $f$ 在其某个邻域内可展成幂级数，且该邻域 $D(z_0, \varepsilon) \subseteq S$，故 $S$ 为开集。我们知道 $\Omega$ 连通，从而 $S$ 要么是空集要么是整个 $\Omega$（此时 $f$ 在 $\Omega$ 上常值）。

这就给出了结论：对区域 $\Omega$ 上的非常值解析函数 $f$ 及任一点 $z_0$，存在自然数 $m$，使 $f'(z_0) = \cdots = f^{(m-1)}(z_0) = 0$ 而 $f^{(m)}(z_0) \neq 0$. 此时称 $f$ 在 $z_0$ 处的**零点阶数/消没次数**为 $m$.

此时，有 $f(z) - f(z_0) = (z - z_0)^m g(z)$，其中 $g$ 解析。这可以一般地定义为：

$$
g(z) = \begin{cases}
	(f(z) - f(z_0)) / (z - z_0)^m & z \neq z_0 \cr
	f^{(m)}(z_0) / m! & z = z_0
\end{cases}
$$

这将给出如下结论：

{% admonition(type="theorem", title="零点孤立性") %}
对区域 $\Omega$ 上的非常值解析函数 $f$，其零点是孤立的。
{% end %}

对一个零点，考察对应的 $g$，由解析推连续知存在某个邻域使 $g$ 处处非零，从而 $f$ 在去心邻域内处处非零。

{% admonition(type="theorem", title="解析函数的刚性/唯一性定理") %}
区域 $\Omega$ 上的解析函数 $f, g$，若存在点列 $\\{z_n\\}$ 且它有 $\Omega$ 内的极限点，则 $\Omega$ 上 $f\equiv g$.
{% end %}

这是因为该极限点是 $f - g$ 的非孤立零点。

如果极限点不在 $\Omega$ 内，则取 $z_n = \frac 1 {n\pi}$ 有反例 $f(z) = \sin \frac{1}{z}$ 与 $g(z) = 0$.

---

接下来，我们将解析函数看作平面区域到平面区域的映射。

由于对 $f(z)-f(z_0)$ 的 $m$ 阶零点有 $f(z) - f(z_0) = (z-z_0)^m g(z)$，我们可进一步取 $h^m = g$ 使 $f(z) - f(z_0) = ((z-z_0) h(z))^m$.

令 $\varphi(z) = (z-z_0) h(z)$，则 $\varphi'(z_0) = h(z_0)\neq 0$，故存在 $z_0$ 的邻域及 $\varphi(z_0) = 0$ 的邻域使 $\varphi$ 为解析同胚。

{% admonition(type="theorem", title="开映射定理") %}
区域 $\Omega$ 上的非常值解析函数 $f$ 是开映射。
{% end %}

只需 $f(\Omega)$ 开。对 $w_0 = f(z_0)$，其中 $z_0$ 为 $f(z)-f(z_0)$ 的 $m$ 阶零点，存在 $w_0$ 的邻域 $O$，使其中点在 $z_0$ 邻域内有 $m$ 个原像，故 $O\subset f(\Omega)$.

进而我们知道对单叶解析函数 $f$，它是 $\Omega\to f(\Omega)$ 的解析同胚。

### 代数基本定理
我们来使用开映射定理：

{% admonition(type="theorem", title="最大模原理") %}
对区域 $\Omega$ 上的非常值解析函数 $f$，$|f(z)|$ 在 $\Omega$ 内无最大值点。
{% end %}

因为 $f(z_0)$ 是 $f(D(z_0, \varepsilon))$ 的内点。这表明闭区域中最大值只能在边界处取到。

{% admonition(type="theorem", title="代数基本定理") %}
$n$ 次多项式 $P(z) = a_0 + \cdots + a_nz^n$ 在 $\Complex$ 中有零点。
{% end %}

将 $P$ 视作 $\bar{\Complex} \to \bar{\Complex}$ 的连续映射，其不为常数，故 $P(\bar{\Complex})$ 为开集，而 $\bar{\Complex}$ 是紧的，故 $P(\bar{\Complex})$ 为闭集。从而 $P(\bar{\Complex}) = \bar{\Complex}$.

## 积分工具
### Cauchy 估计
我们可以用 $|f(z)|$ 在区域内的最大值控制任意的 $|f^{(n)}(z)|$：

{% admonition(type="theorem", title="Cauchy 估计") %}
区域 $\Omega$ 上的解析函数 $|f| \leq M$，则对任意点 $z_0$ 及 $0 < r \leq \mathrm{dist}(z_0, \partial\Omega)$ 有：

$$|f^{(n)}(z_0)|\leq \frac{n!M}{r^n}$$
{% end %}

不妨设 $\Omega = D(z_0, R)$，对 $r < R$ 考虑下式，然后让 $r \to R$ 即可：

$$f^{(n)}(z_0) = \frac{n!}{2\pi\mathrm{i}}\int_{|w-z_0| = r}\frac{f(w)}{(w-z_0)^{n+1}}\mathrm{d}w$$

$$|f^{(n)}(z_0)| \leq \frac{n!}{2\pi} \int_{|w-z_0| = r} \left|\frac{f(w)}{(w-z_0)^{n+1}}\right| |\mathrm{d}w|\leq \frac{n!M}{r^n}$$

{% admonition(type="theorem", title="Liouville 定理") %}
解析函数 $f$ 在 $\Complex$ 上有界，则必为常数。
{% end %}

在先前结论中取 $r \to +\infty$，就有 $|f'(z_0)|$ 只能取零。

这表明：在解析同胚意义下，$\Complex$ 中单连通区域恰有 $\Complex$ 和 $D(0, 1)$ 两类。

我们将 $\Complex$ 上的解析函数称为**整函数**，其中不为多项式的称为**超越整函数**。

{% admonition(type="theorem", title="Weierstrass 定理") %}
对非常值的整函数 $f$，$f(\Complex)$ 在 $\Complex$ 中稠密。
{% end %}

假设结论不成立，从开集 $\Complex \setminus \overline{f(\Complex)}$ 中取一个 $z_0$，则有 $g(z) = 1 / (f(z) - z_0)$ 有界，矛盾。

{% admonition(type="theorem", title="Picard 小定理") %}
对非常值的整函数 $f$，$\Complex \setminus f(\Complex)$ 至多包含一个点。
{% end %}

不妨假设 $f$ 取不到 $0, 1$ 两点。取 $\lambda: D(0, 1) \to \Complex \setminus \set{0, 1}$ 的万有复叠，则 $f$ 可以被提升成全纯的 $\bar f$，有 $\bar f$ 是有界的，故 $\bar f$ 常值，从而 $f$ 常值。

对万有复叠构造，我们知道级 $2$ 的主同余子群 $\Gamma(2)$ 的基本区域 $Y(2) \cong \mathbb{P}(\Complex) \setminus \set{0, 1, \infty}$，同构的构造是：

$$\lambda(\tau) = \frac{\vartheta_2(0,\tau)^4}{\vartheta_3(0,\tau)^4}$$

### 平均值
{% admonition(type="theorem", title="平均值定理") %}
区域 $\Omega$ 上的解析函数 $f$，对任意点 $z_0$ 及 $0 < r < \mathrm{dist}(z_0, \partial \Omega)$ 有：

$$f(z_0) = \frac{1}{2\pi}\int_0^{2\pi} f(z_0 + re^{i\theta}) \mathrm{d}\theta$$
{% end %}

这可直接由 Cauchy 公式得到。

这给出**平均值不等式**：

$$|f(z_0)| = \frac{1}{2\pi} \int_0^{2\pi} |f(z_0 + re^{i\theta})| \mathrm{d}\theta$$

而对 $r$ 积分有：

$$f(z_0) = \frac{1}{\pi R^2} \iint \limits_{D(z_0, R)} f(z) \mathrm{d}S$$

## 平方可积解析函数
我们假定下文中区域 $\Omega$ 以有限条逐段光滑曲线为边界。

对 $\Omega$ 上的复值函数 $f$，称它为**平方可积函数**，若 $|f(z)^2|$ 在其上广义可积，其全体记作 $A^2(\Omega)$. 这是复线性空间，因为对 $f, g\in A^2(\Omega)$，有 $|f(z) + g(z)|^2 \leq 2(|f(z)|^2 + |g(z)|^2)$，从而 $f + g\in A^2(\Omega)$.

我们进一步在其上定义如下[**内积**](@/posts/linear_algebra_2_final.md)：

$$\braket{f, g} = \iint\limits_\Omega f(z) \overline{g(z)} \mathrm{d}S$$

---

现在可以给出一个最大模原理的新证明：假设 $z_0 \in \Omega$ 是 $|f(z)|$ 最大值点，有：

$$\iint\limits_{D(z_0, R)} |f(z_0)|^2 \mathrm{d}S \geq \iint\limits_{D(z_0, R)} |f(z)|^2$$

对 $\braket{f, \mathbf{1}}$ 使用 Cauchy 不等式，得到：

$$
\iint\limits_{D(z_0, R)} |f(z_0)|^2 \mathrm{d}S \geq
\frac 1 {\pi R^2} \left|\iint_{D(z_0, R)} f(z) \mathrm{d}S\right|^2 =
\pi R^2 |f(z_0)|^2
$$

取等，故有 $|f(z)|^2 \equiv |f(z_0)|^2$.

---

我们称**单位正交函数系**是指一族 $\braket{\varphi_i, \varphi_j} = \delta_{ij}$，如果它是极大的，则进一步称为**完备单位正交函数系**。

我们考虑函数 $f$ 对于一族完备单位正交函数系的形式级数：

$$f(z) \sim \sum_{n=1}^\infty \braket{f, \varphi_n} \varphi_n(z)$$

称为 $f$ 关于 $\\{\varphi_n\\}$ 的 Fourier 级数。它均方收敛且内闭收敛于 $f$.

## Schwarz 引理
{% admonition(type="theorem", title="Schwarz 引理") %}
$f$ 是单位圆盘到自身的解析映射，且 $f(0) = 0$，则：
1. 对圆盘内任一点，有 $|f(z)|\leq |z|$，这会使得 $|f'(0)|\leq 1$
2. 存在 $z_0\neq 0$ 使 $|f(z_0)| = |z_0|$ 或 $|f'(0)| = 1$ 的充要条件是 $f(z)=e^{\mathrm{i}\theta}z$
{% end %}

考虑 $f$ 在 $0$ 处展开成的幂级数 $a_1z + a_2z^2 + \cdots = zg(z)$，有 $g$ 在单位圆盘内解析。

对 $z_0$，取 $|z_0| < r < 1$，由最大模原理有 $|g(z)| \leq |g(r)| < 1 / r$，令 $r \to 1$ 即有 $|f(z_0)|\leq |z_0|$.

---

我们可以用 Schwarz 引理给出单位圆盘的解析自同胚群：因为 $f$ 与 $f'$ 均可经分式线性变换转为满足 Schwarz 引理的条件，它是保欧氏距离的，从而只能是分式线性变换，形如：

$$f(z) = e^{\mathrm{i}\theta}\frac{z-z_0}{1-\bar{z_0}z}$$

由于这些变换可以将单位圆盘中指定的一点变成另一指定的点，可以得到：

{% admonition(type="theorem", title="更一般形式的 Schwarz 引理") %}
$f$ 是单位圆盘到自身的解析映射，则对任意 $z_1, z_2\in D(0, 1)$ 有：

$$\left|\frac{f(z_1)-f(z_2)}{1-\overline{f(z_1)}f(z_2)}\right|\leq \left|\frac{z_1-z_2}{1-\bar{z_1}z_2}\right|$$
{% end %}

其微分形式为：

$$\frac{|\mathrm{d}f(z)|}{1-|f(z)|^2}\leq \frac{|\mathrm{d}z|}{1-|z|^2}$$

我们在单位圆盘上定义新的弧长微元（称为 Poincaré 度量）：

$$\mathrm{d}s = \frac{|\mathrm{d}z|}{1-|z|^2}$$

就可定义分段光滑曲线的非欧长度，并依次定义非欧距离和测地线，此过程与一般的光滑流形上的做法相同。
