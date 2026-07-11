+++
title = "（椭圆）模形式基本定义与结构"
date = 2026-07-11

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "数论"]
+++

李文威在《模形式初步》中言，模形式是一门彻上彻下，勾连四方的学问，能有效组织被本科课程分割承包的知识，进而将数学还原到浑然一体的面貌。本文略过了各种数论应用。

<!-- more -->

参考的是 Don Zagier 的 *Elliptic Modular Forms and Their Applications* 第一、二节。

## 基本定义
### 模函数
我们用 $\mathfrak{H}$ 表示上半平面，$\mathrm{SL}(2, \R)$ 在其上标准的作用方式是 Möbius 变换：

$$
\begin{aligned}
	\gamma = \begin{pmatrix} a & b \cr c & d \end{pmatrix}: \mathfrak{H} & \to \mathfrak{H} \cr
	z & \mapsto \frac{az + b}{cz + d}
\end{aligned}
$$

用 $\Im$[^im] 表示虚数部分，我们有下式：

$$\Im(\gamma z) = \frac{\Im(z)}{|cz+d|^2} \tag{1}$$

（椭圆）模函数与模形式是在 Fuchsian 群[^fuchsian]（暂时只考虑全模群 $\Gamma_1 = \mathrm{SL}(2, \Z)$）的作用下不变/以特定方式变形的函数。

模群得名于 $\mathfrak{H} / \Gamma_1$ 是 $\Complex$ 上椭圆曲线的同构类。每个点 $z \in \mathfrak{H}$ 可对应到一个格 $\Lambda_z = \set{m + nz | m, n \in \Z}$，而 $\Complex / \Lambda_z$ 是一个 $\Complex$ 上的椭圆曲线。[^elliptic-curve]反之，一个 $\Complex$ 上的椭圆曲线可以写成某个 $\Complex / \Lambda$，在（$\Complex$ 上）位似等价意义下唯一。考虑格的有序基 $(\omega_1, \omega_2), \Im(\omega_1 / \omega_2) > 0$，选取不同的有序基对应 $\Gamma_1$ 中的变换。

我们说**模空间**大致是指，一个代数簇，其点对应某个固定类型的代数簇的同构类。**模函数**是其上的一个复值函数（在 $\mathrm{SL}(2, \R)$ 的离散子群这种语境下则要求是亚纯函数）。**模形式**是 $\mathfrak{H}$ 上（可能还要尖点）的全纯函数。每个模函数可以表示成两个模形式的商。

从格的观点看，模形式是满足 $F(\lambda\Lambda) = \lambda^{-k}F(\Lambda)$ 的函数，其中 $k$ 称作**权**。用 $M_k(\Gamma)$ 表示 $\Gamma$ 上权 $k$ 的模形式构成的空间。我们记 $f(z) = F(\Lambda_z)$ 则对 $\Gamma_1$ 有：

$$f\left(\frac{az + b}{cz + d}\right) = (cz + d)^k f(z) \tag{2}$$

对于尖点处的全纯性，对应的是次指数增长，对任意 $C > 0$ 有 $f(x + \mathrm{i}y) = O(e^{Cy}), c \to +\infty$ 及 $f(x + \mathrm{i}y) = O(e^{C/y}), c \to 0$.

在 (2) 中取 $\gamma(z) = z + 1$ 知 $f$ 有周期 $1$，从而可以表达为如下 Fourier development，这给出了模形式与其它数学领域的重大关联。

$$f(z) = \sum_{n=0}^\infty a_nq^n, q = e^{2\pi\mathrm{i}z} \tag{3}$$

### 基本区域
我们可以考虑 $\overline{\Gamma_1} = \Gamma_1/\set{\pm 1}$. 由 (2) 知奇权的 $f$ 一定是 $0$，从而只需讨论偶权的 $M_k(\overline{\Gamma_1})$.

群 $\overline{\Gamma_1}$ 有生成元 $S(z) = -1/z$ 与 $T(z) = z+1$ 满足关系 $S^2 = (ST)^3 = 1$. 有 $f \in M_k(\Gamma_1)$ 等价于它有周期 $1$ 且：

$$f(-1/z) = z^k f(z), z \in \mathfrak{H} \tag{4}$$

我们知道 $f$ 在一点处的值即可知道整个轨道上的值。我们引入**基本区域**是指一个开集 $\mathcal{F}$ 满足 $\mathcal{F}$ 中不同点不在一个轨道上；$z \in \mathfrak{H}$ 中的点都可等价到某个 $\mathcal{F}$ 的闭包中的点。

{% admonition(type="theorem", title="全模群的基本区域") %}
全模群的基本区域是：

$$\mathcal{F}_1 = \set{z \in \mathfrak{H} | |z| > 1, |\Re(z)| < \frac 1 2}$$
{% end %}

设 $\Lambda_z$ 中模长最小点 $cz + d$，则存在 $\gamma_1 = \begin{pmatrix} a & b \cr c & d \end{pmatrix} \in \Gamma_1$. 由 (1) 知 $\Im(\gamma_1z)$ 在轨道上最大。取 $n$ 使 $|\Re(\gamma_1z + n)| < 1/2$ 即知在 $\mathcal{F}_1$ 中。

假设 $\mathcal{F}_1$ 中有 $z_2 = \gamma z_1$，则通过下式枚举讨论即可：

$$\frac{\sqrt 3}{2} < \Im(z_2) = \frac{\Im(z_1)}{|cz_1 + d|^2} \leq \frac{\Im(z_1)}{c^2\Im(z_1)^2} < \frac{2}{c^2\sqrt 3}$$

给 $\mathcal{F}_1$ 加上边界实部非正的部分成为 $\widetilde{\mathcal{F}_1}$，则它是一个严格基本区域。

### 有限维数性
现在来证 $M_k(\Gamma_1)$ 是有限维的。

$f$ 的[零点阶数](@/posts/complex_analysis_2.md) $\mathrm{ord}_z(f)$ 在 $z$ 的轨道上是一致的，从而对 $P \in \mathfrak{H}/\Gamma_1$ 有 $\mathrm{ord}_P(f)$ 是合法的。$\widetilde{\mathcal{F}_1}$ 有奇点/椭圆点，因为 $\omega = e^{2\pi\mathrm{i}/3}$ 在 $ST$ 作用下不动，$\mathrm{i}$ 在 $S$ 作用下不动，我们用 $n_P$ 表示 $P$ 在 $\overline{\Gamma_1}$ 中的稳定化子的阶数。$\mathfrak{H}/\Gamma_1$ 需要被紧化，这可以通过添加一个 $\infty$ 实现（此时 $\overline{\mathcal{F}_1} = (\mathfrak{H} \cup \mathbb{Q} \cup \set{\infty}) / \Gamma_1$）。

我们定义 $\mathrm{ord}_\infty(f)$ 是 (3) 式中的最小非零 $a_n$，则：

{% admonition(type="theorem", title="命题") %}
设 $f \in M_k(\Gamma_1)$ 非零，则：

$$\sum _{P \in \widetilde{\mathcal{F} _1}} \frac{1}{n _P} \mathrm{ord} _P(f) + \mathrm{ord} _\infty(f) = \frac{k}{12} \tag{5}$$
{% end %}

![挖去零点](/images/complex/modular_zeros.jpg)

在 $\overline{\mathcal{F}_1}$ 中挖去所有零点与无穷的邻域（$\Im(z) > Y$），使这些邻域不交，设得到 $D$. 对 $\mathrm{d}(\ln z)$ 沿着 $D$ 的边界积分使用 Cauchy 定理知为 $0$.

另一方面可以分段积分，两边的竖直线积分值被抵消。对水平线 $l$ 从 $-1/2+\mathrm{i}Y$ 到 $1/2+\mathrm{i}Y$，设在 $\infty$ 处 Fourier 展开写为 $q^{\mathrm{ord}_\infty (f)} \cdot h(q)$，其中 $h$ 在 $0$ 处全纯，则：

$$
\int _l \mathrm{d}(\ln f) =
\int _l (\mathrm{ord} _\infty (f) + \ln h) \mathrm{d}q =
2\pi\mathrm{i} \cdot \mathrm{ord} _\infty (f)
$$

其它零点就是正常使用[留数定理](@/posts/complex_analysis_3.md)（如果稳定化子不是中心则拼起来）。

对圆弧 $a_1$ 从角度 $\pi/3$ 到 $\pi/2$，圆弧 $a_2$ 从角度 $\pi/2$ 到 $2\pi/3$，使用 (4) 有：

$$
\int _{a_1} \mathrm{d}(\ln f) =
\int _{a_1} \mathrm{d}(\ln f(-1/z)) - k\mathrm{d}z / z =
-\int _{a_2} \mathrm{d}(\ln f) - \int _{a_1} k\mathrm{d}z / z
$$

$$\int _{a_1} k\mathrm{d}z / z = k\int _{\pi/3}^{\pi/2} \mathrm{i} \mathrm{d}\theta = \mathrm{i} \frac \pi 6$$

整理得到 (5) 式。

这给出推论，在 $k < 0$ 或 $k$ 奇时 $M_k(\Gamma_1)$ 的维数为 $0$，其余情况：

$$
\dim M_k(\Gamma_1) = \begin{cases}
	[k/12] + 1 & k \not\equiv 2 \pmod{12} \cr
	[k/12] & k \equiv 2 \pmod{12}
\end{cases} \tag{6}
$$

因为我们可以取 $m = [k/12] + 1$ 个非椭圆点，对 $f_1, \dots, f_{m+1}$ 可以取线性组合让这些点处全取 $0$，然后使用结论。

另一个推论是，对线性无关的 $f, g \in M_{12}(\Gamma_1)$，有以下同构：

$$
\begin{aligned}
	\mathfrak{H} / \Gamma_1 \cup \set{\infty} & \to \mathbb{P}^1(\Complex) \cr
	z & \mapsto f(z) / g(z)
\end{aligned}
$$

因为任意 $\lambda f - \mu g$（其中 $(\lambda, \mu) \neq (0, 0)$）恰有一个零点。

---

我们设置一个双曲度规（$\mathrm{d}(x + \mathrm{i}y) = y^{-2}\mathrm{d}x\mathrm{d}y$），则：

$$
\mathrm{Vol}(\mathfrak{H} / \Gamma_1) =
\int_{-1/2}^{1/2} \left(\int_{\sqrt{1-x^2}}^\infty \frac{\mathrm{d}y}{y^2}\right) \mathrm{d}x =
\int_{-1/2}^{1/2} \frac{\mathrm{d}x}{\sqrt{1 - x^2}} =
\frac \pi 3
$$

通过与 (5) 式类似的推导可以说明，对于一般的离散、使得基本区域体积有限的子群 $\Gamma \subseteq \mathrm{SL}(2, \R)$，右侧会被换成 $k \mathrm{Vol}(\mathfrak{H} / \Gamma) / 4\pi$（使用 Gauss‑Bonnet 来联系面积）。

{% admonition(type="theorem", title="命题") %}
设 $\Gamma$ 是 $\mathrm{SL}(2, \R)$ 的离散子群且基本区域体积有限，则：

$$\dim M_k(\Gamma) \leq \frac{kV}{4\pi} + 1$$
{% end %}

特别地，$k < 0$ 时 $M_k(\Gamma) = \set{0}$，$k = 0$ 时 $M_k(\Gamma) = \Complex$.

这一命题的用处是，如果我们（在数论中）得到了两个数列 $\\{a_n\\}, \\{b_n\\}$ 并猜想它们是等同的，如果 $\sum a_n q^n$ 与 $\sum b_n q^n$ 是同一个群的相同权的模形式，则只需要对有限个 $a_n = b_n$ 做验证。

## Eisenstein 级数
### Eisenstein 级数
我们定义记号（那么式 (2) 是 $f = f|_k \gamma$）：

$$(f|_k \gamma) = (cz+d)^{-k} f\left(\frac{az+b}{cz+d}\right) \tag{7}$$

那么 $f \mapsto f|_k g$ 给出了 $\mathrm{SL}(2, \R)$ 到 $\mathfrak{H}$ 上次指数/指数增长的全纯函数构成线性空间的一个作用。空间 $M_k(\Gamma)$ 就是被 $\Gamma$ 固定的一个子空间。

对一般的有限群 $G$ 到线性空间 $V$ 上的线性作用，一个构造 $G$-不变向量的方法是选定一个 $v_0$ 然后考虑 $\sum_{g \in G} v_0|g$. 如果 $v_0$ 是 $G_0$ 不变的，可以只考虑 $\sum_{g \in G/G_0} v_0|g$. 在 $G$ 无限的情况下，我们求和就需要考虑收敛性。

此处，取 $\Gamma$ 是一个 Fuchsian 群，$v_0$ 是一个有理函数，以这种方式得到的称为 Poincaré 级数。特别地，考虑：

$$
\Gamma_\infty =
\set{\gamma \in \Gamma | \gamma\infty = \infty} =
\set{\pm (z \mapsto z + n) | n \in \Z}
$$

我们定义 **Eisenstein 级数**：

$$E_k(z) = \sum_{\gamma \in \Gamma_1 / \Gamma_\infty} 1|_k \gamma$$

可进一步写出：

$$
E _k(z) =
\sum _{\gamma \in \overline{\Gamma _1} / \overline{\Gamma _\infty}} 1| _k \gamma =
\frac 1 2 \sum _{\substack{c, d \in \Z \cr (c, d) = 1}} \frac{1}{(cz + d)^k} \tag{8}
$$

我们可见在 $k > 2$ 时它绝对收敛，且取 $\Im (z) \to \infty$ 知它非零。

---

另一种引入方式是，我们考虑：

$$G_k(\Lambda) = \frac 1 2 \sum_{\lambda \in \Lambda \setminus 0} \lambda^{-k} \tag{9}$$

也即：

$$G_k(z) = \frac 1 2 \sum_{\substack{m, n \in \Z \cr (m, n) \neq (0, 0)}} \frac 1 {(mz + n)^k} \tag{10}$$

事实上存在差一个 Riemann zeta 函数的关系：

$$G_k(z) = \zeta(k)E_k(z) \tag{11}$$

实际上还存在如下正则化方式，使得 Fourier 系数都是有理数：

$$\mathbb{G}_k(z) = \frac{(k-1)!}{(2\pi\mathrm{i})^k} G_k(z) \tag{12}$$

{% admonition(type="theorem", title="环的结构") %}
环 $M_\ast(\Gamma_1) \coloneqq \bigoplus_k M_k(\Gamma)$ 由 $E_4$ 与 $E_6$ 中的 Eisenstein 级数自由生成。
{% end %}

先证明线性无关性。假设存在 $E_6(z)^2 = \lambda E_4(z)^3$ 的关系，则权为 $2$ 的 $f(z) = E_6(z) / E_4(z)$ 满足 $f^2 = \lambda E_4$，从而是全纯的，与 $M_2(\Gamma_1) \leq 0$ 矛盾。这将说明 $E_4$ 与 $E_6$ 线性无关。

然后使用式 (6) 分析维数即可，此结论说明式 (6) 实际上可以取等号。

### Fourier 展开
{% admonition(type="theorem", title="Fourier 展开") %}
对偶数 $k > 2$，有 $\mathbb{G}_k(z)$ 的 Fourier 展开为：

$$-\frac{B_k}{2k} + \sum_{n=1}^\infty \sigma_{k-1}(n) q^n \tag{13}$$

其中 $B_k$ 表示第 $k$ 个 Bernoulli 数，即：

$$\sum_{k=0}^\infty \frac{B_k}{k!} x^k = \frac{x}{e^x - 1}$$

$\sigma_{k-1}(n)$ 表示 $n$ 的所有正因数的 $k-1$ 次方和。
{% end %}

证明略。[^proof-expansion]

我们得到：

$$
\begin{array}{l}
	\mathbb{G}_4(z) = \frac 1 {240} + q + 9q^2 + 28q^3 + 73q^4 + 126q^5 + 252q^6 + \cdots \cr
	\mathbb{G}_6(z) = -\frac 1 {504} + q + 33q^2 + 244q^3 + 1057q^4 + \cdots \cr
	\mathbb{G}_8(z) = \frac 1 {480} + q + 129q^2 + 2188q^3 + \cdots
\end{array}
$$

这给出：

$$
\begin{array}{l}
	E_4(z) = 1 + 240q + 2160q^2 + \cdots \cr
	E_6(z) = 1 - 504q - 16632q^2 + \cdots \cr
	E_8(z) = 1 + 480q + 61920q^2 + \cdots
\end{array}
$$

### 权 2 的 Eisenstein 级数
我们可以使用式 (13) 反过来定义 Eisenstein 级数在权 $2$ 的情况，或者让：

$$G_2(z) = \frac 1 2 \sum_{n \neq 0} \frac 1 {n^2} + \frac 1 2 \sum_{m \neq 0} \sum_{n \in \Z} \frac 1 {(mz + n)^2} \tag{14}$$

由于没有绝对收敛性，我们不再有 $G_2(-1/z) = z^2 G_2(z)$，而是：

{% admonition(type="theorem", title="命题") %}
$$G_2\left(\frac{az + b}{cz + d}\right) = (cz + d)^2 G_2(z) - \pi\mathrm{i}c(cz + d) \tag{15}$$
{% end %}

我们考察以下 (10) 式的改造：

$$G_{2, \varepsilon}(z) = \frac 1 2 \sum_{(m, n) \neq (0, 0)} \frac 1 {(mz + n)^2 |mz + n|^{2\varepsilon}}$$

通过分析方法可以说明：

$$\lim_{\varepsilon \to 0} G_{2, \varepsilon}(z) = G_2(z) - \frac \pi {2y}$$

### 判别函数
我们定义判别函数（这来自于对应的椭圆曲线的判别式）为：

$$\Delta(z) = q \prod_{n=1}^\infty (1-q)^{24} \tag{16}$$

{% admonition(type="theorem", title="命题") %}
$\Delta$ 是 $\Gamma_1$ 的权 $12$ 的模形式。
{% end %}

由于 $\Delta \neq 0$ 我们可以考虑：

$$
\frac 1 {2\pi\mathrm{i}} \frac{\mathrm{d}}{\mathrm{d}z} \ln \Delta(z) =
1 - 24\sum_{n=1}^\infty \frac{nq^n}{1 - q^n} =
1 - 24\sum_{n=1}^\infty \sigma_1(m)q^m = E_2(z)
$$

这会给出：

$$
\frac 1 {2\pi\mathrm{i}} \frac{\mathrm{d}}{\mathrm{d}z} \ln \left(\frac{\Delta(\frac{az + b}{cz + d})}{(cz + d)^{12}\Delta(z)}\right) = 0
$$

我们回忆 $\dim M_{12}(\Gamma_1) = 2$，也就是说 $M_{12}(\Gamma_1) = \mathrm{span} \set{(E_4(z))^3, (E_6(z))^2}$，只需要观察两者 Fourier 展开的常数项及一次项即知：

$$\Delta(z) = \frac 1 {1728} ((E_4(z))^3 - (E_6(z))^2) \tag{17}$$

回顾之前提及的到 $\mathbb{P}^1(\Complex)$ 的同构，我们可以取模函数，称为模不变量（系数表可见于 [A000521 - OEIS](https://oeis.org/A000521)）：

$$j(z) = \frac{(E_4(z))^3}{\Delta(z)} = q^{-1} + 744 + 196884q + 21493760q^2 + \cdots$$

---

我们记 $\Delta(z)$ 的系数：

$$\Delta(z) = \sum_{n=1}^\infty \tau(n) q^n$$

$\tau(n)$ 的值列表可以参考 [A000594](https://oeis.org/A000594). Ramanujan 在 1915 年计算了前 30 项并猜测对不同的素数有 $\tau(pq) = \tau(p)\tau(q)$，并于次年被 Mordell 证明。Ramanujan 也猜想 $|\tau(p)| \leq 2p^5\sqrt{p}$，这直到 1974 才被 Deligne 作为 Weil 猜想的一个结果证明。

---

我们称一个模形式是**尖点形式**，如果 Fourier 展开中的常数项为 $0$. 从而可以声称模形式可以写成一个 Eisenstein 级数与一个尖点形式的线性组合。

{% admonition(type="theorem", title="命题") %}
若 $f(z) \in M_k(\Gamma_1)$ 是尖点形式，写成 $\sum_{n=1}^\infty a_n q^n$，则 $|a_n| \leq Cn^{k/2}$，其中 $C$ 只与 $f$ 有关。
{% end %}

函数 $z \mapsto y^{k/2}|f(z)|$ 在 $\Gamma_1$ 下不变。由于 $f(z) = O(q)$ 可给出估计：

$$|f(z)| \leq cy^{-k/2}$$

对任意 $y$ 我们有下式，从而可取 $y = 1/n$ 及 $C = ce^{2\pi}$：

$$a_n = e^{2\pi ny} \int_0^1 f(x + \mathrm{i}y) e^{-2\pi\mathrm{i}nx} \mathrm{d}x$$

---

[^im]: 命令 `\Im`，Zagier 讲义中显示的是以前的版本 $\mathfrak{J}$.
[^fuchsian]: $\mathrm{SL}(2, \R)$ 的离散子群。而 $\mathrm{SL}(2, \Z)$ 的子群称为**同余子群**。而 $N$ 级的**主同余子群**指：

$$\Gamma(N) = \set{\gamma \in \mathrm{SL}(2, \R) | \gamma \equiv \begin{pmatrix} 1 & 0 \\\\ 0 & 1 \end{pmatrix} \pmod N}$$

[^elliptic-curve]: 某些代数几何结论指出亏格 1 的紧 Riemann 面可嵌入射影平面成为光滑三次曲线。实际上可以用 Weierstrass p 函数写出：

$$\wp(z) = \frac{1}{z^2} + \sum_{\omega \in \Lambda \setminus \set{0}} \left(\frac{1}{(z-\omega)^2} - \frac{1}{\omega^2}\right)$$

$$(\wp')^2 = 4\wp^3 - g_2\wp - g_3$$

[^proof-expansion]: 用到一些讲义没有解释证明的 Euler 结论，应当属于一般的解析数论方法。
