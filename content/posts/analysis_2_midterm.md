+++
title = "数学分析Ⅱ期中复习笔记"
date = 2026-04-12
updated = 2026-04-13

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析", "微积分学"]
+++

本文用于准备数学分析Ⅱ的期中考试。这半学期的内容是定积分（Riemann 积分），广义积分（无穷积分与瑕积分）与数项级数（更多的敛散性判断与计算）。

<!-- more -->

## 定积分
### 概念与可积性
{% admonition(type="definition", title="定积分") %}
对任意分割和取点 $\xi_i \in [x_{i-1}, x_i]$ 极限存在时，定义：

$$\int_a^b f(x) \mathrm{d}x = \lim_{\max \\{\Delta x_i\\} \to 0} \sum_{i=1}^n f(\xi_i) \Delta x_i$$
{% end %}

此时称 $f$ 在 $[a, b]$ 上 Riemann 可积 $f \in R[a, b]$.

做题时可以用来把一些极限转成积分。

{% admonition(type="theorem", title="Newton-Leibniz 定理") %}
对 $f \in R[a, b]$ 若存在 $F'(x) = f(x)$ 则：

$$\int_a^b f(x) \mathrm{d}x = F(x) \Big |_a^b$$
{% end %}

使用 Lagrange 中值定理写为 $F(x_i) - F(x_{i-1}) = f(\xi_i) \Delta x_i$.

---

关于 Darboux 上下和，上下积分的内容省略。一个有用的证明可积的方法是证明振幅和可以任意小。

---

{% admonition(type="theorem", title="Riemann 定理") %}
设 $f$ 在 $[a, b]$ 上有界，则 $f \in R[a, b]$ 当且仅当对任意 $\varepsilon, \eta > 0$ 存在某个分割使得：

$$\sum _ {\text{振幅}_i \geq \eta} \Delta x_i < \varepsilon$$
{% end %}

通过琐碎的分析。

{% admonition(type="theorem", title="Lebesgue 定理") %}
设 $f$ 在 $[a, b]$ 上有界，则 $f \in R[a, b]$ 当且仅当其间断点集零测（开覆盖总长可任意小）。
{% end %}

基于 Riemann 定理通过琐碎的分析。

{% admonition(type="definition", title="有界变差函数") %}
对 $[a, b]$ 的一个分割 $\Delta$ 称 $\sigma(f, \Delta) = \sum_{i=0}^{n-1} |f(x_{i+1}) - f(x_i)|$ 为变差，定义全变差：

$$\bigvee_{a}^{b} f(x) = \sup_\Delta \sigma(f, \Delta)$$

其有界时称 $f$ 是 $[a, b]$ 上的有界变差函数，记作 $f \in BV[a, b]$.
{% end %}

Lipschitz 连续函数是有界变差的。感觉定义出来也没什么用，写在这里是防止题目里不给定义直接用。

### 性质
定积分的保序性、线性性、区间可加性易见。

{% admonition(type="question", title="习题课 W2 2") %}
设 $f \in C[0, 1]$ 且 $\int_0^1 f(x) \mathrm{d}x = 1$, 证明：

$$\int_0^1 (1+x^2) f^2(x) \mathrm{d}x \geq \frac{4}{\pi}$$
{% end %}

使用积分的 Cauchy 不等式，将之乘以：

$$\int_0^1 \frac{1}{1+x^2} \mathrm{d}x$$

{% admonition(type="question", title="习题课 W2 3") %}
设 $f \in C^1[0, 1]$ 且 $f(0) = 0, f(1) = 1$, 证明：

$$\int_0^1 |f(x) - f'(x)| \mathrm{d}x \geq \frac{1}{e}$$
{% end %}

令 $F(x) = e^{-x}f(x)$, 有原式大于等于：

$$\int_0^1 F'(x) \mathrm{d}x = F(1) - F(0)$$

{% admonition(type="definition", title="变上限函数") %}
$$F(x) = \int_a^x f(t) \mathrm{d}t$$

{% end %}

$F$ 连续，且在 $f$ 的所有连续点处可导，在这样的点处成立 $F'(x) = f(x)$. 因为可以考虑：

$$F(x) - F(x_0) = f(x_0) (x - x_0) + \int_{x_0}^x [f(t) - f(x_0)] \mathrm{d}t$$

{% admonition(type="theorem", title="换元积分") %}
$x = \varphi(t)$ 是 $[\alpha, \beta]$ 上连续可微的分段单调函数，满足 $\varphi(t) \in [A, B], [a, b] \subseteq [A, B], f \in C[A, B]$, 则：

$$\int_a^b f(x) \mathrm{d}x = \int_\alpha^\beta f(\varphi(t)) \varphi'(t) \mathrm{d}t$$
{% end %}

{% admonition(type="question", title="2024 P3") %}
计算定积分：

$$\int_0^1 \frac{\ln(1+x)}{1+x^2} \mathrm{d}x$$
{% end %}

考虑凑 $\ln$ 上的求和，作 $x = \frac{1 - t}{1 + t}$ 恰有：

$$\int_{\sqrt{2}-1}^1 \frac{\ln(1+x)}{1+x^2} \mathrm{d}x = \int_0^{\sqrt{2}-1} \frac{\ln \frac{2}{1+t}}{1+t^2} \mathrm{d}t$$

注：翻作业发现我之前做过，当时是正常做法，令 $t = \arctan x$.

{% admonition(type="theorem", title="分部积分法") %}
设 $u, v \in D[a, b]$ 且 $u', v' \in R[a, b]$, 则：

$$\int_a^b u(x) \mathrm{d}v(x) = u(x)v(x) \Big|_a^b - \int_a^b v(x) \mathrm{d}u(x)$$
{% end %}

这可以推出积分的 Young 不等式。进而有 Hölder 不等式，进而有：

{% admonition(type="question", title="习题课 W2 13 Minkowski 不等式") %}
设 $f, g \in R[a, b]$ 且恒非负，对 $p \geq 1$ 有：

$$\left(\int_a^b (f+g)^p(x) \mathrm{d}x\right)^{\frac{1}{p}} \leq \left(\int_a^b f^p(x) \mathrm{d}x\right)^{\frac{1}{p}} \left(\int_a^b g^p(x) \mathrm{d}x\right)^{\frac{1}{p}}$$
{% end %}

$$
\int_a^b (f+g)^p(x) \mathrm{d}x =
\int_a^b (f+g)^{p-1}(x)f(x) \mathrm{d}x + \int_a^b (f+g)^{p-1}(x)g(x) \mathrm{d}x \leq
\left(\int_a^b (f+g)^p(x) \mathrm{d}x\right)^{\frac{p-1}{p}} \left(\int_a^b f^p(x) \mathrm{d}x\right)^{\frac{1}{p}} + \cdots
$$

{% admonition(type="theorem", title="Wallis 公式") %}
$$\frac{\pi}{2} = \prod_{n=1}^\infty \left(\frac{2n}{2n-1} \cdot \frac{2n}{2n+1}\right)$$
{% end %}

通过计算 $I_n = \int_0^{\pi/2} \sin^n x \mathrm{d}x$ 奇数与偶数项的递推公式并夹逼。

{% admonition(type="tip", title="补充说明") %}
当我们说 $\int_0^1 \sin \frac{1}{x} \mathrm{d}x$ 时，实际上是指 $\int_0^1 f(x) \mathrm{d}x$, 其中：

$$
f(x) = \begin{cases}
	\sin \frac{1}{x} & x \neq 0 \\\\
	0 & x = 0
\end{cases}
$$
{% end %}

{% admonition(type="theorem", title="积分中值定理") %}
设有 $g \in R[a, b]$ 不变号及 $f \in R[a, b]$, 则存在 $\mu \in [\inf f, \sup f]$ 使得：

$$\int_a^b f(x)g(x) \mathrm{d}x = \mu \int_a^b g(x) \mathrm{d}x$$
{% end %}

其推论积分第一中值定理从略。

{% admonition(type="question", title="习题课 W3 1") %}
设 $f \in C^1[0, 1]$, 证明存在 $\xi \in [0, 1]$ 使得：

$$\int_0^1 f(x) \mathrm{d}x = f(0) + \frac{1}{2}f'(\xi)$$
{% end %}

左式等于：

$$\int_0^1 f(x) \mathrm{d}(x-1) = (x-1)f(x) \Big|_0^1 - \int_0^1 (x-1)f'(x) \mathrm{d}x = f(0) - f'(\xi)\int_0^1 (x-1) \mathrm{d}x$$

{% admonition(type="question", title="习题课 W3 4 (1)") %}
$$\lim_{n \to +\infty} \int_{n^2}^{n^2+n} \frac{e^{-1/x}}{\sqrt{x}} = 1$$
{% end %}

重点在于想到可以用中值定理。

{% admonition(type="question", title="讲义例 7.5.1") %}
设 $f \in C[a, b]$, 证明：

$$\lim_{n \to +\infty} \int_0^1 \frac{nf(x)}{1+n^2x^2} \mathrm{d}x = \frac{\pi}{2}f(0)$$
{% end %}

考虑：

$$\left|\int_0^1 \frac{n[f(x)-f(0)]}{1+n^2x^2} \mathrm{d}x\right| \leq \left|\int_0^\delta \cdots \mathrm{d}x\right| + \left|\int_\delta^1 \cdots \mathrm{d}x\right|$$

其中 $\delta$ 使得 $|f(x)-f(0)| < \frac{\varepsilon}{\pi}$.

{% admonition(type="theorem", title="积分第二中值定理") %}
设 $g \in R[a, b]$ 及 $f$ 在 $[a, b]$ 非负递增，则存在 $\xi \in [a, b]$ 使得：

$$\int_a^b f(x)g(x) \mathrm{d}x = f(a) \int_a^\xi g(x) \mathrm{d}x$$
{% end %}

{% admonition(type="theorem", title="广义分部积分公式") %}
设 $f, g \in R[a, b]$, 记 $F(x) = \int_a^x f(t) \mathrm{d}t, G(x) = \int_a^x g(t) \mathrm{d}t$, 则：

$$\int_a^b F(x)g(x) \mathrm{d}x = F(x)G(x) \Big|_a^b - \int_a^b G(x)f(x) \mathrm{d}x$$
{% end %}

用 Abel 变换。

关于积分余项此处从略。

{% admonition(type="theorem", title="Riemann-Lebesgue 引理") %}
设单调函数 $f \in R[a, b]$, 周期 $T$ 的周期函数 $g \in R[0, T]$ 满足 $\int_0^T g(x) \mathrm{d}x = 0$, 则：

$$\lim_{p \to +\infty} \int_a^b f(x)g(px) \mathrm{d}x = 0$$
{% end %}

取分割（设区间上下界 $M_i, m_i$）使得 $\sum (M_i - m_i) \Delta x_i < \frac{\varepsilon}{2\sup g}$, 则：

$$\left|\int_a^b f(x)g(px) \mathrm{d}x\right| = \left|\sum \int_{x_{i-1}}^{x_i} f(x)g(px) \mathrm{d}x\right| \leq \left|\sum \int_{x_{i-1}}^{x_i} [f(x) - m_i]g(px) \mathrm{d}x\right| + \left|\sum \int_{x_{i-1}}^{x_i} m_i g(px) \mathrm{d}x\right|$$

其中后一项通过：

$$\int_{x_{i-1}}^{x_i} g(px) \mathrm{d}x = \frac{1}{p} \int_{px_{i-1}}^{px_i} g(t) \mathrm{d}t \leq \frac{T\sup g}{p}$$

---

或者使用积分第二中值定理即可。

### 几何应用
定积分在几何学中的应用不考。由于与几何Ⅱ内容有一定重合，放在该处。

## 广义积分
### 无穷积分
无穷积分是用极限定义的，需注意只有对任意 $c$ 收敛且值相同时才有：

$$\int_{-\infty}^{+\infty} f(x) \mathrm{d}x = \int_{-\infty}^c f(x) \mathrm{d}x + \int_c^{+\infty} f(x) \mathrm{d}x$$

而 $\lim_{a \to +\infty} \int_{-a}^a f(x) \mathrm{d}x$ 是另一种东西（Cauchy 主值积分）。

Cauchy 准则、比较判别法及其极限形式、推广的 Newton-Leibniz 公式略去。

{% admonition(type="question", title="2024 P4") %}
$f(x)$ 以 $T$ 为周期，且可积，证明：

$$\lim_{n \to +\infty} n\int_n^{+\infty} \frac{f(x)}{x^2} \mathrm{d}x = \frac{1}{T} \int_0^T f(x) \mathrm{d}x$$
{% end %}

记 $\frac{1}{T} \int_0^T f(x) \mathrm{d}x = A$, 左式可以写为（由 $f$ 有界，差项趋于 $0$）：

$$\lim_{n \to +\infty} n \sum_{k=0}^{+\infty} \frac{A}{(kT + n)^2} \mathrm{d}x$$

这等于：

$$\int_0^{+\infty} \frac{A}{(xT + 1)^2} \mathrm{d}x = \frac{A}{T}$$

{% admonition(type="definition", title="绝对收敛") %}
若 $\int_a^{+\infty} |f(x)| \mathrm{d}x$ 收敛则称**绝对收敛**，收敛但不绝对收敛称**条件收敛**。
{% end %}

{% admonition(type="theorem", title="Dirichlet 判别法") %}
若 $f$ 的有限积分有界，且 $g$ 单调趋于 $0$, 则下式收敛：

$$\int_a^{+\infty} f(x)g(x) \mathrm{d}x$$
{% end %}

{% admonition(type="theorem", title="Abel 判别法") %}
若 $\int_a^{+\infty} f(x) \mathrm{d}x$ 收敛，且 $g$ 单调有界, 则下式收敛：

$$\int_a^{+\infty} f(x)g(x) \mathrm{d}x$$
{% end %}

这可以给出一个推论：⭐若 $g$ 单调趋于 $0$, 则 $f(x)$ 与 $f(x)(1+g(x))$ 无穷积分的收敛性相同。这是因为：两边都可用 Abel 判别法。

{% admonition(type="question", title="2022 P4") %}
讨论积分的收敛性和绝对收敛性：

$$\int_1^{+\infty} \frac{\sin x}{x^\alpha + \sin x} \mathrm{d}x$$
{% end %}

不妨 $\alpha > 0$, 先考察绝对收敛性，此时可用极限形式比较判别法，考察 $\frac{|\sin x|}{x^\alpha}$. 分析即可。

$$\frac{\sin x}{x^\alpha + \sin x} = \frac{\sin x}{x^\alpha} - \frac{\sin^2 x}{x^\alpha (x^\alpha + \sin x)}$$

第一项由 Dirichlet 判别法知收敛，第二项用极限形式比较判别法即可。

{% admonition(type="question", title="习题课 W4 5") %}
讨论积分的收敛性和绝对收敛性：

$$\int_1^{+\infty} \sin \left(\frac{\sin x}{x}\right) \mathrm{d}x$$

$$\int_1^{+\infty} \sin x^2 \mathrm{d}x$$
{% end %}

对 (1) 式，作 Taylor 展开：

$$\sin \left(\frac{\sin x}{x}\right) = \frac{\sin x}{x} - \frac{\cos (\xi(x))}{6} \left(\frac{\sin x}{x}\right)^3$$

减号后面部分绝对收敛。

对 (2) 式作换元得：

$$\int_1^{+\infty} \frac{\sin t}{2\sqrt{t}} \mathrm{d}t$$

### 瑕积分
同无穷积分，略去。

### 计算
{% admonition(type="question", title="书例 10.3.2") %}
计算积分：

$$I = \int_0^{+\infty} \frac{1}{1+x^4} \mathrm{d}x$$
{% end %}

令 $x = \frac{1}{t}$ 得 $I = \int_0^{+\infty} \frac{t^2}{1+t^4} \mathrm{d}t$. 从而：

$$2I = \int_0^{+\infty} \frac{1+x^2}{1+x^4} \mathrm{d}x = \int_0^{+\infty} \frac{(x-1/x)'}{(x-1/x)^2+2} \mathrm{d}x = \frac{1}{\sqrt{2}} \arctan \frac{u}{\sqrt{2}} \Big|_{-\infty}^{+\infty}$$

{% admonition(type="question", title="书例 10.3.5") %}
计算 Euler-Poisson 积分：

$$I = \int_0^{+\infty} e^{-x^2} \mathrm{d}x$$
{% end %}

过于有技巧性。通过对下式求极限得结果为 $\frac{\sqrt{\pi}}{2}$.

$$\int_0^{+\infty} (1-x^2)^n \mathrm{d}x \leq \int_0^{+\infty} e^{-nx^2} \mathrm{d}x \leq \int_0^{+\infty} \frac{1}{(1+x^2)^n} \mathrm{d}x$$

{% admonition(type="question", title="书例 10.3.6") %}
计算积分：

$$I = \int_0^{+\infty} \frac{\sin^2 x}{x^2} \mathrm{d}x$$
{% end %}

$$\int_0^{\pi/2} \frac{\sin (2n+1)x}{\sin x} \mathrm{d}x = \int_0^{\pi/2} (1 + 2\cos 2x + \cdots + 2\cos 2nx) \mathrm{d}x = \frac{\pi}{2}$$

通过分部积分知：

$$\int_0^{+\infty} \frac{\sin x}{x} \mathrm{d}x = \int_0^{+\infty} \frac{\sin^2 x}{x^2} \mathrm{d}x$$

我们考察前者，其等于：

$$
\lim_{n \to +\infty, n \text{ odd}} \int_0^{n\pi/2} \frac{\sin x}{x} \mathrm{d}x =
\lim_{n \to +\infty, n \text{ odd}} \int_0^{\pi/2} \frac{\sin nx}{x} \mathrm{d}x =
\lim_{n \to +\infty, n \text{ odd}} \int_0^{\pi/2} \frac{\sin nx}{\sin x} \mathrm{d}x + \lim_{n \to +\infty, n \text{ odd}} \int_0^{\pi/2} \sin nx \left(\frac{1}{x} - \frac{1}{\sin x}\right) \mathrm{d}x =
\frac{\pi}{2} + 0
$$

{% admonition(type="question", title="书例 10.3.8") %}
函数 $f$ 满足对 $0 < a < b$ 均有 $\int_a^b \frac{f(x)}{x} \mathrm{d}x$ 存在，且 $\lim_{x \to 0^+} f(x) = L, \lim_{x \to +\infty} f(x) = M$, 求 Frullani 积分：

$$I = \int_0^{+\infty} \frac{f(\alpha x) - f(\beta x)}{x} \mathrm{d}x, \quad \alpha, \beta > 0$$
{% end %}

$$
\int_a^b \frac{f(\alpha x) - f(\beta x)}{x} \mathrm{d}x =
\int_{a\alpha}^{b\alpha} \frac{f(y)}{y} \mathrm{d}y - \int_{a\beta}^{b\beta} \frac{f(y)}{y} \mathrm{d}y =
\int_{b\beta}^{b\alpha} \frac{f(y)}{y} \mathrm{d}y - \int_{a\beta}^{a\alpha} \frac{f(y)}{y} \mathrm{d}y
$$

使用积分第一中值定理，然后求极限。

## 数项级数
### 正项级数
默认此节中定理都加上正项级数要求。

{% admonition(type="theorem", title="结论") %}
若 $\\{a_n\\}$ 递减，且 $\sum a_n$ 收敛，则 $\lim na_n = 0$.
{% end %}

用 Cauchy 准则即可。

比较判别法、比值判别法、根式判别法从略。

{% admonition(type="theorem", title="Rabbe 判别法") %}
1. 若 $\underline{\lim}\ n(\frac{a_n}{a_{n+1}}-1) > 1$ 则 $\sum a_n$ 收敛
2. 若 $\overline{\lim}\ n(\frac{a_n}{a_{n+1}}-1) < 1$ 则 $\sum a_n$ 发散
{% end %}

采用 $\frac{1}{n^p}$ 为对标级数，使用比较判别法。若希望更精细，对标级数也可选取 $\frac{1}{n\ln^p n}$, $\frac{1}{n\ln n (\ln \ln n)^p}$ 等。

{% admonition(type="theorem", title="积分判别法") %}
$f$ 在 $[1, +\infty)$ 上单调下降趋于 $0$ 且 $a_n = f(n)$, 则 $\sum_{n=1}^{+\infty} a_n$ 与 $\int_1^{+\infty} f(x) \mathrm{d}x$ 敛散性相同。
{% end %}

{% admonition(type="theorem", title="Abel-Dini 定理") %}
1. $\sum a_n$ 收敛，则 $\sum a_n/T_n^{1+\alpha}$ 收敛的充要条件是 $\alpha < 0$, 其中 $T_n$ 是余式和 $a_n + \cdots$
2. $\sum a_n$ 发散，则 $\sum a_n/S_n^{1+\alpha}$ 收敛的充要条件是 $\alpha > 0$, 其中 $S_n$ 是部分和
{% end %}

考试考了 (2). 一侧用积分判别法，一侧用 Cauchy 准则。

### 任意项级数
Leibniz 判别法从略。Dirichlet 与 Abel 判别法类似无穷积分的结论。Abel 判别法推论也成立。

可以把相同符号的相邻项括在一起。

{% admonition(type="example", title="讲义例 9.3.3") %}
讨论交错级数 $\sum_{n=2}^{+\infty} \frac{(-1)^n}{n^p (\ln n)^q}$ 的收敛性和绝对收敛性。
{% end %}

$$
\begin{cases}
p<0 & \text{发散} \\\\
p=0 & \begin{cases} q\leq 0 & \text{发散} \\\\ q>0 & \text{条件收敛} \end{cases} \\\\
0<p<1 & \text{条件收敛} \\\\
p=1 & \begin{cases} q\leq 1 & \text{条件收敛} \\\\ q>1 & \text{绝对收敛} \end{cases} \\\\
p>1 & \text{绝对收敛}
\end{cases}
$$

{% admonition(type="question", title="2024 P2") %}
讨论级数的敛散性和绝对敛散性：

$$\sum_{n=2}^{+\infty} \frac{(-1)^n (n+1)^2 \arctan \sqrt{n}}{n^2 (n+(-1)^n)^p}$$
{% end %}

绝对敛散性是容易讨论的。使用比较判别法，转为讨论 $\sum_{n=2}^{+\infty} \frac{\pi / 2}{n^p}$ 敛散性使用熟知结论。

使用 Abel 判别法推论，题可被简化为 $\sum_{n=2}^{+\infty} \frac{(-1)^n}{(n+(-1)^n)^p}$ 敛散性，此式即 $\sum_{n=2}^{+\infty} \frac{(-1)^{n-1}}{n^p}$, 用 Leibniz 判别法即可。

### 级数重排
有界重排（$|\sigma(n)-n|$ 有界）不会改变敛散性和值。对无界重排我们有 [Riemann 重排定理](/posts/analysis-1-final/#jiang-yi-yue-du)。

---

$\sum_{n=1}^{+\infty} a_n$ 与 $\sum_{n=1}^{+\infty} b_n$ 绝对收敛时，其乘积矩阵中元素按任何顺序构成的级数都绝对收敛。

Mertens 定理从略。

{% admonition(type="question", title="2024 P5") %}
1. 将级数 $\sum_{n=1}^{+\infty} \frac{(-1)^{n+1}}{n}$ 重排，使其值为 $\ln 6$
2. 级数 $\sum_{n=1}^{+\infty} a_n, \sum_{n=1}^{+\infty} b_n$ 收敛，令 Cauchy 乘积级数 $c_i = \sum_{j+k = n+1} a_j b_k$, 正方形乘积 $\sum_{i=1}^{n} d_i = \sum_{i=1}^{n} a_i \sum_{i=1}^{n} b_i$. 证明：$\sum_{n=1}^{+\infty} c_n$ 的充要条件是：

$$\lim_{n \to +\infty} \sum_{i=1}^n d_i - \sum_{i=1}^n c_i = 0$$
{% end %}

对 (1) 我们证明引理：如果依次选取，每次取 $p$ 个正项 $q$ 个负项，则和为：

$$\ln 2 + \frac{1}{2} \ln \frac{p}{q}$$

对 (2) 只需必要性，即 Abel 定理。

一种证明是用 Stolz 和 Toplitz, 令对应的部分和 $A_i, B_i, C_i$, 有：

$$\lim C_n = \lim (C_1 + \cdots + C_n) / n = \lim (A_1B_n + \cdots A_nB_1) / n = \lim A_n \lim B_n$$

另一种是给 $a_n, b_n$ 配 $x^n$ 考察幂级数，但是幂级数应当是下半学期的，从略。

{% admonition(type="definition", title="无穷乘积") %}
用极限定义。注意积为 $0$ 时定义为发散。
{% end %}

在 $a_n > -1$ 时 $\prod (1+a_n)$ 收敛当且仅当 $\sum \ln (1+a_n)$ 收敛。

{% admonition(type="question", title="2022 P6") %}
1. 请给出 $\sum_{n=1}^\infty a_n$ 发散而 $\prod_{n=1}^\infty (1+a_n)$ 收敛的一个实例
2. 请尝试给出这样的 $a_n$ 的一般表达式并说明理由
{% end %}

(1) 的一个构造：奇数项为 $1$, 偶数项为 $-\frac{1}{2}$.

对 (2) 考虑 $\ln (1 + x) = x - \frac{1}{2} x^2 + o(x^2)$, 一个想法是：

$$
a_{2n} = \frac{1}{n^\alpha} + o\left(\frac{1}{n^{2\alpha}}\right) \\\\
a_{2n+1} = -\frac{1}{n^\alpha} + \frac{1}{n^{2\alpha}} + o\left(\frac{1}{n^{2\alpha}}\right)
$$
