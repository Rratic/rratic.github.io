+++
title = "数学分析Ⅰ期末复习笔记"
description = "需要记忆的结论与 trick."
date = 2026-01-06

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析", "微积分学"]
+++

本文用于准备数学分析Ⅰ的期末考试。

期中挂了，所以期末还是整理一下为好。重要的 trick 使用⭐标出，在一些浏览器环境中可以使用 `Ctrl + F` 搜索功能。

## 主线
下半学期是从导数开始的。

### 导数与微分
$f'_+(x_0)$ 意为右导数：

$$\lim_{\Delta x \to 0^+} \frac{f(x_0 + \Delta x) - f(x_0)}{\Delta x}$$

记法 $f'(g(x))$ 是指 $f'(u) |_{u = g(x)}$.

一些反三角函数的导数列举如下：

| 函数 | 导函数 |
| :-: | :-: |
| $\arcsin x$ | $\frac{1}{\sqrt{1-x^2}}$ |
| $\arccos x$ | $-\frac{1}{\sqrt{1-x^2}}$ |
| $\arctan x$ | $\frac{1}{1+x^2}$ |
| $\operatorname{arccot} x$ | $-\frac{1}{1+x^2}$ |

在有限情形，总有：

$$\left(\sum_{k=1}^n f_k(x)\right)' = \sum_{k=1}^n f_k'(x)$$

$$(f_1(x) \cdots f_n(x))' = \sum_{k=1}^n f_1(x) \cdots f_{k-1}(x) f_k'(x) f_{k+1}(x) \cdots f_n(x)$$

微分是这样定义的：如果存在常数 $A$，使得

$$\Delta y = f(x + \Delta x) - f(x) = A \Delta x + o(\Delta x) \quad (\Delta x \to 0)$$

关于微分的高观点看法可以参考[微分与微分形式](/posts/differential-forms/)。另外 $o(h(x))$ 可以被理解为无幺的函数环。

对乘法高阶导数有 Leibniz 法则：

$$\mathrm{d}^n (u \cdot v) = \sum_{k=0}^n \binom{n}{k} \mathrm{d}^{n-k}u \cdot \mathrm{d}^k v$$

复合函数的高阶导数有 Faà di Bruno 法则，但是应该不至于用到。

### 微分中值定理
{% admonition(type="theorem", title="Fermat 引理") %}
$f$ 在 $x_0$ 处可导且它为极值点，则 $f'(x_0) = 0$.
{% end %}

使用左右导数证明。称 $f'(x) = 0$ 的点为**驻点**。

{% admonition(type="theorem", title="Rolle 中值定理") %}
$f \in C[a, b] \cap D(a, b)$ 满足 $f(a)=f(b)$，则存在 $\xi \in (a, b)$ 使得 $f'(\xi) = 0$.
{% end %}

使用 Fermat 引理即可。

反复使用它得到的有用推论是：⭐若 $f$ 在 $[a, b]$ 上有 $k$ 个零点（记重数），则存在 $\xi \in (a, b)$ 使得 $f^{(k-1)}(\xi) = 0$，只有一个 $\geq k$ 阶零点的情形除外。

在构造函数使用 Rolle 中值定理时，⭐有时会用到：

$$(f \cdot e^g)' = (f' + f \cdot g') \cdot e^g$$

{% admonition(type="theorem", title="Lagrange 中值定理") %}
对 $f \in C[a, b] \cap D(a, b)$ 存在 $\xi \in (a, b)$ 使得 $f(b) - f(a) = f'(\xi) (b - a)$.
{% end %}

使用 Rolle 中值定理即可。

它的有用推论是：若 $f'(x) = 0$ 则 $f(x) \equiv c$，这用其它方法没有那么方便证明。

{% admonition(type="theorem", title="Cauchy 中值定理") %}
对 $f, g \in C[a, b] \cap D(a, b)$ 存在 $\xi \in (a, b)$ 使得 $(f(a)-f(b))g'(\xi) = (g(a)-g(b))f'(\xi)$.
{% end %}

我们对 $h(x) = k_1f(x) - k_2g(x)$ 使用 Rolle 中值定理，其中：

$$\frac{k_2}{k_1} = \frac{f(b)-f(a)}{g(b)-g(a)}$$

⭐这种构造方法在做题时很常见。

{% admonition(type="theorem", title="Darboux 介值定理") %}
$f \in D[a, b]$ 且 $f'(a) < f'(b)$，则对任意 $\lambda \in (f'(a), f'(b))$ 存在 $\xi \in (a, b)$ 使得 $f'(\xi) = \lambda$.
{% end %}

考虑 $h(x) = f(x) - \lambda x$ 的最小值点。

### L’Hôpital 法则
{% admonition(type="theorem", title="L’Hôpital 法则Ⅰ") %}
$f, g \in D(a, b)$ 且 $g'$ 在 $(a, b)$ 非零，如果：
1. $\lim_{x\to a^+} f(x) = \lim_{x\to a^+} g(x) = 0$
2. $\lim_{x\to a^+} \frac{f'(x)}{g'(x)}$ 在扩展实数系中存在

则有：

$$\lim_{x\to a^+} \frac{f(x)}{g(x)} = \lim_{x\to a^+} \frac{f'(x)}{g'(x)}$$
{% end %}

补定义 $f(x) = g(x) = 0$，然后使用 Cauchy 中值定理即可。容易把单侧改为双侧的结果。

⭐可以反复使用 L’Hôpital 法则同时适当进行无穷小代换。

{% admonition(type="theorem", title="导数极限定理") %}
$f$ 在 $x_0$ 的邻域内连续，在 $x_0$ 的去心邻域内可导，且 $\lim_{x \to x_0} f'(x)$ 存在，则：

$$f'(x_0) = \lim_{x \to x_0} f'(x)$$
{% end %}

使用 Lagrange 中值定理即可。

{% admonition(type="theorem", title="L’Hôpital 法则Ⅱ") %}
$f, g \in D(a, b)$ 且 $g'$ 在 $(a, b)$ 非零，如果：
1. $\lim_{x\to a^+} g(x) = \infty$
2. $\lim_{x\to a^+} \frac{f'(x)}{g'(x)}$ 在扩展实数系中存在

则有：

$$\lim_{x\to a^+} \frac{f(x)}{g(x)} = \lim_{x\to a^+} \frac{f'(x)}{g'(x)}$$
{% end %}

使用 Cauchy 中值定理即可。容易把单侧改为双侧的结果。

### Taylor 展开式
{% admonition(type="theorem", title="带 Peano 余项的 Taylor 公式") %}
$f$ 在 $x_0$ 处 $n$ 阶可导，则：

$$f(x) = \sum_{k=0}^n \frac{f^{(k)}(x_0)}{k!} (x-x_0)^k + o((x-x_0)^n) \quad (x\to x_0)$$
{% end %}

令 $R(x) = f(x) - \sum_{k=0}^n \frac{f^{(k)}(x_0)}{k!} (x-x_0)^k$，则 $R(x_0) = \cdots = R^{(n)}(x_0) = 0$，反复使用 L’Hôpital 法则即有：

$$\lim_{x\to x_0} \frac{R(x)}{(x-x_0)^n} = \cdots = \frac{1}{n!} R^{(n)}(x_0) = 0$$

一些 $n$ 阶 Maclaurin 多项式列举如下：

| 函数 | 多项式 |
| :-: | :-: |
| $e^x$ | $1+x+\frac{1}{2}x^2+\cdots+\frac{1}{n!}x^n$ |
| $\sin x$ | $x-\frac{1}{6}x^3+\frac{1}{120}x^5+\cdots+(-1)^{n-1}\frac{x^{2n-1}}{(2n-1)!}$ |
| $\cos x$ | $1-\frac{1}{2}x^2+\frac{1}{24}x^4+\cdots+(-1)^{n}\frac{x^{2n}}{(2n)!}$ |
| $\ln (1+x)$ | $x-\frac{1}{2}x^2+\frac{1}{3}x^3+\cdots+(-1)^{n-1}\frac{x^n}{n}$ |
| $(1+x)^\alpha$ | $1+\alpha x+\frac{\alpha(\alpha-1)}{2}x^2+\cdots+\frac{\alpha(\alpha-1)\cdots(\alpha-n+1)}{n!}x^n$ |

$\tan x$，$\arcsin x$ 等较为复杂且写了也记不住，不再列举。

$\arctan x$ 导一次后变成 $\frac{1}{1+x^2}$，这是好展开的。

⭐可以直接用带 Peano 余项的 Taylor 公式来写加减法、乘法、除法（有时）、函数复合的结果。

⭐有时可以从表达式先提取出一个 $x^\alpha$，把剩下部分写成带 Peano 余项的 Taylor 公式。可以参考 Puiseux 级数相关。

{% admonition(type="theorem", title="带 Lagrange 余项的 Taylor 公式") %}
$f$ 在 $(a, b)$ 上 $n+1$ 阶可导，则对 $x, x_0 \in (a, b)$ 存在介于期间的 $\xi$ 使得：

$$f(x) = \sum_{k=0}^n \frac{f^{(k)}(x_0)}{k!} (x-x_0)^k + \frac{f^{(n+1)}(\xi)}{(n+1)!}(x-x_0)^{n+1}$$
{% end %}

可以反复使用 Cauchy 中值定理。

感觉 Cauchy 型余项之类的东西没什么用。

### 利用导数研究函数
凸是指下凸，定义为对不同的 $a, b \in I$ 及 $0 < t < 1$ 有 $f(ta+(1-t)b) \leq tf(a) + (1-t)f(b)$. 开区间上凸函数条件可以推出连续。

{% admonition(type="definition", title="拐点") %}
$f(x)$ 在 $U(x_0, \delta)$ 连续，且在 $(x_0 - \delta, x_0)$ 与 $(x_0, x_0 + \delta)$ 有相异的严格凸性，则称 $x_0$ 为 $f(x)$ 的拐点。
{% end %}

若 $f''(x)$ 在该点处存在则其值为 $0$.

### 不定积分
一个需补充记忆的结论：

$$\int \frac{1}{\sqrt{1+x^2}} \mathrm{d}x = \ln \left(x+\sqrt{1+x^2}\right) + C$$

如果忘记了，考虑换元 $x = \frac{e^t - e^{-t}}{2}$（在其它情形下换元 $x = \tan x$ 可能更好）有：

$$\int \frac{1}{\sqrt{1+x^2}} \mathrm{d}x = \int \frac{2}{e^t + e^{-t}} \mathrm{d}\left(\frac{e^t - e^{-t}}{2}\right) = t + C$$

这个结论将给出：

$$\int \sqrt{1+x^2} \mathrm{d}x = \frac{1}{2} \left(x\sqrt{x^2+1} + \ln \left(x+\sqrt{1+x^2}\right)\right) + C$$

又，带绝对值的积分得到的可能是分段函数，如 $\int |\cos (x)| \mathrm{d}x$，不能直接写成 $\sin (x) \mathrm{sgn}(\cos x) + C$.

积分有大量的技巧……如下：

{% admonition(type="question", title="例题") %}
求 $I = \int \frac{\cos x}{a\cos x + b\sin x} \mathrm{d}x$ 与 $J = \int \frac{\sin x}{a\cos x + b\sin x} \mathrm{d}x$.
{% end %}

使用：

$$
\left\\{\begin{align*}
& aI + bJ = x + C \\\\
& bI - aJ = \ln |a\cos x + b\sin x| + C
\end{align*}\right.
$$

此外，对 $\int \frac{1}{a\cos x + b\sin x} \mathrm{d}x$ 可考虑 $a\cos x + b\sin x = \sqrt{a^2 + b^2} \cos (x + \phi)$.

{% admonition(type="question", title="例题") %}
计算 $I_n = \int \frac{1}{(x^2 + 1)^n} \mathrm{d}x$.
{% end %}

$$
I_n = \int \frac{1}{(x^2 + 1)^n} \mathrm{d}x =
\frac{x}{(x^2 + 1)^n} - \int x \cdot \mathrm{d}\left(\frac{1}{(x^2 + 1)^n}\right) =
\frac{x}{(x^2 + 1)^n} + 2n \int \frac{x^2}{(x^2 + 1)^{n+1}} \mathrm{d}x =
\frac{x}{(x^2 + 1)^n} + 2n I_n - 2n I_{n+1}
$$

有：

$$I_{n+1} = \frac{x}{2n(x^2+1)^n} + \frac{2n-1}{2n} I_n$$

特别地，$I_1 = \arctan x + C$.

{% admonition(type="question", title="例题") %}
$y$ 是由 $y^2(x-y) = x^2$ 确定的隐函数，计算 $\int \frac{1}{y^2} \mathrm{d}x$.
{% end %}

⭐令 $y = tx$，则有 $x = \frac{1}{t^2(1-t)}$ 与 $y = \frac{1}{t(1-t)}$，可以算得 $3t - 2\ln |t| + C = \frac{3y}{x} - 2\ln |\frac{y}{x}| + C$.

类似地，对 $y = \sqrt{ax^2 + bx + c}$，我们会考虑第一类 Euler 替换 $y = t - \sqrt{a}x$ 与第二类 Euler 替换 $y = xt + \sqrt{c}$.

## 附加
### 往年题
{% admonition(type="question", title="2019 P3") %}
设 $f(x) = (\arcsin x)^2$，求 $f^{(n)}(0)$.
{% end %}

首先推导出 $(1-x^2)f''(x) - xf'(x) = 2$，对于 $n > 3$，对它求 $n-2$ 阶到得到：

$$\sum_{k=0}^{n-2} \binom{n-2}{k} [(1-x^2)]^{(k)} [f''(x)]^{(n-2-k)} - \sum_{k=0}^{n-2} \binom{n-2}{k} [x]^{(k)} [f'(x)]^{(n-2-k)} = 0$$

这给出：

$$f^{(n)}(0) = (n-2)^2 f^{(n-2)}(0)$$

{% admonition(type="question", title="2019 P8") %}
计算：

$$\int \frac{x^3 \arcsin x}{\sqrt{1-x^2}} \mathrm{d}x$$
{% end %}

首先积：

$$
\int \frac{x^3}{\sqrt{1-x^2}} \mathrm{d}x =
\frac{1}{2} \int \frac{x^2}{\sqrt{1-x^2}} \mathrm{d}(x^2) =
-\sqrt{1-x^2} + \frac{1}{3}(1-x^2)\sqrt{1-x^2} + C
$$

然后进行：

$$
\int \frac{x^3 \arcsin x}{\sqrt{1-x^2}} \mathrm{d}x =
\int \arcsin x \cdot \mathrm{d}\left(-\sqrt{1-x^2} + \frac{1}{3}(1-x^2)\sqrt{1-x^2}\right) =
\arcsin x \left(-\sqrt{1-x^2} + \frac{1}{3}(1-x^2)\sqrt{1-x^2}\right) - \int \left[-\sqrt{1-x^2} + \frac{1}{3}(1-x^2)\sqrt{1-x^2}\right] \mathrm{d}(\arcsin x) =
\arcsin x \left(-\sqrt{1-x^2} + \frac{1}{3}(1-x^2)\sqrt{1-x^2}\right) + \frac{2x}{3} + \frac{x^3}{9} + C
$$

{% admonition(type="question", title="2021 P4") %}
计算：

$$\int \frac{1 - \sin x}{1 + \sin x} \mathrm{d}x$$
{% end %}

考虑万能公式代换 $x = 2\arctan t$ 得：

$$
\int \frac{1 - \sin x}{1 + \sin x} \mathrm{d}x =
2\int \frac{2}{(t+1)^2} - \frac{1}{t^2+1} \mathrm{d}t =
-\frac{4}{t+1} - 2\arctan t =
-\frac{4}{1+\tan \frac{x}{2}} - x
$$

{% admonition(type="question", title="2023 P9") %}
$f \in C_{[-1, 1]}^\infty$ 且总有 $f^{(n)}(0) = 0$，且存在常数 $C$ 使得对任意自然数 $n$ 有：

$$\sup_{-1 \leq x \leq 1} |f^{(n)}(x)| \leq n! C^n$$

证明：在 $[-1, 1]$ 上 $f(x) \equiv 0$.
{% end %}

作业题常见的方法。如果不恒为 $0$，取一个 $f(\lambda) = q \neq 0$ 使 $\frac{1}{|\lambda|} > C$，然后考虑带 Lagrange 余项的 Taylor 公式。

{% admonition(type="question", title="2023 P10 (1)") %}
求不定积分：

$$\int \frac{x^2}{(x\sin x + \cos x)^2} \mathrm{d}x$$
{% end %}

猜测结果的分母，然后凑出结果（没有找到别的做法）：

$$\frac{-x\cos x + \sin x}{x\sin x + \cos x}$$

{% admonition(type="question", title="2023 P10 (2)") %}
求极限：

$$\lim_{n \to +\infty} \prod_{k=1}^n \cos \frac{k}{n^{3/2}}$$
{% end %}

泰勒展开到足够的项数即可。

### 习题课
记录一些整个学期习题课中出现的不显然或需要技巧的结论（这包括了上半学期）。

{% admonition(type="question", title="W3 1") %}
若 $f(x)$ 在 $(0, +\infty)$ 上有定义，证：若 $\frac{f(x)}{x}$ 单调下降，则 $f(a+b) \leq f(a)+f(b)$.
{% end %}

这是通过⭐配系数：

$$\frac{f(a+b)}{a+b} = \frac{a}{a+b} \frac{f(a+b)}{a+b} + \frac{b}{a+b} \frac{f(a+b)}{a+b} \leq \frac{a}{a+b} \frac{f(a)}{a} + \frac{b}{a+b} \frac{f(b)}{b} = \frac{f(a)+f(b)}{a+b}$$

{% admonition(type="question", title="W3 6") %}
设 $\lim_{n\to\infty} a_n = a$，正项数列 $\\{p_n\\}$ 满足 $\lim_{n\to\infty} p_n / (p_1 + \cdots + p_n) = 0$，证：

$$\lim_{n\to\infty} \frac{p_1a_n + p_2a_{n-1} + p_na_1}{p_1 + \cdots + p_n} = a$$
{% end %}

不妨 $a=0$. 对 $\epsilon > 0$ 取 $N$ 使 $n > N$ 时 $|a_n| < \epsilon$，则考虑：

$$\lim_{n\to\infty} \frac{p_1a_n + p_2a_{n-1} + p_na_1}{p_1 + \cdots + p_n} = \frac{p_1a_n + p_2a_{n-1} + p_{n-N}a_{N+1}}{p_1 + \cdots + p_n} + \frac{p_{n-N+1}}{p_1 + \cdots + p_n}a_N + \cdots + \frac{p_n}{p_1 + \cdots + p_n}a_1$$

⭐固定 $N$，然后取充分大的 $n$ 放右侧即可。

{% admonition(type="question", title="W9 6") %}
$f$ 在 $(0, +\infty)$ 上连续，且对任意 $a > 0$ 有 $\lim_{n\to\infty} f(na) = 0$，证：

$$\lim_{x\to+\infty} f(x) = 0$$
{% end %}

假设结论不成立，存在 $\epsilon > 0$ 使得一列递增趋向于无穷的 $x_i$ 满足 $|f(x_i)| > \epsilon$，由连续性存在一列 $[x_i-\delta_i, x_i+\delta_i]$ 使 $|f(x)|$ 在其上大于 $\epsilon/2$.

现在我们希望找到无穷多个不同的 $n_i$ 对应的 $[(x_i-r_i)/n_i, (x_i+r_i)/n_i]$ 包含某个 $a$. 先把 $[x_1-\delta_1, x_1+\delta_1]$ 移到 $(0, 1)$ 中，然后对充分大的 $x_i$ 存在对应的 $n_i$ 使得 $x_i/n_i$ 在其内部；不断作此操作，得到一闭区间套，使用闭区间套定理即可。

{% admonition(type="question", title="W9 9") %}
考虑 $[0, 1]$ 到自身的保定向自同胚构成的集合：

$$\mathrm{Hom}^+ [0, 1] = \\{f: [0, 1] \stackrel{1:1}{\to} [0, 1], f \in C[0, 1], f(0)=0, f(1)=1 \\}$$

设 $f, g \in \mathrm{Hom}^+ [0, 1]$ 满足对任意 $0 < x < 1$ 有 $f(x), g(x) > x$，证存在 $h \in \mathrm{Hom}^+ [0, 1]$ 使得 $h^{-1} \circ f \circ h = g$.
{% end %}

任给 $a \in (0, 1)$，有函数迭代 $\\{f^n(a)\\}$ 与 $\\{g^n(a)\\}$ 为严格增序列，且 $n\to+\infty$ 与 $n\to-\infty$ 时极限为 $1$ 与 $0$.

取定线性双射 $l: [a, f(a)] \to [a, g(a)]$，则 $h$ 可以表为：

$$
h(x) = \begin{cases}
x & x = 0, 1 \\\\
f^n(l(g^{-n}(x))) & x \in [g^n(a), g^{n+1}(a)]
\end{cases}
$$

{% admonition(type="question", title="W12 4") %}
设 $y = (1 + \sqrt{x})^{2n+2}$，求 $y^{(n)}(1)$.
{% end %}

令 $z = (1 - \sqrt{x})^{2n+2}$，有 $z^{(n)}(1) = 0$，故而 $y^{(n)}(1) = (y + z)^{(n)}(1) = 4(n+1)(n+1)!$.

{% admonition(type="question", title="W13 4") %}
$f \in C[0, 1], D(0, 1)$ 且 $f(0) = 0, f(1) = 1$. 设 $k_1 + \cdots + k_n = 1$ 是正数，证明存在互不相同的 $t_1, \cdots, t_n \in (0, 1)$ 使得：

$$\frac{k_1}{f'(t_1)} + \cdots + \frac{k_n}{f'(t_n)} = 1$$
{% end %}

由介值性，存在 $0 < x_1 < \cdots < x_{n-1} < 1$ 使得：

$$f(x_1) = k_1, \cdots, f(x_{n-1}) = k_1 + \cdots + k_{n-1}$$

在两两之间使用 Lagrange 中值定理即可。

{% admonition(type="question", title="W13 5") %}
$f$ 在 $[a, b]$ 上存在 $n + 1$ 阶导数，满足对 $k = 0, 1, \cdots, n$ 有 $f^{(k)}(a) = f^{(k)}(b) = 0$.

证明：
1. 存在 $\xi \in (a, b)$ 使得 $f(\xi) = f^{(n+1)}(\xi)$
2. 假设 $b - a \leq \pi$，则存在 $\eta \in (a, b)$ 使得 $f(\eta) = -f^{(n+1)}(\eta)$
{% end %}

对第一问，⭐首先 $h(a) = h(b) = 0$ 时期间有某个 $h(\xi) = h'(\xi)$，这是因为可以对 $e^{-x}h(x)$ 使用 Rolle 中值定理。然后令 $g(x) = \sum_{i=0}^n f^{(i)}(x)$ 就有 $g(a) = g(b) = 0$，存在 $g(\xi) - g'(\xi) = f(\xi) - f^{(n+1)}(\xi) = 0$.

对第二问，当 $n$ 为偶时可仿照第一问（对 $e^xh(x)$ 使用 Rolle 中值定理）。当 $n$ 为奇时，令 $\omega = e^{\mathrm{i}\frac{\pi}{n+1}}$ 及 $g(x) = \sum_{i=0}^n \omega^i f^{(i)}(x)$. 有 $g(x) - \omega g'(x) = f(x) + f^{(n+1)}(x)$.

令 $h(x) = \mathrm{Re}(e^{-x/\omega }g(x))$，有：

$$h'(x) = e^{-x\cos \frac{\pi}{n+1}} \cdot \cos \left(x\sin\frac{\pi}{n+1} - \frac{\pi}{n+1}\right) \cdot (f(x) + f^{(n+1)}(x))$$

使用平移不变性设 $a = (-\frac{\pi}{2} + \frac{\pi}{n+1})/\sin \frac{\pi}{n+1}$ 即得结论。

{% admonition(type="question", title="W13 9 Bellman-Grönwall 不等式") %}
$f$ 在 $[a, +\infty)$ 连续，在 $(a, +\infty)$ 可导，且存在常数 $c$ 满足对任意 $x \in (a, +\infty)$ 有：

$$f'(x) \leq cf(x)$$

证明对任意 $x \in [a, +\infty)$ 有 $f(x) \leq f(a) e^{c(x-a)}$
{% end %}

令 $g(x) = f(a) e^{c(x-a)}$，算得 $\left(\frac{f(x)}{g(x)}\right)' \leq 0$.

{% admonition(type="question", title="W14 2") %}
证明以下命题：
1. $f$ 在 $(0, +\infty)$ 上可导，$a > 0$，若有 $\lim_{x\to+\infty} [af(x) + f'(x)] = l$ 则 $\lim_{x\to+\infty} f(x) = l/a$
2. $f$ 在 $(0, +\infty)$ 上二阶可导，若有 $\lim_{x\to+\infty} [f(x) + 2f'(x) + f''(x)] = l$ 则 $\lim_{x\to+\infty} f(x) = l$
3. $f$ 在 $(0, +\infty)$ 上二阶可导，若有 $\lim_{x\to+\infty} [f(x) + xf'(x) + f''(x)] = 0$ 则 $\lim_{x\to+\infty} f(x) = \lim_{x\to+\infty} f'(x) = \lim_{x\to+\infty} f''(x) = 0$
{% end %}

对第一问，使用洛必达法则：

$$\lim_{x\to+\infty} f(x) = \lim_{x\to+\infty} \frac{e^{ax}f(x)}{e^{ax}} = \lim_{x\to+\infty} \frac{e^{ax}(f'(x)+af(x))}{ae^{ax}} = \frac{l}{a}$$

第二问使用第一问的结论，看成 $\lim_{x\to+\infty} [f(x) + f'(x)] + [f(x) + f'(x)]' = l$.

对第三问：

$$\lim_{x\to+\infty} f(x) = \lim_{x\to+\infty} \frac{e^{x^2/2}f(x)}{e^{x^2/2}} \stackrel{\text{洛}}{=} \lim_{x\to+\infty} \frac{xf(x)+f'(x)}{x} \stackrel{\text{洛}}{=} \lim_{x\to+\infty} [xf(x) + f'(x)]' = 0$$

$$\lim_{x\to+\infty} xf'(x) = \lim_{x\to+\infty} \frac{e^{x^2/2}f'(x)}{e^{x^2/2}/x} \stackrel{\text{洛}}{=} \lim_{x\to+\infty} \frac{xf'(x)+f''(x)}{-\frac{1}{x^2}+1} = 0$$

{% admonition(type="question", title="W14 3") %}
$f$ 在 $[a, b]$ 上二阶可导，且 $f'(a) = f'(b) = 0$. 证明：存在 $\xi \in (a, b)$ 使得：

$$|f''(\xi)| \geq \frac{4}{(b-a)^2} |f(b)-f(a)|$$
{% end %}

在 $\frac{a+b}{2}$ 处对 $a$ 与 $b$ 使用带 Lagrange 余项的 Taylor 公式。

{% admonition(type="question", title="W14 5") %}
$f$ 在 $(x_0-\delta, x_0+\delta)$ 上 $n$ 阶可导，且 $f''(x_0) = \cdots = f^{(n-1)}(x_0) = 0, f^{(n)}(x_0) \neq 0$. 当 $0 < |h| < \delta$ 时存在 $0 < \theta(h) < 1$ 使得 $f(x_0+h)-f(x_0) = hf'(x_0+\theta(h)h)$，证明：

$$\lim_{h\to 0} \theta(h) = \frac{1}{n^{\frac{1}{n-1}}}$$
{% end %}

考虑 Peano 余项的 Taylor 公式：

$$f'(x_0+\theta(h)h) = f'(x_0) + \frac{f^{(n)}(x_0)}{(n-1)!}(\theta(h)h)^{n-1} + o(h^{n-1})$$

有：

$$\frac{f'(x_0+\theta(h)h) - f'(x_0)}{h^{n-1}} = \frac{f^{(n)}(x_0)}{(n-1)!}\theta(h)^{n-1} + o(1)$$

又 $h \to 0$ 时左式等于 $\frac{f^{(n)}(x_0)}{n!} + o(1)$. 即得结论。

### 讲义阅读
来自于品的数学分析一讲义。该讲义内容相当丰富。

{% admonition(type="theorem", title="Riemann 重排定理") %}
若 $\sum_{n=1}^\infty a_n$ 收敛但不绝对收敛，则可以将它重新排列，使之收敛到预先指定的扩展实数系中的 $\alpha$.
{% end %}

令非负项按顺序排列是 $\\{x_n\\}$，负项按顺序排列是 $\\{y_n\\}$，则有 $\lim_{n\to\infty} x_n = \lim_{n\to\infty} y_n = 0$ 及 $\sum_{n=1}^\infty x_n = +\infty$，$\sum_{n=1}^\infty y_n = -\infty$.

对 $\alpha\in\mathbb{R}$，我们这样构造新序列：先按顺序填入非负项直到 $\geq \alpha$，再按顺序填入负项直到 $\leq \alpha$，重复进行此操作。

对 $\alpha=+\infty$，我们先按顺序填入非负项直到 $\geq 1$，然后填入一个负项，再按顺序填入非负项直到 $\geq 2$，依此类推[^infinite-ball-problem]；$\alpha=-\infty$ 的方法类似。

{% admonition(type="theorem", title="逐项求导定理") %}
$I = [a, b]$ 是闭区间，$\\{f _k\\} _{k\geq 0}$ 是一列 $C^1(I)$ 的函数，设 $\sum _{k=0}^\infty f _k$ 在 $I$ 上逐点收敛，如果 $\sum _{k=0}^\infty f _k'(x)$ 在 $I$ 上一致收敛，那么 $f$ 可导并且 $f'(x) = \sum _{k=0}^\infty f _k'(x)$.
{% end %}

首先，一致收敛可以推出 $\sum _{k=0}^\infty \\|f _k'\\| _\infty$ 收敛（其中 $\\|f\\| _\infty = \sup _{x\in I} |f(x)|$），这可以通过一致收敛的定义与闭区间套定理得到。

然后设 $g(x) = \sum _{k=0}^\infty f _k'(x)$，去讨论 $\int_x^{x_0} g(x)$. 没有找到不使用微积分基本定理的方法。

{% admonition(type="theorem", title="Émile Borel 引理") %}
对任意给定数列 $\\{a_n\\}_{n\geq 0}$，存在光滑函数 $f$ 使得 $f^{(n)}=a_n$.
{% end %}

考虑函数：

$$
\phi(x) = \begin{cases}
e^{-\frac{1}{x^2}} & x > 0 \\\\
0 & x \leq 0
\end{cases}
$$

$$\chi(x) = \frac{\phi(2-|x|)}{\phi(2-|x|) + \phi(|x|-1)}$$

$\chi(x)$ 在 $|x| \leq 1$ 取值是 $1$，在 $|x| \geq 2$ 取值是 $0$，⭐且是光滑的。

现在考虑：

$$f_k(x) = \frac{a_k}{k!}x^k \chi(t_kx)$$

有：

$$
f_k^{(n)}(0) = \begin{cases}
a_k & n = k \\\\
0 & n \neq k
\end{cases}
$$

令 $f(x) = \sum_{k=0}^\infty f_k(x)$ 即是所求，可以证明满足逐项求导定理条件，其中用到 $k \geq 2n$ 时：

$$f_k^{(n)}(x) = a_k \sum_{l=0}^n \binom{n}{l} \frac{t_k^{n-l}}{(k-l)!}x^{k-l}\chi^{(n-l)}(t_kx)$$

---

Peano 的想法是，考虑：

$$
\left(\frac{c_kx^k}{1+b_kx^2}\right)^{(n)}(0) = \begin{cases}
n!(-1)^jc_{n-2j}b_{n-2j}^j & k = n - 2j, j \in \mathbb{Z}_{\geq 0} \\\\
0 & \text{else}
\end{cases}
$$

适当选取时下式满足条件：

$$f(x) = \sum_{k=0}^\infty \frac{c_kx^k}{1+b_kx^2}$$

{% admonition(type="theorem", title="Baire 纲定理") %}
$U_n$ 是完备的度量空间 $(X, d)$ 中稠密的开集，则 $U_\infty = \bigcap_{n=1}^{\infty} U_n$ 稠密。
{% end %}

任取 $x_0 \in X$ 与 $\varepsilon_0 > 0$，我们将找到 $x_\infty \in U_\infty$ 使得 $d(x_\infty, x) < 2\varepsilon_0$.

我们知道存在 $x_1 \in U_1$ 使 $d(x_1, x_0) < \varepsilon_0$，而由开集条件可以找到 $B(x_1, 2\varepsilon_1) \subset U_1$，可以进一步要求 $2\varepsilon_1 < \varepsilon_0$.

重复此操作，可找到一列 $\\{x _n\\} _{n\geq 0}$ 与 $\\{\varepsilon_n\\} _{n\geq 0}$ 使得：
- $x_{n+1} \in U_{n+1}$
- $d(x_{n+1}, x_n) < \varepsilon_n$
- $B(x_{n+1}, 2\varepsilon_{n+1}) \subset U_{n+1}$
- $2\varepsilon_{n+1} < \varepsilon_n$

现在由 $\\{x _n\\} _{n\geq 0}$ 是 Cauchy 列及 $X$ 完备，存在极限 $\lim _{n\to\infty} x _n = x _\infty$ 即为所求。

---

[^infinite-ball-problem]: 这让我想到一个佯谬：房间里有一个罐子，第 $k$ 次往里面放入 $m > 1$ 个有编号（编号为 $(k-1)m+1, \cdots , km$）的小球，然后取出编号为 $k$ 的小球。假设第一次用时 $t$，之后每次用时是上一次的一半，则经过 $2t$ 时间后的结果是什么？一方面，从数量上看应该罐子中有无穷多个小球；与此同时，任何一个有编号的小球都应该在某一刻被取出了。对此的解释是，数学上根本无法定义无穷多次查找的结果；相应地，物理上就应该不容许这种行为成立。
