+++
title = "数学分析Ⅰ期末复习笔记"
description = "需要记忆的结论与 trick."
date = 2025-12-17

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "分析学", "微积分学"]
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
| $\sin x$ | $x-\frac{1}{3}x^3+\frac{1}{120}x^5+\cdots+(-1)^{n-1}\frac{x^{2n-1}}{(2n-1)!}$ |
| $\cos x$ | $1-\frac{1}{2}x^2+\frac{1}{24}x^4+\cdots+(-1)^{n}\frac{x^{2n}}{(2n)!}$ |
| $\ln (1+x)$ | $x-\frac{1}{2}x^2+\frac{1}{3}x^3+\cdots+(-1)^{n-1}\frac{x^n}{n}$ |
| $(1+x)^\alpha$ | $1+\alpha x+\frac{\alpha(\alpha-1)}{2}x^2+\cdots+\frac{\alpha(\alpha-1)\cdots(\alpha-n+1)}{n!}x^n$ |

$\tan x$，$\arcsin x$ 等较为复杂且写了也记不住，不再列举。

⭐可以直接用带 Peano 余项的 Taylor 公式来写加减法、乘法、除法（有时）、函数复合的结果。

⭐有时可以从表达式先提取出一个 $x^\alpha$，把剩下部分写成带 Peano 余项的 Taylor 公式。可以参考 Puiseux 级数相关。

{% admonition(type="theorem", title="带 Lagrange 余项的 Taylor 公式") %}
$f$ 在 $(a, b)$ 上 $n+1$ 阶可导，则对 $x, x_0 \in (a, b)$ 存在介于期间的 $\xi$ 使得：

$$f(x) = \sum_{k=0}^n \frac{f^{(k)}(x_0)}{k!} (x-x_0)^k + \frac{f^{(n+1)}(\xi)}{(n+1)!}(x-x_0)^{n+1}$$
{% end %}

可以反复使用 Cauchy 中值定理。

感觉 Cauchy 型余项之类的东西没什么用。

### 不定积分
一个需补充记忆的结论：

$$\int \frac{1}{\sqrt{1+x^2}} = \ln \left(x+\sqrt{1+x^2}\right) + C$$

这将给出：

$$\int \sqrt{1+x^2} = \frac{1}{2} \left(x\sqrt{x^2+1} + \ln \left(x+\sqrt{1+x^2}\right)\right) + C$$

又，⭐带绝对值的积分可以用 $\mathrm{sgn}$ 处理：

$$\int |\sin x - \cos x| \mathrm{d}x = (\sin x + \cos x)\ \mathrm{sgn}(\cos x - \sin x)$$

## 附加
### 往年题
{{ todo() }}

### 习题课
记录一些整个学期习题课中出现的不显然的结论。

{{ todo() }}

### 讲义阅读
来自于品的数学分析一讲义。该讲义内容相当丰富。

{% admonition(type="theorem", title="Riemann 重排定理") %}
若 $\sum_{n=1}^\infty a_n$ 收敛但不绝对收敛，则可以将它重新排列，使之收敛到预先指定的扩展实数系中的 $\alpha$.
{% end %}

令非负项按顺序排列是 $\\{x_n\\}$，负项按顺序排列是 $\\{y_n\\}$，则有 $\lim_{n\to\infty} x_n = \lim_{n\to\infty} y_n = 0$ 及 $\sum_{n=1}^\infty x_n = +\infty$，$\sum_{n=1}^\infty y_n = -\infty$.

对 $\alpha\in\mathbb{R}$，我们这样构造新序列：先按顺序填入非负项直到 $\geq \alpha$，再按顺序填入负项直到 $\leq \alpha$，重复进行此操作。

对 $\alpha=+\infty$，我们，$\alpha=-\infty$ 的方法类似。

{% admonition(type="theorem", title=" Émile Borel 引理") %}
对任意给定数列 $\\{a_n\\}_{n\geq 0}$，存在光滑函数 $f$ 使得 $f^{(n)}=a_n$.
{% end %}

{{ todo() }}

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
