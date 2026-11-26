+++
title = "环论（二）：PID 上的有限生成模"
date = 2026-11-26

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

我们知道交换群的自同构是环（其中加法定义为按群运算，乘法定义为复合）。类似于群作用，模就是环作用。

为方便起见，本文只考虑交换环，不讨论左/右模。

<!-- more -->

## 模的定义
环 $R$ 上的模是 $(M, \varphi)$，其中 $M$ 是交换群，$\varphi: A \to \mathrm{End}(M)$ 是环同态。展开来说，就是要求 $r \cdot (s \cdot m) = (rs) \cdot m$；$1_R \cdot m = m$；$r \cdot (m + n) = r \cdot m + r \cdot n$ 及 $(r + s) \cdot m = r \cdot m + s \cdot m$.

读者可以验证：域上的模就是线性空间；$\Z$ 模就是一个交换群。

子模、直积、直和、商模等定义与同构定理略去。称秩 $n$ 的自由 $R$-模是指：

$$R^{\oplus n} = \underbrace{R \oplus \cdots \oplus R}_n$$

对于交换环的同态 $\rho: R \to S$，一个 $S$-模 $M$ 可以看作一个 $R$-模。这是通过 $r \cdot m = \rho(r) \cdot m$，特别地，$S$ 本身是一个 $R$-模，称为 $R$-代数。

模的同态就是 $\phi: M \to N$ 使得 $\phi(r \cdot m) = r \cdot \phi(m)$，此时实际上有 $\mathrm{Hom}_R(M, N)$ 是一个 $R$-模（这一点需要是交换环）。另外 $\ker \phi$ 与 $\operatorname{Im} \phi$ 是 $R$-模。

子模对标正规子群，可以定义不可约模，然后有模版本的 Jordan-Hölder 定理。

## 模的生成
对 $X \subseteq M$，定义 $M$ 的由 $X$ 生成的子模：

$$RX \coloneqq \set{r_1a_1 + \cdots + r_na_n | r_i \in R, a_i \in X}$$

称 $M$ 是秩 $n$、自由的，如果存在 $a_1, \dots, a_n \in M$，称为**基**，使得每个 $m \in M$ 可以唯一表示为 $r_1a_1 + \cdots + r_na_n$，也就是说 $R^{\oplus n} \cong M$.[^rank]

对于有限生成模 $M$，考虑以下映射，如果 $\ker \phi$ 是有限生成的，则称 $M$ 是**有限展示**（finitely presented）的：

$$
\begin{aligned}
    \phi: R^{\oplus n} &\longrightarrow M \\\\
        (r_1, \dots, r_n) &\longmapsto r_1a_1 + \cdots + r_na_n
\end{aligned}
$$

设域 $F$ 及其上向量空间 $V$，给定线性映射 $T: V \to V$，可以在 $V$ 上定义一个 $F[x]$-模结构（$V$ 有限维则它作为 $F[x]$-模有限生成）：

$$(a_nx^n + \cdots + a_0) \cdot v = a_nT^n(v) + \cdots + a_0v$$

## PID 上有限生成模
{% <theorem> %}
$R$ 是整环，则 $M = R^{\oplus n}$ 上任意 $n + 1$ 个元素线性相关。
{% </theorem> %}

设 $F = \mathrm{Frac}(R)$，则对 $n + 1$ 个元素 $x_1, \dots, x_{n+1}$ 存在分式域中不全为零的系数 $r_1', \dots, r_{n+1}'$ 使得求和为 $0$，乘以分母之积即可。

{% <theorem> %}
$R$ 是 PID，对 $M \cong R^{\oplus m}$ 的子模 $N$，
1. $N$ 自由，且秩 $n \leq m$
2. 存在 $M$ 的基 $y_1, \dots, y_m$ 及 $R$ 中非零的 $a_1 \mid a_2 \mid \cdots \mid a_n$，使得 $a_1y_1, \dots, a_ny_n$ 是 $N$ 的基
{% </theorem> %}

设 $N \neq \set 0$，我们来寻找 $a_1y_1$.

对任意 $R$-模同态 $\phi: M \to R$，有 $\phi(N)$ 是 $R$-子模，也就是 $R$ 的理想。考虑 $S = \set{\phi(N)}$，则 $S$ 包含一个极大理想[^maximal] $(a_1)$，设 $\phi_1(y) = a_1$，则由极大性 $a_1 \mid \phi(y)$.

固定一个 $M \cong R^{\oplus m}$，给出 $\pi_i: M \to R$，考虑 $\pi_i(y) = a_1b_i$，故 $y = (a_1b_1, \cdots, a_1b_m)$，由 $\phi_1(y) = a_1$，有 $\phi_1(y_1 \coloneqq (b_1, \cdots, b_m)) = 0$，我们断言：

- $M \cong \braket{y_1} \oplus \ker \phi_1$，可以考虑 $x = \phi_1(x) \cdot y_1 + (x - \phi_1(x) \cdot y_1)$，其中 $x - \phi_1(x) \cdot y_1 \in \ker \phi_1$
- $N \cong \braket{a_1y_1} \oplus (N \cap \ker \phi_1)$，因为 $\phi_1(x) \in (a_1)$，$x - \phi_1(x) \cdot y_1 \in N$

我们可以对 $N_1 = N \cap \ker \phi_1$ 继续进行操作，有 $N_1 = 0$ 或 $N_1 = \braket{a_2y_2} \oplus N_2$，由前一定理，会在 $\braket{a_1y_1} \oplus \cdots \oplus \braket{a_{n+1}y_{n+1}}$ 之前停止，有 (1) 得证。

对于 (2)，同样归纳地操作，得到：

$$
N = \braket{a_1y_1} \oplus \cdots \oplus \braket{a_my_m} \\\\
M = \braket{y_1} \oplus \cdots \oplus \braket{y_n}
$$

现在只需证明 $a_1 \mid a_2$，若不然，$d = a_1r_1 + a_2r_2$，考察 $R$-模同态：

$$
\begin{aligned}
\phi': M &\longrightarrow R \\\\
    y_1 &\longmapsto r_1 \\\\
    y_2 &\longmapsto r_2 \\\\
    \text{other } y_i &\longmapsto 0
\end{aligned}
$$

于是 $\phi'(a_1y_1 + a_2y_2) = d$，与 $\phi_1$ 最小性矛盾。

{% <theorem title="PID 上有限生成模的分类"> %}
$R$ 是 PID，$M$ 是有限生成 $R$-模，则：

$$M \cong R^{\oplus r} \oplus R/(a_1) \oplus \cdots \oplus R/(a_n)$$

其中 $a_1 \mid \cdots \mid a_n$；$r$ 唯一，称为**秩**，$a_1, \dots, a_n$ 在相伴意义下唯一。
{% </theorem> %}

其直接推论是 $M$ 无挠[^torsion-free]当且仅当它自由。另一推论是有限生成 Abel 群的结构定理。

由于 $M$ 有限生成，设生成元 $x_1, \dots, x_n$，则存在满射：

$$
\begin{aligned}
\phi: R^{\oplus n} &\longrightarrow M \\\\
    (a_1, \dots, a_n) &\longmapsto a_1x_1 + \cdots + a_nx_n
\end{aligned}
$$

则 $\ker \phi$ 是 $R^{\oplus n}$ 的子模，对它使用前一定理，有新的一组基，从而：

$$M \cong \frac{\braket{y_1} \oplus \cdots \oplus \braket{y_n}}{\braket{a_1y_1} \oplus \cdots \oplus \braket{a_my_m}} \cong R/(a_1)y_1 \oplus \cdots \oplus R/(a_m)y_m \oplus R^{\oplus n-m}$$

为了证明唯一性，考虑将每个 $a_i$ 作素因子分解。证明略。

---

[^rank]: 为了方便之后讨论，还需要这里的秩是唯一的。设一组基 $\set{a_1, \dots, a_n}$，取 $R$ 的一个极大理想 $\mathfrak{m}$，令 $N = a_1\mathfrak{m} + \cdots + x_n\mathfrak{m}$ 是 $M$ 的一个子模。有商模 $M/N$ 是域 $R/\mathfrak{m}$ 上的模，即线性空间。然后就可以使用线性空间上的结论。或者直接仿照 [Steinitz 替换引理](@/posts/linear_algebra_1_midterm.md)。或者使用之后即将证明的定理。
[^maximal]: 使用 PID 是 UFD 那里的证法。
[^torsion-free]: 对整环 $R$ 和 R-模 $M$，称 $x \in M$ 为**挠/扭元素**，如果存在 $R$ 中非零元素 $a$ 使 $xa = 0$；否则称为**自由元素**。所有元素都是挠元素的称为**挠/扭模**，所有非零元素都是自由元素的称为**无挠/扭**的。
