+++
title = "高等代数Ⅰ期末复习笔记"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "线性代数"]
+++

本文用于准备高等代数实验班 2025 - 2026(I) 的期末考试。

## 内容摘要
伴随矩阵 $\operatorname{adj} A$，我们回忆 $(\operatorname{adj} A) _{ij} = (-1)^{i+j} M _{ji}$，知它是整矩阵，$\det \operatorname{adj} A = (\det A)^{n-1}$.

## 往年题
{% admonition(type="question", title="2019 P1") %}
考虑实线性空间：

$$V = \\{f \in \mathbb{R}[x] | \deg (f) \leq 3\\}$$

定义 $T_1, T_2 \in L(V)$ 为：

$$T_1(f) = \sum_{k=0}^3 f(k) (x-2019)^k,\ T_2(f) = \sum_{k=0}^3 (D^kf)(x-2020)^k$$

求 $\det T_1$ 和 $\det T_2$.
{% end %}

对 $T_1$，取基 $\\{1, (x-2019), (x-2019)^2, (x-2019)^3\\}$，得到的是一个 Vandermonde 行列式，我们知道可化为 $\prod_{1 \leq i < j \leq n} (a_j - a_i)$.

$T_2$ 同理，得到对角矩阵。

{% admonition(type="question", title="2019 P2") %}
取定 $z \in \mathbb{C}$，有 $f, g \in \mathbb{Q}[x] \setminus \\{0\\}$ 满足 $f(z) = 0, g(z) \neq 0$，证：
1. 存在 $h_1 \in \mathbb{Q}[x]$ 使 $h_1(z) = g(z)^{-1}$
2. 存在 $h_2 \in \mathbb{Q}[x] \setminus \\{0\\}$ 使 $h_2(g(z)) = 0$
{% end %}

(1) 是因为可以对 $f$ 与 $g$ 作辗转相除，得到 $p(z)f(z) - q(z)g(z) = 1$.

(2) 是因为设 $\deg f = n$，则 $\\{h(z) | h \in \mathbb{Q}[x]\\} \subset \mathbb{C}$ 作为 $\mathbb{Q}$-线性空间是至多 $n$ 维的，从而 $1, g(z), \cdots, [g(z)]^n$ 线性相关。

{% admonition(type="question", title="2019 P5") %}
设 $V$ 有限维实线性空间，$W_1, \cdots, W_m \subset V$ 子空间，$\alpha_1, \cdots, \alpha_m \in V$，假设：

$$\dim W_i = \dim V - 1, \quad \bigcap_{i=1}^m W_i = \\{0\\}, \quad \alpha_i \notin W_i$$

设 $T(L(V))$ 满足如下条件：对任意 $i$ 存在 $j$ 使得 $T(\alpha_i + W_i) \subseteq \alpha_j + W_j$，证：$\det T = \pm 1$.
{% end %}

我们来刻画 $W_i$，取 $f_i$ 使 $f_i(\alpha_i) = 1$ 且 $f_i|_{W_i} \equiv 0$. 由条件知 $f_1, \cdots, f_m$ 构成 $V^\ast$ 的基，记它们构成集合 $E$.

考虑转置 $T^t$，我们回忆它是 $T^t(g) = g \circ T$，则对 $T(\alpha_i + \beta) = \alpha_j + \gamma$ 有 $(T^tf_j - f_i)(\alpha_i + \beta) = 0$，故有 $T^tf_j = f_i$.

现在我们知道 $T^t(E) = E$，故存在某个 $(T^t|_E)^k = \mathrm{id}$.

{% admonition(type="question", title="2020 P4") %}
$A \in \mathbb{Z}^{n\times n}$ 满足 $|\det A| = 2$，证存在 $Z \in \mathbb{Z}^{n\times 1}$，使得对任意 $Y \in \mathbb{Z}^{n\times 1}$ 存在 $X \in \mathbb{Z}^{n\times 1}$ 使得 $AX - Y$ 为 $\mathbf{0}$ 或 $Z$.
{% end %}

对 $n$ 维整列向量称 $v \sim w$ 如果 $v - w$ 是 $A$ 乘以某个 $n$ 维整列向量的值。考虑将 $\det A$ 视作体积考虑格点，知只有两个等价类。但是这不容易严格说明。

---

先证对 $A$ 可找到 $|\det P, \det Q| = 1$ 的整系数矩阵使得 $PAQ$ 是对角阵。首先，通过初等行变换进行辗转相除、交换两行可以得到一个上三角阵。此时对角线上除了一个是 $\pm 2$ 以外都是 $\pm 1$，可以通过初等行、列变换得到对角阵。

此时 $AX = Y$ 有解即于 $(PA)(QX) = PY$ 有解，易见。

{% admonition(type="question", title="2022 P7") %}
设 $V$ 为实线性空间 $\mathbb{R}^{2022}$，求正整数 $r$ 的最小值，使得存在 $L \in M^r(V)$，当 $T \in L(V)$ 满足 $L(T\alpha_1, \cdots, T\alpha_r) = L(\alpha_1, \cdots, \alpha_r)$ 时总有 $\det T = 1$.
{% end %}

$r = 1$ 时可强行分析。

对于 $r = 2$，设 $V^\ast$ 的基 $f_1, \cdots, f_{2n}$，令 $L = \sum_{k=1}^n f_{2k-1} \wedge f_{2k}$，则条件即：

$$\sum_{k=1}^n (T^t f_{2k-1}) \wedge (T^t f_{2k}) = \sum_{k=1}^n f_{2k-1} \wedge f_{2k}$$

两边对外积取 $n$ 次幂（即 $\underbrace{e\wedge \cdots \wedge e}_n$），有：

$$n! (T^t f_1)\wedge \cdots \wedge (T^t f_{2n}) = n! f_1\wedge \cdots \wedge f_{2n}$$

{% admonition(type="question", title="2022 P8") %}
考虑 $\mathbb{C}[x]$ 的子空间 $V = \mathrm{span}\\{x^{k^2} | k \in \\{0, 1, \cdots, 8\\}\\}$，求正整数 $n$ 的最小值，使得对 $\mathbb{C}$ 的任意 $n$ 元子集 $S$ 总存在 $z_1, \cdots, z_9 \in S$ 与 $f_1, \cdots, f_9 \in V$ 使得 $f_i(z_j) = \delta_{ij}$.
{% end %}

记 $L_z(f) = f(z)$，则对 $|S| = 65$ 有 $\bigcap_{z \in S} \ker(L_z) = \\{0\\}$，从而 $\mathrm{span}\\{L_z | z \in S\\} = V^\ast$，存在 $L_{z_1}, \cdots, L_{z_9}$ 为基。

另一方面，取 $x^{64}-1$ 的根知 $|S| = 64$ 不行。
