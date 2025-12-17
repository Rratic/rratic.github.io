+++
title = "高等代数Ⅰ期中复习笔记"
description = "可能有用的小结论及往年题选做 & 考后总结。"
date = 2025-10-28
updated = 2025-11-04

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "线性代数"]
+++

本文用于准备高等代数实验班 2025 - 2026(I) 的期中考试。

## 作业及延申
{% admonition(type="question", title="Steinitz 替换引理") %}
若 $S = \\{\alpha_1,\cdots,\alpha_s\\}$ 线性无关，可由 $T = \\{\beta_1,\cdots,\beta_t\\}$ 线性表出，则可用 $S$ 替换 $T$ 中的 $s$ 个向量，使新的向量组与 $S$ 等价。
{% end %}

我们维护一个向量组，初始是 $T$，每次取出某个 $\alpha_i$，放入后整体变得线性相关，此时总是可以弹出一个 $\beta_j$.

{% admonition(type="question", title="习题 2.3:6") %}
$V$ 为 $n$ 维 $F$-线性空间，子空间 $M_1,\cdots,M_{n-1},N_1,\cdots,N_{n-1}$ 使 $\dim M_i = \dim N_i = i$，且 $M_i\subset M_{i+1}, N_i\subset N_{i+1}$，证明存在 $V$ 的基 $S$，使这些子空间均由它的子集生成。
{% end %}

同例题使用的证法，记 $M_0=N_0=\emptyset$，又记 $k_i$ 是使 $(M_i\setminus M_{i-1})\cap N_{k_i}\neq\emptyset$ 的最小下标，则 $k_i$ 互不相等。

取 $s_i\in (M_i\setminus M_{i-1})\cap (N_{k_i}\setminus N_{k_i-1})$ 即可。

{% admonition(type="question", title="AB-BA") %}
域 $F$ 上的迹 $0$ 方阵可以写成 $AB-BA$ 的形式。
{% end %}

全体迹 $0$ 方阵与全体可写成 $AB-BA$ 的方阵都构成线性空间，且后者是前者的子空间。

现在考虑前者的一组基（记 $E_{ij}$ 是指只有 $a_{ij}$ 为 $1$ 其余为 $0$ 的方阵）：
* 全体 $E_{ij}$，共 $n^2-n$ 个
* 全体 $E_{ii}-E_{i+1, i+1}$，共 $n-1$ 个

那么就有：

$$E_{ij} = E_{ii}E_{ij}-E_{ij}E_{ii}$$

$$E_{ii}-E_{i+1,i+1} = E_{i,i+1}(E_{i,i+1}+E_{i+1,i}) - (E_{i,i+1}+E_{i+1,i})E_{i,i+1}$$

可由后者表出。

## 往年题选做
{% admonition(type="question", title="2016 P7") %}
求最小正整数 $k$，使对任意 $A\in F^{9\times 9}, A^4 = 0$，存在 $B\in F^{9\times k}, C\in F^{k\times 9}$ 使 $A=BC$.
{% end %}

采取这样的视角：$B$ 把 $C$ 原来的一行变为原来所有行的线性组合（可以有任意多的系数为零），那么所求即 $\mathrm{rank}(A)$ 的最大值。

使用 $\mathrm{rank}(A^2)\geq 2\ \mathrm{rank}(A)-9, \mathrm{rank}(A^4)\geq 2\ \mathrm{rank}(A^2)-9$ 即可。

{% admonition(type="question", title="2017 P5") %}
$V$ 为 $n$ 维 $F$-线性空间，$T_1, T_2\in L(V)$，证 $|\dim\ker (T_1T_2) - \dim\ker (T_2T_1)|\leq \frac{n}{2}$.
{% end %}

此题只是形式上吓人，考虑

$$\dim\ker (T_1T_2)\leq \dim\ker (T_1) + \dim\ker (T_2)\leq 2\dim\ker (T_2T_1)$$

即可。我们回顾前一个不等号是因为：

令 $T_0=T_2|_{\ker (T_1T_2)}$，则 $\dim\ker (T_1T_2) = \dim\ker(T_0) + \dim\mathrm{Im}(T_0)$，而 $\ker(T_0)=\ker(T_2)$ 且 $\mathrm{Im}(T_0)\subseteq\ker(T_1)$.

又有 $\dim\ker (T_2) = n - \dim\mathrm{Im} (T_2) \leq n - \dim\mathrm{Im} (T_2T_1) = \dim\ker (T_2T_1)$.

{% admonition(type="question", title="2019 P1") %}
实矩阵 $A=\begin{pmatrix} 1 & 1 & 1 \\\\ 0 & 1 & 1 \\\\ 0 & 0 & 1 \end{pmatrix}$，令 $V_n = \mathrm{span}(A^n, A^{n+1})$.
1. 求 $\dim V_6$.
2. 求 $\dim (V_6\cup V_8)$.
{% end %}

只需考虑到结论

$$\mathrm{rank}(A)+\mathrm{rank}(B)-n\leq \mathrm{rank}(AB)\leq \min \\{\mathrm{rank}(A), \mathrm{rank}(B)\\}$$

就有 $A^n$ 是可逆的（或者直接取逆为 $(A^{-1})^n$），从而有 $A^nf(A)=A^ng(A)\iff f(A)=g(A)$.

{% admonition(type="question", title="2022 P2") %}
对域 $F$，一个 $F^{n\times n}$ 的子空间 $M$ 称为“优美”的，如果对任意 $A\in F^{n\times n}, B\in M$ 有 $AB\in M$，求“优美”的子空间维数的所有可能值。
{% end %}

采取这样的视角：$A$ 把 $B$ 原来的一行变为原来所有行的线性组合（可以有任意多的系数为零），那么一个子空间 $M$ 可以有至多 $n$ 个线性无关的行向量作为“素材”，对其中的元素，每一个素材在每一行有一个可变的系数取遍 $F$，故可能值为 $0, n, 2n, \cdots, n^2$.

{% admonition(type="question", title="2022 P3") %}
域 $F$ 上 $A\in F^{n\times n}, B=A^n, \alpha\in F^{n\times 1}$，设 $B^3\alpha = B^2\alpha$，则 $B^2\alpha = B\alpha$.
{% end %}

去证 $\ker B^2 = \ker B$，这是因为：
* $\dim\ker A^{m+1}\geq\dim\ker A^m$
* $\ker A^{m+1} = \ker A^m \implies \ker A^{m+2} = \ker A^{m+1}$

{% admonition(type="question", title="2023 P3") %}
$V$ 为 $n$ 维 $F$-线性空间，$T_1, T_2\in L(V)$，满足 $T_1\circ T_2=T_2\circ T_1, \mathrm{rank}(T_1)=\mathrm{rank}(T_2)$。证明：

$$\dim(\ker T_1+\mathrm{Im} (T_2)) = \dim(\ker T_2+\mathrm{Im} (T_1))$$
{% end %}

只需 $\ker T_1\cap\mathrm{Im} (T_2) = \ker T_1\circ T_2 = \ker T_2\circ T_1 = \ker T_2\cap\mathrm{Im} (T_1)$.

{% admonition(type="question", title="2023 P4") %}
$\mathbb{R}^{2023}$ 有子空间 $W_1+W_2 = W_2+W_3 = W_3+W_1 = \mathbb{R}^{2023}$，求 $\dim (W_1\cap W_2 + W_2\cap W_3 + W_3\cap W_1)$ 的所有可能值。
{% end %}

记 $\dim W_1\cap W_2\cap W_3 = k$，然后考虑 $\mathbb{R}^{2023}/(W_1\cap W_2\cap W_3)$（或者说，不妨填成前 $k$ 个分量）。将题目转化为：

$W_1+W_2 = W_2+W_3 = W_3+W_1 = \mathbb{R}^{2023-k}, \dim W_1\cap W_2\cap W_3 = 0$，求 $\dim (W_1\cap W_2 + W_2\cap W_3 + W_3\cap W_1) + k$ 的所有可能值。

有 $\dim (W_1\cap W_2 + W_2\cap W_3 + W_3\cap W_1) = \dim (W_1\cap W_2) + \dim (W_2\cap W_3) + \dim (W_3\cap W_1) = 2 (\dim W_1 + \dim W_2 + \dim W_3) - 3\times (2023-k)$，故取值为 $[1, 2023]$ 的奇数。

构造只需让 $k$ 为该值即可。

{% admonition(type="question", title="2024 P3") %}
整数 $n\geq 2$，有 $F$-线性空间的子空间 $V, W\subset F^{n\times n}$，且 $\dim V = \dim W = n^2-1$，则存在非零矩阵 $A$ 使得 $\forall B\in V, AB\in W$.
{% end %}

以下记 $A = (a_{ij})_{1\leq i,j\leq n}$.

设 $V = \\{M\mid \sum s_{ij}m_{ij} = 0\\}, W = \\{M\mid \sum t_{ij}m_{ij} = 0\\}$，则 $AB\in W\iff \sum t_{ij}(\sum_k a_{ik}b_{kj}) = 0$，也即 $\sum b_{ij}(\sum_k a_{ki}t_{kj}) = 0$.

只需 $A^TT = S$ 或 $A^TT = \mathbf{0}$，分类即可。

{% admonition(type="question", title="2024 P4") %}
是否存在 $\mathbb{R}^{2024}$ 的有限个二维子空间 $W_1,\cdots,W_k$，使对任意二维子空间 $M$ 有

$$M = \bigcap_{i=1}^k (W_i+M)$$

若存在，求 $k$ 最小值。
{% end %}

由 $M \neq \cap_{i=1}^k (W_i+M) \iff \exists \gamma\notin M, \forall 1\leq i\leq k, \dim W_i\cap (M+\gamma)>0$ 易知为 $4$.

## 考后总结
错了一道填空题，内容为：

{% admonition(type="question", title="2025 P1 (4)") %}
对 $\Omega = \mathbb{Z}\times\mathbb{Z}$，是否存在 $T\in L(\mathbb{R}^2)$，使 $T(\Omega+(\frac15, 0)) = \Omega+(\frac25, 0)$.
{% end %}

存在，取 $T: (x, y)\mapsto (2x+y, 5x+2y)$.
