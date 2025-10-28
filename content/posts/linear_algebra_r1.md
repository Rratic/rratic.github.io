+++
title = "高等代数Ⅰ期中复习笔记"
description = "可能有用的小结论及往年题选做。"
date = 2025-10-28

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

$$E_{ij} = E_{ii}E_{jj}-E_{jj}E_{ii}$$

$$E_{ii}-E_{i+1,i+1} = E_{i,i+1}(E_{i,i+1}+E_{i+1,i}) - (E_{i,i+1}+E_{i+1,i})E_{i,i+1}$$

可由后者表出。

## 往年题选做
{% admonition(type="question", title="2017 P5") %}
$V$ 为 $n$ 维 $F$-线性空间，$T_1, T_2\in L(V)$，证 $|\dim\ker (T_1T_2) - \dim\ker (T_2T_1)|\leq \frac{n}{2}$.
{% end %}

此题只是形式上吓人，考虑

$$\dim\ker (T_1T_2)\leq \dim\ker (T_1) + \dim\ker (T_2)\leq 2\dim\ker (T_2T_1)$$

即可。我们回顾前一个不等号是因为：

令 $T_0=T_2|_{\ker (T_1T_2)}$，则 $\dim\ker (T_1T_2) = \dim\ker(T_0) + \dim\mathrm{Im}(T_0)$，而 $\ker(T_0)=\ker(T_2)$ 且 $\mathrm{Im}(T_0)\subseteq\ker(T_1)$.

{% admonition(type="question", title="2019 P1") %}
实矩阵 $A=\begin{pmatrix} 1 & 1 & 1 \\\\ 0 & 1 & 1 \\\\ 0 & 0 & 1 \end{pmatrix}$，令 $V_n = \mathrm{span}(A^n, A^{n+1})$.
1. 求 $\dim V_6$.
2. 求 $\dim (V_6\cup V_8)$.
{% end %}

只需考虑到结论

$$\mathrm{rank}(A)+\mathrm{rank}(B)-n\leq \mathrm{rank}(AB)\leq \min \\{\mathrm{rank}(A), \mathrm{rank}(B)\\}$$

就有 $A^n$ 是可逆的（或者直接取逆为 $(A^{-1})^n$），从而有 $A^nf(A)=A^ng(A)\iff f(A)=g(A)$.

{% admonition(type="question", title="2022 P3") %}
域 $F$ 上 $A\in F^{n\times n}, B=A^n, \alpha\in F^{n\times 1}$，设 $B^3\alpha = B^2\alpha$，则 $B^2\alpha = B\alpha$.
{% end %}

去证 $\ker B^2 = \ker B$，这是因为：
* $\dim\ker A^{m+1}\geq\dim\ker A^m$
* $\ker A^{m+1} = \ker A^m \implies \ker A^{m+2} = \ker A^{m+1}$
