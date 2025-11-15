+++
title = "模论（一）"
date = 2025-11-15

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "模论"]
+++

我们知道交换群的自同构是环（其中加法定义为按群运算 $f+g: x\mapsto f(x) + g(x)$，乘法定义为复合）。

类似于群作用，模就是环作用。

## 基本概念
### 基本定义
一个环 $A$ 上的模是 $(M, \varphi)$，其中 $M$ 是交换群，$\varphi: A\to \mathrm{End}(M)$ 是环同态。

若将其看作 $\varphi: A\times M\to M$（即左模），它将满足：
* 双线性
* 单位律 $1_Am = m$
* 结合律 $a(bm)=(ab)m$

相应地，若将环同态 $A^{op}\to \mathrm{End}(M)$ 看作 $\varphi: M\times A\to M$（右模），将满足：
* 双线性
* 单位律 $m1_A = m$
* 结合律 $(ma)b = m(ab)$

采取何种选择取决于上下文，本文中采取的是右模。

我们来看一些例子：
1. 域上的模是线性空间：对域 $F$ 和 F-模 $(V, +)$，有 $(V, +, \cdot)$ 构成线性空间，其中 $\alpha\cdot k = \varphi(\alpha, k)$
2. 交换群均可成为 $\mathbb{Z}$ 模：令 $\varphi(m, a) = m + m + \cdots + m$

读者可自行验证定义。

---

子模、直积、直和等定义可以参考群论的定义。

### 自由模
对幺环 $R$ 和 R-模 $M$，它的子集 $S$ 称为 M-**线性无关**的，如果对 $S$ 的任意有限子集 $\\{x_1, x_2, \cdots , x_n\\}$ 有 $x_1a_1+\cdots+x_na_n=0$ 蕴含 $a_1=\cdots=a_n=0$.

如果这个子集还构成 $M$ 的生成元，则称它为 R-**基**，有基的模称为**自由模**。

有限集 $S$ 是 $M$ 的一组 R-基实际上就是说

$$M = \bigoplus_{x\in S} xR$$

存在以下结论：

{% admonition(type="info", title="秩") %}
设 $R$ 是交换幺环，$M$ 是有限生成的自由 R-模，则它任意一族基的元素个数相等。称这个基的势为 $M$ 的**秩**，记作 $\mathrm{rank}_R(M)$.
{% end %}

易见 $M$ 的任意一组基只有有限个元素。

设它的一组基 $\\{x_1, x_2, \cdots , x_r\\}$，由 $R$ 是交换幺环，它有极大理想。取一个极大理想 $\mathfrak{M}$，令 $N = x_1\mathfrak{M}+\cdots +x_r\mathfrak{M}$，则它是 $M$ 的一个子模。

故商模 $M/N$ 可视为商环 $F = R/\mathfrak{M}$ 上的模，而由 $\mathfrak{M}$ 是极大理想，$F$ 是域，从而 $M/N$ 是 F-线性空间，且 $x_i$ 的像成为了它的一组基。然后使用线性空间上的结论即可。

## 主理想整环上的有限生成模
### 扭
对整环 $R$ 和 R-模 $M$ 及 $x\in M$，如果存在 $R$ 中非零元素 $a$ 使 $xa=0$，则称 $x$ 为**扭元素**，否则称为**自由元素**。所有元素都是扭元素的称为**扭模**，所有非零元素都是自由元素的称为**无扭模**。

{% admonition(type="abstract", title="自由") %}
主理想整环上的有限生成模是自由的当且仅当它是无扭的。
{% end %}

如果 R-模 $M$ 是自由的，不考虑零模，设有基 $x_1, \cdots , x_n$ 及 $xa=0$，有 $x=x_1a_1+\cdots+x_na_n$ 推出 $0=x_1a_1a+\cdots+x_na_na$，故 $a_i$ 全为零，矛盾。

反之，如果是无扭的，设有一组生成元 $x_1, \cdots , x_m$，极大线性无关组 $x_1, \cdots , x_r$，设它们生成模 $N$.

若 $r<m$，设 $x_1a_{j1}+\cdots+x_ra_{jr}+x_jb_j=0, j=r+1,\cdots,m$，我们令 $b=b_{r+1}\cdots b_m$，有 $Mb\subseteq N$ 为自由模，又 $\varphi: M\to Mb,\ x\mapsto xb$ 为模同构。

### 分解
我们先证明：

{% admonition(type="abstract", title="有限生成自由模的子模") %}
主理想整环上的有限生成自由模的子模仍为有限生成自由模。
{% end %}

先证子模是自由模，且秩不超过原来模的秩。

{{ todo() }}
