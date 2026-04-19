+++
title = "高等代数Ⅱ期中复习笔记"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "线性代数"]
+++

本文用于准备高等代数Ⅱ的期中考试。这半学期的内容是推进到准素循环分解，其动机是尽可能精细的直和分解。

<!-- more -->

## 引入
### 基本概念
外直和定义略去，当以下映射：

$$
\begin{aligned}
	\Phi: \bigoplus_{i=1}^k V_i & \to V \cr
	(\alpha_1, \cdots, \alpha_k) & \mapsto \alpha_1 + \cdots + \alpha_k
\end{aligned}
$$

为单射时称 $\sum_{i=1}^k V_i$ 为内直和。

### 特征值
对 $T \in L(V)$ 考察：

$$V_c = \ker (c \cdot \mathrm{id}_V - T)$$

它不是零空间时称 $c$ 是**特征值**，该空间是关于 $c$ 的**特征子空间**，其中非零向量是（关于 $c$ 的）**特征向量**。称 $T$ 的所有特征值构成集合 $\sigma(T)$ 为**谱集**。

对矩阵 $A = [T]_\mathcal{B}$ 我们记它的**特征多项式**是 $F$ 上的多项式函数：

$$f_A = \det (xI_n - A)$$

其基本性质易见，且有谱集与 $f_A$ 根的集合相等。我们记**代数重数** $m(c, f)$ 是最高的 $(x-c)^k \mid f$ 次数，**几何重数**是 $\dim V_c$, 则：

$$\dim V_c \leq m(c, f)$$

这是因为我们可以取 $V_c$ 的基，把它扩充成 $V$ 的基，看矩阵的行列式计算。

我们考虑 $T$ 的所有零化多项式：

$$M_T = \set{f \in F[x] | f(T) = 0}$$

它存在唯一的首一生成元，称为 $T$ 的**极小多项式** $p_T$.

{% admonition(type="theorem", title="Cayley-Hamilton 定理") %}
$$p_T \mid f_T$$
{% end %}

我们考察 $F[x]$-模，对 $f \in F[x]$ 及 $\alpha \in V$ 定义：

$$f\alpha = f(T) \alpha$$

取 $T$ 在基 $\mathcal{B} = \set{\alpha_1, \cdots, \alpha_n}$ 下的矩阵为 $A$.

先证断言：对 $g \in F[x]$ 如果存在 $B \in F[x]^{n\times n}$ 使得 $(xI_n-A)B = gI_n$, 则 $g(T) = 0$. 这是因为：

$$(0, \cdots, 0) = ((\alpha_1, \cdots, \alpha_n)(xI_n - A)) B = (\alpha_1, \cdots, \alpha_n)(gI_n)$$

又 $f_T$ 符合断言中的条件，因为可以取 $B = \operatorname{adj} (xI_n - A)$. 从而 $f_T \in M_T$.

{% admonition(type="theorem", title="可对角化") %}
$T$ 可对角化，当且仅当 $p_T$ 为 $F[x]$ 中互不相同首一一次式乘积。
{% end %}

容易证明（读者注意采取线性映射视角！）。

在此基础上，让 $P$ 是那些对应的特征向量从左到右排成的矩阵，则 $P^{-1}AP$ 是对角阵，每一格是对应特征值。

### 不变子空间
$V$ 的子空间 $W$ 是 $T$-不变的，如果 $T(W) \subseteq W$. 并不总是存在 $T$-不变补空间（$Z$ 是 $T$-不变子空间且 $V = W \oplus Z$）。

考虑取 $W$ 的基，然后扩充为 $V$ 的基，可以观察到：

$$f_T = f_{T_W} \cdot f_{T_{V/W}}$$

$$\operatorname{lcm}(p_{T_W}, p_{T_{V/W}}) \mid p_T \mid p_{T_W} \cdot p_{T_{V/W}}$$

{% admonition(type="theorem", title="可同时对角化") %}
$T, U$ 可同时对角化当且仅当它们分别可对角化且交换。
{% end %}

只需证后推前。对 $\dim V$ 归纳。

当 $T$ 不为 $I$ 的常数倍时，按特征值分解：

$$V = \bigoplus V_{c_i}$$

由交换知每个 $V_{c_i}$ 都是 $U$-不变的，分别使用归纳假设。

{% admonition(type="theorem", title="可三角化") %}
以下条件等价：
1. $T$ 可（上）三角化
2. $f_T$ 为 $F[x]$ 中一次式之积（无需互异）
3. $p_T$ 为 $F[x]$ 中一次式之积（无需互异）
{% end %}

只需证 (3) 推 (1) 成立。往证存在 $T$-不变（全）旗，即：

$$0 = W_0 \subset W_1 \subset \cdots \subset W_n = V$$

其中 $\dim W_k = k$. 我们归纳构造 $W_k$. 考察：

$$p_{T_{V/W_{k-1}}} \mid p_T$$

在 $F$ 中有根，即是特征值，故有特征向量。

{% admonition(type="theorem", title="可同时三角化") %}
$F$ 代数闭域，其上 $\mathcal{F} \subseteq L(V)$ 中的映射两两交换，则 $\mathcal{F}$ 可同时三角化。
{% end %}

不妨设 $\mathcal{F}$ 有限，对 $\mathcal{F}$ 归纳。

{{ todo() }}

## 准素循环分解
### $T$-不变分解
即分解为 $T$-不变子空间的直和：

$$V = \bigoplus W_i$$

令 $T_i = T_{W_i}$, 读者易证 $p_T = \operatorname{lcm}(p_{T_1}, \cdots, p_{T_k})$ 及：

$$\ker T = \bigoplus \ker T_i$$

$$\operatorname{Im} T = \bigoplus \operatorname{Im} T_i$$

### 准素分解
当 $p_T$ 为素多项式的幂时，称 $T$ 准素。

{% admonition(type="theorem", title="准素分解") %}
设 $p_T = \prod_{i=1}^k p_i^{r_i}$ 其中 $r_i \geq 1$, $p_i$ 为不同的首一素多项式，记 $W_i = \ker (p_i^{r_i}(T))$, 则：

$W_i$ 非零，且 $T$-不变，且 $T_{W_i}$ 极小多项式 $p_i^{r_i}$. 且：

$$V = \bigoplus_{i=1}^k W_i$$
{% end %}

记 $f_i = p_T / p_i^{r_i}$, 则有：

$$
\begin{rcases}
	p_i^{r_i}(T) f_i(T) = 0 \cr
	f_i(T) \neq 0
\end{rcases} \implies \ker p_i^{r_i} \neq 0
$$

又，易见 $W_i$ 间线性无关，及 $p_{T_{W_i}} = p_i^{r_i}$.

### 循环分解
我们回顾证明 Cayley-Hamilton 定理时引入了模。令 $R = F[x]$ 使 $V$ 成为 $R$-模。记 $\alpha$ 生成的循环子空间：

$$R_\alpha = \set{f\alpha | f \in R}$$

若 $R_\alpha = V$, 则称 $\alpha$ 为**循环向量**，存在循环向量时称 $T$ 循环。

记 $M(\alpha) = \set{f \in R | f\alpha = 0}$ 的唯一首一生成元为 $\alpha$ 的**零化子** $p_\alpha$. 易见 $\dim R_\alpha = \deg p_\alpha$.

{% admonition(type="theorem", title="引理") %}
存在 $\alpha$ 使得 $p_\alpha = p_T$.
{% end %}

$$\set{\alpha | p_\alpha = p_T} = V \setminus \bigcup_{i=1}^k \ker f_i(T)$$

{{ todo() }}

易见 $T$ 循环当且仅当 $p_T = f_T$, 且在基 $\set{\alpha, T\alpha, \cdots, T^{n-1}\alpha}$ 下是多项式 $f_T$ 的友阵：

$$
\begin{pmatrix}
	0 &   & & & -a_0 \cr
	1 & 0 & & & -a_1 \cr
	& \ddots & \ddots & & \vdots \cr
	& & 1 & 0 & -a_{n-2} \cr
	& & & 1 & -a_{n-1} \cr
\end{pmatrix}
$$

这可以给出 Cayley-Hamilton 的另一证明。

{% admonition(type="theorem", title="循环分解") %}
对 $T \in L(V)$ 存在 $T$-不变分解 $V = \bigoplus V_i$ 满足 $V_i$ 非零，$T_{V_i}$ 循环，且记**不变因子** $p_i = p_{T_{V_i}}$ 有：

$$p_r \mid \cdots \mid p_1$$

这些不变因子的数量与内容被 $T$ 决定，且 $p_T = p_1, f_T = p_1 \cdots p_r$.
{% end %}

{{ todo() }}

{% admonition(type="definition", title="有理标准型") %}
使用循环分解，将友阵沿着对角线排出的就是有理标准型（从上到下编号 $1 \cdots r$ 要求 $p_r \mid \cdots \mid p_1$）。
{% end %}

对任意矩阵存在且唯一。

{% admonition(type="theorem", title="定理") %}
$A$ 与 $A^\top$ 相似。
{% end %}

写成有理标准型，使用：

$$\det (xI_k - C_g)^\top = \det (xI_k - C_g)$$

{% admonition(type="example", title="求有理标准型") %}
$$A = \begin{pmatrix} 5 & -6 & -6 \cr -1 & 4 & 2 \cr 3 & -6 & -4 \end{pmatrix} = \mathbb{Q}^{3\times 3}$$
{% end %}

算得 $f_A = (x-1)(x-2)^2$ 从而 $p_A = (x-1)(x-2)$. 故 $p_1 = (x-1)(x-2), p_2 = x-2$.

$$
\begin{pmatrix} C_{p_1} \cr & C_{p_2} \end{pmatrix} =
\left(\begin{array}{cc|c}
	0 & -2 \cr
	1 & 3 \cr
	\hline
	& & 2
\end{array}\right)
$$

现在找 $P$ 使 $P^{-1}AP$ 等于上述标准型。即找 $F^{3\times 1} = R\alpha_1 \oplus R\alpha_2$, 此时 $P = \begin{pmatrix} \alpha_1 & A\alpha_1 & \alpha_2 \end{pmatrix}$.

我们需要 $p_{\alpha_1} = p_1$ 即 $\alpha_1$ 不是特征向量，取 $\begin{pmatrix} 1 \cr 0 \cr 0 \end{pmatrix}$ 即可。另需要 $p_{\alpha_2} = p_2$ 是特征向量，且 $\alpha_2 \notin R\alpha_1$.

### 准素循环分解
回顾 $T$ 准素的等价条件是 $p_T$ 准素（这等价于 $f_T$ 准素）；而 $T$ 循环的等价条件是 $p_T = f_T$.

{% admonition(type="theorem", title="定理") %}
$T$ 不可分解当且仅当 $T$ 准素循环。
{% end %}

只需证准素循环时不可分解。假设可分解为非平凡的 $V_1 \oplus V_2$ 则：

$$f_1f_2 = f_T = p_T = \operatorname{lcm}(p_1, p_2) \mid \operatorname{lcm}(f_1, f_2)$$

{% admonition(type="theorem", title="准素循环分解") %}
存在 $T$-不变分解使得 $T_{V_i}$ 准素循环分解。记 $q_i = p_{T_{v_i}} = f_{T_{v_i}}$, 则**初等因子**序列 $q_1, \cdots q_s$ 在不计次序意义下唯一。
{% end %}

由于准素分解是严格唯一的，我们可以直接使用准素分解对应子空间与循环分解对应子空间的交。

{% admonition(type="theorem", title="定理") %}
循环子空间的不变子空间循环。
{% end %}

$$p_T \mid p_{T_W} \cdot p_{T_{R\alpha/W}} \mid f_{T_W} \cdot f_{T_{R\alpha/W}} \mid f_T$$

取等，故 $p_{T_W} = f_{T_W}$.

{% admonition(type="definition", title="Jordan 标准型") %}
Jordan 块 $J_n(c)$ 是指 $n$ 阶方阵：

$$
% TODO: 调整长宽比
\begin{pmatrix}
	c \cr
	1 & c \cr
	  & \ddots & \ddots \cr
	  & & 1 & c
\end{pmatrix}
$$

其满足 $f_T = p_T = (x-c)^n$.

将 Jordan 块沿着对角线排出的就是 Jordan 标准型。
{% end %}

当 $f_T$ 为一次式之积时可由准素循环分解写出 Jordan 标准型。

{% admonition(type="theorem", title="Jordan 分解") %}
若 $V$ 是代数闭的域 $F$ 上的线性空间，则对 $T \in L(V)$ 存在唯一 $D, N \in L(V)$ 使得 $T = D + N$, 其中 $D$ 可对角化，$N$ 幂零且 $D, N$ 可交换。
{% end %}

先证存在性。若写成 Jordan 标准型则易见；另一种取法是：设 $p_T = \prod (x-c_i)^{r_i}$, 记 $q_i = (x-c_i)^{r_i}$, 则由中国剩余定理存在多项式 $f$ 满足：

$$f \equiv c_i \pmod {q_i}$$

取 $D = f(T), N = T - D$ 即可。

再证唯一性：现在有 $D, N$ 是 $T$ 的多项式满足条件，若还有 $D'+N'$ 满足条件，则我们易推出 $D, D', N, N'$ 交换。有 $D, D'$ 可同时对角化，从而 $D - D'$ 可对角化，同时 $N - N'$ 幂零，有 $(D-D') + (N-N')$ 是 $0$ 的 Jordan 分解。故 $D = D'$.

## Remark
### 完全域
当 $F = \R$ 时，若 $A$ 在 $\Complex$ 上可以分解为 $D + N$, 则 $\bar{D} + \bar{N}$ 也是一个分解，则由唯一性知 $D, N$ 是实的。

这一结论可以推广到完全域（$K$ 完全域等价于 $K[x]$ 中无平方因子式在 $\bar{K}$ 中无重根，即 $a \in K \iff \sigma(a) = a, \forall \sigma \in \operatorname{Gal}(\bar{K}/K)$），因为可写作 $\sigma(A) = \sigma(D) + \sigma(N)$.

{% admonition(type="definition", title="单纯") %}
对有限维 $F$-线性空间 $V$ 及 $T \in L(V)$ 我们称：
1. $T$ **半单**，若 $V$ 的任一 $T$-不变子空间有 $T$-不变补空间
2. $T$ **单纯**，若 $V$ 无非平凡 $T$-不变子空间
{% end %}

实际上 $T$ 半单等价于 $p_T$ 无平方因式；$T$ 单纯等价于 $f_T$ 为素多项式。

### 小结论
{{ todo() }}
