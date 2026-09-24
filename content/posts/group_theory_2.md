+++
title = "群论（二）：合成群列与可解群"
date = 2025-06-16
updated = 2026-09-25

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

本文经过大幅重新组织。原本主要通过三阶魔方的例子及导群来引入，现在参考代数学Ⅰ的讲授方式，将合成群列作为第一部分并在之后展现关联。

<!-- more -->

## 群的分类
### Hölder 纲领
分类问题是数学中基本的问题。对群来说，分类就是讨论所有同构意义下不相同的群，然而这个目标过于难以实现。

我们不妨退一步，回忆如果 $N \triangleleft G$ 就可以有商群 $G/N$，这两者可以拼凑出群的部分信息。可以递归地做此操作，直到剩下的是：

{% <definition title="单群"> %}
一个非平凡群称为单群，如果它没有平凡子群及自身之外的正规子群。
{% </definition> %}

Hölder 最早提出对有限单群的分类计划，这跨越了漫长的时间，最终在本世纪（我们倾向于认为）完成。分类包括 18 个单群族（素数阶循环群、交错群及 16 族 Lie 型单群）与 26 个散在单群。

{% <theorem> %}
$A_n \\, (n \geq 5)$ 是单群。
{% </theorem> %}

强行讨论即可。

### 合成群列
我们将如下序列称为次正规群列，其中在每个商都是单群时称为**合成群列**（此时这些商称为**合成因子**）：

$$\set{e} = G_0 \triangleleft G_1 \triangleleft \cdots \triangleleft G_n = G$$

{% <theorem title="Schreier 定理"> %}
有限群的任何次正规群列可以加细为合成群列。
{% </theorem> %}

假设某群列中 $G_i/G_{i-1}$ 有非平凡正规子群 $H$，取其在以下典范同态下的原像，可以插在序列中：

$$
\begin{aligned}
\sigma: G_i & \longrightarrow G_i/G_{i-1} \cr
	g & \longmapsto gG_{i-1}
\end{aligned}
$$

{% <theorem title="Jordan-Hölder 定理"> %}
对两个合成群列：

$$
\set{e} = A_0 \triangleleft A_1 \triangleleft \cdots \triangleleft A_m = G \\\\
\set{e} = B_0 \triangleleft B_1 \triangleleft \cdots \triangleleft B_n = G \\\\
$$

有 $m = n$，且存在 $[n] \to [m]$ 的双射 $\sigma$，使 $A_{\sigma(i)} / A_{\sigma(i) - 1} = B_i / B_{i - 1}$.
{% </theorem> %}

我们先证明 Zassenhaus 引理：

$$\frac{A_{i-1}(A_i \cap B_j)}{A_{i-1}(A_i \cap B_{j-1})} \cong \frac{(A_i \cap B_j)B_{j-1}}{(A_{i-1} \cap B_j)B_{j-1}}$$

考虑如下函数：

$$\phi: A_i \cap B_j \to A_{i-1}(A_i \cap B_j) \twoheadrightarrow \frac{A_{i-1}(A_i \cap B_j)}{A_{i-1}(A_i \cap B_{j-1})}$$

使用第一同构定理即有下式，然后用对称性即可。

$$\frac{A_{i-1}(A_i \cap B_j)}{A_{i-1}(A_i \cap B_{j-1})} \cong \frac{A_i \cap B_j}{(A_{i-1} \cap B_j) \cdot (A_i \cap B_{j-1})}$$

回到原定理，令 $A_{ij}' = A_{i-1}(A_i \cap B_j), B_{ij}' = (A_i \cap B_j)B_{j-1}$，则：

$$
\set{e} = A_0 = A_{10}' \trianglelefteq A_{11}' \trianglelefteq \cdots \trianglelefteq A_{1n}' = A_1 = \cdots = A_m = G \\\\
\set{e} = B_0 = B_{01}' \trianglelefteq B_{11}' \trianglelefteq \cdots \trianglelefteq B_{m1}' = B_1 = \cdots = B_n = G
$$

我们去找突变的位置，设 $A_i / A_{i-1} \cong A_{i, \sigma(i)}' / A_{i, \sigma(i)-1}'$ 及 $B_j / B_{j-1} \cong B_{\tau(j), j}' / B_{\tau(j)-1, j}'$，有 $\sigma$ 与 $\tau$ 互逆，且据引理，

$$A_i / A_{i-1} = B_{\sigma(i)} / B_{\sigma(i)-1}$$

### 可解群
一个群称为**可解群**，如果它有一个次正规群列，每一个商都是交换群。回顾[交换群可以分解为循环群的直积](@/posts/group_theory_1.md)，此定义对有限群即所有合成因子均是素数阶循环群。

“可解”名称来自于之后会看到的[关于方程可解性的工作](@/posts/field_theory_1.md)。

{% <example> %}
以下上三角可逆矩阵构成的群是可解群：

$$
G = \left\\{
	\begin{pmatrix}
		\ast & \ast & \ast \cr
		0 & \ast & \ast \cr
		0 & 0 & \ast
	\end{pmatrix}
	\in \mathrm{GL}_3(\Complex)
\right\\}
$$
{% </example> %}

考虑下式，有 $G/N \cong (\Complex^\times, \cdot)^3$，$N/N' \cong (\Complex, +)^2$ 及 $N' \cong (\Complex, +)$：

$$
N = \left\\{
	\begin{pmatrix}
		1 & \ast & \ast \cr
		0 & 1 & \ast \cr
		0 & 0 & 1
	\end{pmatrix}
	\in \mathrm{GL}_3(\Complex)
\right\\}
$$

$$
N' = \left\\{
	\begin{pmatrix}
		1 & 0 & \ast \cr
		0 & 1 & 0 \cr
		0 & 0 & 1
	\end{pmatrix}
	\in \mathrm{GL}_3(\Complex)
\right\\}
$$

{% <theorem title="Hall 定理"> %}
有限群 $G$ 可解当且仅当对所有满足 $\gcd(n, |G|/n) = 1$ 的 $n \mid |G|$，$G$ 有 $n$ 阶子群。
{% </theorem> %}

证明超出本文范围。

{% <theorem title="Feit-Thompson 定理"> %}
每一个奇阶的有限群都是可解群。
{% </theorem> %}

此定理的证明长达 254 页，在此仅作告示之用。

## 换位子
### 魔方
不妨设每个面的中心固定，令 $G$ 表示魔方的变换群。

我们记六个面为上面 U，下面 D，左面 L，右面 R，前面 F 和后面 B（取首字母），以大写字母表示将该面顺时针旋转 90°. 显然有 $G \leq \braket{U, R, F, D, L, B}$.

魔方剩余可动的有两类：角块和棱块，它们两两不同，由于分别有位置和旋转状态，有：

$$G\leq Z_3^8\times S_8 \times Z_2^{12}\times S_{12}$$

接下来更精细地讨论哪些角块、棱块排布是合法的。

先讨论角块，一个角块的标准状态是一个（或三个）面上与中心块颜色相同，将其记作 0，顺时针转 120° 的状态记作 1，顺时针转 240° 的状态记作 2，那么所有标记之和在 mod 3 下不变。同理，棱块的标记之和在 mod 2 下不变。此外，角块的置换奇偶性与棱块的置换奇偶性一致。

同时，我们可以说明符合上述要求的是合法的。因此有：

$$|G| = \frac{1}{12} |Z_3^8\times S_8 \times Z_2^{12}\times S_{12}| = 43252003274489856000$$

回忆[半直积](@/posts/group_theory_1.md)理论，可以进一步将整个魔方群写为：

$$G\cong \set{(c, e) \in (Z_3^7 \rtimes S_8) \times (Z_2^{11} \rtimes S_{12}) | \mathrm{sgn}(\pi_c) = \mathrm{sgn}(\pi_e)}$$

### 换位子
考察如何解魔方。

定义**换位子**是 $[a, b] = aba^{-1}b^{-1}$. 它将满足：如果 $x^{[a, b]}\neq x$，则要么 $a\notin \mathrm{Stab}(x), b\notin \mathrm{Stab}(x^a)$，要么 $b\notin \mathrm{Stab}(x), a\notin \mathrm{Stab}(x^b)$.

例如，我们构造操作：$[[R, U], D] = RUR'U'DURU'R'D'$，这只会改变三个角块的状态。

Thislethwaite Method 将群逐步化为：
- $\braket{U, R, F^2, D, L, B^2}$
- $\braket{U, R^2, F^2, D, L^2, B^2}$
- $\braket{U^2, R^2, F^2, D^2, L^2, B^2}$
- $\set{e}$

Kociemba Algorithm 则将群逐步化为：
- $\braket{U, R^2, F^2, D, L^2, B^2}$
- $\set{e}$

### 一般算法
以上的解法依赖于具体的结构，这里提供一个通用方法：Schreier-Sims-Minkwits 算法。[^1]

我们希望进行这样的操作：每次多固定一个集合上的元素，其稳定化子就是原变换群的真子群，如此下去可以得到一个链 $G = G_0 > G_1 > \cdots > G_n = \set{e}$，而由于我们要写出一个操作序列，设第 $i$ 个阶段可能的操作为 $r_{i_1}, r_{i_2}, \dots$ 有 $r_{i_1}G_{i+1}, r_{i_2}G_{i+1}, \dots$ 陪集族构成 $G_i$.

{% <theorem title="Schreier 子群引理"> %}
$G$ 是一个由集合 $S$ 中元素（置换）生成的群，有子群 $H$，设（左）陪集代表元构成集合 $R$，其中元素 $g$ 对应代表元为 $\bar{g}$，则 $H$ 是由 $\set{\overline{sr}^{-1}sr | r\in R, s\in S}$ 生成的。
{% </theorem> %}

对 $H$ 的元素 $h = s_1s_2\cdots s_k$，其中 $s_i$ 为生成元，记 $t_i = \overline{s_{i+1}\cdots s_k}$，其中 $t_0 = t_k = e$，故有：

$$h = (t_0^{-1}s_1t_1)(t_1^{-1}s_2t_2)\cdots (t_{k-1}^{-1}s_kt_k) \tag{1}$$

由于 $s_it_iH = t_{i-1}H$，有 $\overline{s_it_i} = t_{i-1}$，我们可以把上式写成：

$$h = (\overline{s_0t_0}^{-1}s_1t_1)(\overline{s_1t_1}^{-1}s_2t_2)\cdots (\overline{s_{k-1}t_{k-1}}^{-1}s_kt_k) \tag{2}$$

另一方面，$\overline{sr}^{-1}srH = \overline{sr}^{-1}\overline{sr}H = H$，得证。

---

上式给出了一般的找到链的方法，代码可参见 [Schreier–Sims 算法 - OI Wiki](https://oi-wiki.org/math/algebra/schreier-sims/).

顺带提及 [GAP](https://www.gap-system.org/)，这是一个处理用于离散代数的系统。以下是用它处理一个四面体魔方（每个面有 $9$ 个正三角形）的示例（忽略了角块，只考虑四种大旋转）。

```sh
gap> it := Group(
> (10,16,24)(11,15,19)(12,14,20),
> (2,20,18)(1,21,17)(6,22,16),
> (2,8,24)(3,7,23)(4,12,22),
> (4,18,10)(5,13,9)(6,14,8)
> );
<permutation group with 4 generators>
gap> Size(it);
933120
gap> f := FreeGroup("x", "y", "z", "w");
<free group on the generators [ x, y, z, w ]>
gap> hom := GroupHomomorphismByImages(f, it, GeneratorsOfGroup(f), GeneratorsOfGroup(it));
[ x, y, z, w ] -> [ (10,16,24)(11,15,19)(12,14,20), (1,21,17)(2,20,18)(6,22,16), (2,8,24)(3,7,23)(4,12,22), (4,18,10)(5,13,9)(6,14,8) ]
gap> PreImagesRepresentative(hom, (10,14)(12,24));
x^-1*y*w^-1*y^-1*w*y^-1*x*y*x*z*y*z^-1*y^-1*x^-1*y*x*y*z*y^-1*z^-1*x^-1*w*y*w^-1*y^-1*x^-1*w*x^2*z*x^-1*z^-1*w*x*w^-2*y^-1*x^-1
```

## 可解群
### 换位子群
称**换位子群/导群**是一个群所有的换位子生成的群，记作 $G' = G^{(1)}$，取 $k$ 次导群的结果记为 $G^{(k)}$.

易见 $G/G'$ 是 Abel 的。实际上其中的想法可以被表达为：对 Abel 群 $A$ 及同态 $\phi: G \to A$，可以将它分解为：

$$G \stackrel{\pi}{\twoheadrightarrow} G/G' \stackrel{\bar \phi}{\to} A$$

{% <theorem> %}
一个群可解等价于它的某个有限阶导群 $G^{(n)} = \set{e}$.
{% </theorem> %}

右推左容易。对左推右，考虑一个合成群列，由 $[H_i, H_i] \subseteq H_{i-1}$ 知 $G^{(i)} \leq H_{n-i+1}$.

### 性质
作为综合练习，我们列出可解群的一些性质：

| 性质 | 解释 |
| :-: | :-: |
| 子群可解 | $H^{(n)} \leq G^{(n)}$ |
| 商群可解 | 用合成群列观点 |
| 发出的同态的像可解 | 由上一条 |
| $N, G/N$ 可解则 $G$ 可解 | 把合成群列拼起来 |
| 有限直积可解 | $(G \times H)^{(n)} = G^{(n)} \times H^{(n)}$ |
| 半直积可解 | $G/N \cong H$ |
| 可解正规子群乘积可解 | $AB/B \cong A/(A \cap B)$ |

---

[^1]: Jaap Scherphuis, "Computational Group Theory," *Jaap's Puzzle Page*, <https://www.jaapsch.net/puzzles/schreier.htm>.
