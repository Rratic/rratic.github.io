+++
title = "群论（二）：魔方与次正规群列"
description = "换位子，半直积，Schreier 子群引理，可解群，导群，合成群列，Schreier 定理与 Jordan-Hölder 定理。"
date = 2025-06-16
updated = 2025-06-23

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "抽象代数", "群论"]
+++

[索引与符号表](/posts/index-group-theory/)

注意到有一些群论的文章以魔方为例，但停留在将它作为例子，本文对三阶魔方进行详细说明。

## 记号
不妨设每个面的中心固定，令 $G$ 表示魔方的变换群。

我们记六个面为上面 U，下面 D，左面 L，右面 R，前面 F 和后面 B（取首字母），以大写字母表示将该面顺时针旋转 90°

显然所求的群是由它们生成的，即 $G=<U, R, F, D, L, B>$

## 群结构
### 大小
魔方剩余可动的有两类：角块和棱块，它们两两不同，由于分别有位置和旋转状态，有：

$$G\leq Z_3^8\times S_8 \times Z_2^{12}\times S_{12}$$

接下来更精细地讨论哪些角块、棱块排布是合法的。

先讨论角块，一个角块的标准状态是一个（或三个）面上与中心块颜色相同，将其记作 0，顺时针转 120° 的状态记作 1，顺时针转 240° 的状态记作 2，那么所有标记之和在 $\pmod{3}$ 下不变。同理，棱块的标记之和在 $\pmod{2}$ 下不变。此外，角块的置换奇偶性与棱块的置换奇偶性一致。

同时，我们可以说明符合上述要求的是合法的。

定义换位子 $[a, b] = aba^{-1}b^{-1}$

如果 $x^{[a, b]}\neq x$，则要么 $a\notin \rm{Stab}(x), b\notin \rm{Stab}(x^a)$，要么 $b\notin \rm{Stab}(x), a\notin \rm{Stab}(x^b)$

例如，我们构造操作：$[[R, U], D] = RUR'U'DURU'R'D'$，这只会改变三个角块的状态。

因此有 $|G| = \frac{1}{12} |Z_3^8\times S_8 \times Z_2^{12}\times S_{12}| = 43252003274489856000$

### 半直积
为更好地描述角块群与棱块群，我们定义直积的扩展。

群 $N, H$，$H$ 在 $N$ 上的作用 $\varphi$，定义半直积 $N\rtimes_\varphi H$ 为：
- 集合是 $N\times H$
- 运算 $(n_1, h_1)\cdot (n_2, h_2) = (n_1n_2^{h_1}, h_1h_2)$
- 单位元 $(e_N, e_H)$
- 逆元 $(n^{-h^{-1}}, h^{-1})$

例如，$D_{2n} = Z_n \rtimes Z_2$，其中

$$
\varphi: \begin{cases}
  e \mapsto (g \mapsto g) \\\\
  a \mapsto (g \mapsto g^{-1})
\end{cases}
$$

由于角块（通过操作造成的）位置变化会改变朝向，角块群形如 $(Z_3)^7 \rtimes S_8$

整个魔方群是 $(Z_3^7 \rtimes S_8)\times (Z_2^{11} \rtimes S_{12}) / \sim$

## 解法
### 换位子
现在人类的方法大致如此逐层复原，在后半部分使用预设的小阶数置换的公式。

### 降低群大小
Thislethwaite Method 将群逐步化为
- $<U, R, F^2, D, L, B^2>$
- $<U, R^2, F^2, D, L^2, B^2>$
- $<U^2, R^2, F^2, D^2, L^2, B^2>$
- $\\{e\\}$

Kociemba Algorithm 则将群逐步化为
- $<U, R^2, F^2, D, L^2, B^2>$
- $\\{e\\}$

## 通用解法
以上的解法都依赖于具体的结构，这里提供一个通用方法：Schreier-Sims-Minkwits 算法。[^1]

我们希望进行这样的操作：每次多固定一个集合上的元素，其稳定化子就是原变换群的真子群，如此下去可以得到一个链 $G = G_0 > G_1 > G_2 \cdots G_n = \\{e\\}$，而由于我们要写出一个操作序列，设第 $i$ 个阶段可能的操作为 $r_{i_1}, r_{i_2} \cdots$ 有 $r_{i_1}G_{i+1}, r_{i_2}G_{i+1} \cdots$ 陪集族构成 $G_i$

{% admonition(type="abstract", title="Schreier 子群引理") %}
$G$ 是一个由集合 $S$ 中元素（置换）生成的群，有子群 $H$，设（左）陪集代表元构成集合 $R$，其中元素 $g$ 对应代表元为 $\bar{g}$，则 $H$ 是由 $\\{\overline{sr}^{-1}sr \mid r\in R, s\in S\\}$ 生成的。
{% end %}

对 $H$ 的元素 $h = s_1s_2\cdots s_k$，其中 $s_i$ 为生成元，记 $t_i = \overline{s_{i+1}\cdots s_k}$，其中 $t_0 = t_k = e$，故有

$$h = (t_0^{-1}s_1t_1)(t_1^{-1}s_2t_2)\cdots (t_{k-1}^{-1}s_kt_k)\tag{1}$$

由于 $s_it_iH = t_{i-1}H$，有 $\overline{s_it_i} = t_{i-1}$，我们可以把上式写成

$$h = (\overline{s_0t_0}^{-1}s_1t_1)(\overline{s_1t_1}^{-1}s_2t_2)\cdots (\overline{s_{k-1}t_{k-1}}^{-1}s_kt_k)\tag{2}$$

另一方面，$\overline{sr}^{-1}srH = \overline{sr}^{-1}\overline{sr}H = H$，得证

---

上式给出了一般的找到链的方法，代码可参见 <https://oi-wiki.org/math/algebra/schreier-sims/>

## 群列
### 次正规群列
仍考虑前述的链 $G = G_0 > G_1 > G_2 \cdots G_n = \\{e\\}$，我们希望它的性质足够好。

称序列 $G = G_0 \triangleright G_1 \triangleright G_2 \cdots G_n = \\{e\\}$ 为 $G$ 的**次正规群列**。

### 可解群
称群是**可解群**，如果在次正规群列中每个商 $G_{i-1}/G_i$（称为**合成因子**）都是交换群。

{% admonition(type="abstract", title="可解群的群列") %}
称**导群**是一个群所有的换位子生成的群，记作 $G'=G^{(1)}$，则一个群可解等价于它的若干阶导群 $G^{(k)} = \\{e\\}$
{% end %}

一方面，所有的 $G^{(k)}$ 给出了这个群列的构造。可以证明 $G/G'$ 交换：考虑映射

$$
\begin{aligned}
\sigma \colon & G \to G/G',\\\\
        &g \mapsto gG'
\end{aligned}
$$

有 $\ker\sigma = G'$，即 $\sigma(xyx^{-1}y^{-1})=e$，从而 $\sigma(x)\sigma(y)=\sigma(y)\sigma(x)$，$\rm{Im}\\:\sigma = G/G'$ 交换。

另一方面，我们可以与上类似地说明对 $H\triangleleft G$，$G/H$ 交换当且仅当 $G'\leq H$

{% admonition(type="abstract", title="Feit-Thompson 定理") %}
每一个奇阶的有限群都是可解群。
{% end %}

此定理的证明长达 254 页，在此仅作告示之用。

### 合成群列
如果次正规群列中每个商都是单群，则称为**合成群列**。

{% admonition(type="abstract", title="Schreier 定理") %}
有限群的任何次正规群列可以加细为合成群列。
{% end %}

取长度极大的次正规群列，假设不是合成群列，某个 $G_{i-1}/G_i$ 有非平凡正规子群 $H$

取其在典范同态

$$
\begin{aligned}
\sigma \colon & G_{i-1} \to G_{i-1}/G_i,\\\\
        &g \mapsto gG_i
\end{aligned}
$$

下的原像，可以插在序列中，与极大矛盾。

---

进一步地，可解群的合成因子均为交换的单群，从而是素数阶循环群（利用了[交换群一定可以分解为循环群的直积](/posts/group-theory-p1/#comments)）。

{% admonition(type="abstract", title="Jordan-Hölder 定理") %}
两个合成群列

$$G = G_0 \triangleright G_1 \triangleright G_2 \cdots G_n = \\{e\\}$$
$$G = H_0 \triangleright H_1 \triangleright H_2 \cdots H_m = \\{e\\}$$

可以以某种方式配对，使对应的合成因子同构。
{% end %}

存在一些归纳的证法。同时可以抽象成短正合列说明。

## 注释
[^1]: https://www.jaapsch.net/puzzles/schreier.htm
