+++
title = "群论（一）：群在集合上的作用相关"
description = "群在集合上的作用，Pólya 计数法，共轭作用与 Sylow 定理。"
date = 2025-06-09
updated = 2025-06-11

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["讲义", "数学", "代数", "抽象代数", "群论"]
+++

[索引](/posts/index-group-theory/)

前置知识
- 线性代数
- 群论基本定义

## 群在集合上的作用
群 $G$ 在集合 $\Omega$ 上的作用是一个映射

$$
\begin{aligned}
\varphi \colon &G \to (\Omega\to\Omega),\\\\
        &x \mapsto (\alpha\mapsto\alpha^x)
\end{aligned}
$$

满足单位元对应恒等映射，且 $(\alpha^x)^y = \alpha^{xy}$

你或许可以找到不同形式，如

$$
\begin{aligned}
f \colon &G\times S \to S,\\\\
        &(g, s) \mapsto g(s)
\end{aligned}
$$

但前一种更直观一些。

显然有 $\varphi$ 是从 $G$ 到 $\Omega$ 上变换群的同态。

记集合元素 $\alpha$ 在这一关系下的等价类 $\\{\alpha^x\mid x\in G\\}$ 为其**轨道** $Orb(\alpha)$；全体不变映射 $\\{x\mid\alpha^x=\alpha\\}$ 为其**稳定化子** $Stab(\alpha)$

易知 $|Orb(\alpha)|=|G\colon Stab(\alpha)|$

例如，对于正四面体（记顶点 $\\{A,B,C,D\\}$），设其旋转变换群为 $G$，则：任取一个顶点，它对应的稳定子群阶为 3，轨道为 $\\{A,B,C,D\\}$，故而 $G$ 是 $S_4$ 的 12 阶子群，必然是 $A_4\cong V_4\oplus Z_3\cong Z_2\oplus Z_2\oplus Z_3$

## Pólya 计数法
使用 Pólya 计数法是为了解决这样的问题：我们对所有的可能计数，并且将具有特定对称性的视作同一种（见下面的例子）。

{% admonition(type="abstract", title="Burnside 引理") %}
$\varphi$ 对应的轨道数为 $\frac{1}{|G|} \sum_{g\in G}|X(g)|$，其中 $X(g) = \\{x\mid x^g = x\\}$
{% end %}

其本质是对所有满足 $x^g = x$ 的数对的算两次。

对 $g$ 计数为 $\sum |Stab(g)| = \sum\frac{|G|}{|Orb(g)|} = |G|\cdot ans$

对 $x$ 计数则为 $\sum_{g\in G}|X(g)|$

{% admonition(type="question", title="立方体染色Ⅰ") %}
一个立方体六个面颜色不同，有多少种旋转下不同的染色？
{% end %}

易知对称群 $|G| = |Orb(x)|\cdot |Stab(x)| = 24$

集合 $\Omega$ 是六个面的染色状态构成的集合，作用 $\varphi$ 使得 $\varphi(g)$ 是进行 $g$ 对应的旋转操作。所求即它的轨道数。

$X(g)$ 只在单位元处取到 720，其余情况为空集。故所求为 30.

{% admonition(type="question", title="立方体染色Ⅱ") %}
一个立方体，使用三种颜料染色，有多少种旋转下不同的染色？
{% end %}
定义同上。

对 $G$ 中元素分类：
| 类型 | 个数 | $X(g)$ 值 |
| :-: | :-: | :-: |
| 不动 | 1 | $3^6$ |
| 旋转 90° | 6 | $3^2\cdot 3$ |
| 旋转 180° | 3 | $3^2\cdot 3^2$ |
| 绕一对对棱中点连成的轴旋转 180° | 6 | $3^3$ |
| 绕顶点旋转 120° | 8 | $3^2$ |

所求为 57.

## 共轭作用
共轭作用是在 $G$ 的集合上的作用：$\varphi(g) = (a\mapsto gag^{-1})$

记 $G$ 上 $\\{x\\}$ 的**中心化子** $C_G(x)=\\{a\mid xa=ax\\}$，**中心** $Z(G) = \\{x\mid gx=xg (\forall g\in G)\\}$

则 $x\in Z(G)\Leftrightarrow |Orb(x)|=1$

那么我们可以写出**类方程**：

$$|G| = |Z(G)| + \sum |G\colon C_G(y_i)|$$

其中 $y_i$ 的轨道阶不为 1

例如，对 $|G|=p^l$，所有的 $C_G(y_i)$ 都是真子群，从而 $p\mid |Z(G)|$

## Sylow 定理
对有限群 $G$ 和素数 $p$ 使 $p^l\parallel |G|$，$G$ 的 $p^l$ 阶子群为其 **Sylow p 子群**。

{% admonition(type="abstract", title="第一 Sylow 定理") %}
对有限群 $G$ 和素数 $p$ 使 $p^k\mid |G|$，$G$ 存在 $p^k$ 阶子群。
{% end %}

只需讨论阶大于 $p$ 的非交换群，因为交换群一定可以分解为循环群的直积。[^1]

对 $|G|$ 归纳。
- 若 $p\mid Z(G)$，由于它是交换的，有 p 阶子群。考察它和对应的商群，使用归纳假设。
- 若 $p\nmid Z(G)$，由类方程，存在一个 $p\nmid Orb(y_i)$，有 $p^l\parallel C_G(y_i)$，使用归纳假设。

{% admonition(type="abstract", title="第二 Sylow 定理") %}
对 $p^k\mid |G|$，Sylow p 子群 $P$，$p^k$ 阶子群必为 $P$ 的某个共轭的子群。
{% end %}

令 $\Omega$ 为 $P$ 的左陪集构成的集合，$|G|=p^lm$

对一个 $p^k$ 阶子群 $H$，考察 $H$ 在 $P$ 上的作用 $\varphi(h) = (aP\mapsto haP)$

有 $|Orb(aP)|\big| |H|$，且 $|\Omega|=\sum |Orb(aP)|\not\equiv 0\pmod{p}$

故至少一个 $|Orb(aP)|=1$

对应 $h\in aPa^{-1}$ 即 $H\subseteq aPa^{-1}$

{% admonition(type="abstract", title="第三 Sylow 定理") %}
对 $p^l\parallel |G|, |G|=p^lm$，Sylow p 子群个数 $r$，则 $r\equiv 1\pmod{p}, r\mid m$
{% end %}

称 $H$ 在 $G$ 中**正规化子** $N_G(H) = \\{G\mid gHg^{-1}=H\\}$

则对 $G$ 的 Sylow p 子群 $P$，有 $P\unlhd N_G(P)\le G$

对 $G$ 的 Sylow p 子群 $Q\subseteq N_G(P)$，$P,Q$ 同为 $N_G(P)$ 的 Sylow p 子群，由第二定理知相互共轭。由 $P$ 为 $N_G(P)$ 的正规子群，$P=Q$

令 $\Omega$ 为 Sylow p 子群的集合，$P$ 在 $\Omega$ 上作用为共轭 $\varphi(g) = (Q\mapsto gQg^{-1})$

有 $|Orb(Q)|=1\Leftrightarrow Q=P$，其余整除 $|P|$

故 $r=\sum |Orb(Q)|\equiv 1\pmod{p}$

由第二定理知 $G$ 在 $\Omega$ 上的共轭作用使 $\Omega$ 成为轨道，$r=|\Omega|\big| |G|$，即 $r\mid m$

{% admonition(type="abstract", title="p·q 阶群分类") %}
对素数 $p<q$，$pq$ 阶群在 $q\not\equiv 1\pmod{p}$ 时只有循环群。
{% end %}

取 Sylow p 子群及 Sylow q 子群，由第三定理知个数均为 1，从而分别是正规的

易知 $PQ=G$，又由 $P\cap Q=\\{e\\}$，可知 $G\cong P\oplus Q\cong Z_p\oplus Z_q\cong Z_{pq}$

## 注释 {#comments}
[^1]: 通过以下步骤证明：
* 将 $G$ 用自由群表示法表示为 $<g_1, g_2\cdots g_n\mid rules>$
* 由于是交换群，可以将一个规则（形如 $g_1 g_2^{-1} g_1^2$）任意交换顺序写成 $3x_1-x_2=0$ 的形式
* 所有的规则写成线性方程组，表示为
$$M \begin{pmatrix} g_1 \\\\ \vdots \\\\ g_n \end{pmatrix} = \mathbf{0}$$
* 由于我们在整数上操作，它是 Euclid 整环，可以将矩阵 $M$ 转化为 Smith 标准型
* 其因子给出了 $G$ 的分解
