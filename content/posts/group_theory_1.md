+++
title = "群论（一）：群在集合/群上的作用"
date = 2025-06-09
updated = 2026-10-19

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

在历史上，首先出现的概念是具体的集合上的对称/变换群（取元素为双射，运算为复合），然后才从中抽象出了群的概念。因此，自然地，我们可以考察群到集合的对称群的同态。

<!-- more -->

## 群在集合上的作用
群 $G$ 在集合 $\Omega$ 上的作用（记 $G \curvearrowright \Omega$）是一个映射：

$$
\begin{aligned}
\varphi \colon G & \longrightarrow (\Omega \longrightarrow \Omega) \cr
    x & \longmapsto (\alpha \longmapsto \alpha^x)
\end{aligned}
$$

满足 $(\alpha^x)^y = \alpha^{xy}$，这给出单位元对应恒等映射，逆元对应逆映射。或者说，$\varphi$ 是从 $G$ 到 $\Omega$ 上对称群的同态。

它有时被写成不同的形式，如：

$$
\begin{aligned}
f \colon G\times S & \longrightarrow S \cr
    (g, s) & \longmapsto g(s)
\end{aligned}
$$

个人认为前一种看法更直观一些。只不过对一些群以外的范畴，不一定笛卡尔闭，不一定能写成前一种形式。

记集合元素 $\alpha$ 在这一关系下的等价类为其**轨道**：

$$\mathrm{Orb}(\alpha) \coloneqq \set{\alpha^x | x\in G}$$

记全体不变映射为其**稳定化子**，满足 $|\mathrm{Orb}(\alpha)| = |G\colon \mathrm{Stab}(\alpha)|$：

$$\mathrm{Stab}(\alpha) \coloneqq \set{x | \alpha^x = \alpha}$$

作为一个例子，对于正四面体（记顶点 $\set{A, B, C, D}$），设其旋转变换群为 $G$，则：任取一个顶点，它对应的稳定子群阶为 $3$，轨道为 $\set{A, B, C, D}$，故而 $G$ 是 $S_4$ 的 $12$ 阶子群，必然是 $A_4$.

## 平移作用
左平移作用是在 $G$ 的集合上的作用：

$$
\begin{aligned}
\varphi(g) \colon G & \longrightarrow G \cr
    h & \longmapsto gh
\end{aligned}
$$

{% <theorem> %}
对 $|G| = n$ 的最小素因子 $p$，所有 $n / p$ 阶子群都是正规的。
{% </theorem> %}

设 $n / p$ 阶子群 $H$，考察左平移作用 $G \curvearrowright G/H$，这给出了 $\varphi: G \to S_p$ 同态。

一方面，$\operatorname{Im} \varphi \leq S_p$，故 $|\operatorname{Im} \varphi|$ 整除 $p!$；另一方面 $\ker \varphi \leq H$，有 $|\ker \varphi| \cdot [H : \ker \varphi] = n / p$，相乘即 $[H : \ker \varphi] \mid (p - 1)!$，只能为一。

故 $H = \ker \varphi$，而 $\ker \varphi$ 正规，故 $H$ 正规。

## 半直积
对于 $\phi: H \to \mathrm{Aut}(N)$，我们定义半直积 $N\rtimes_\phi H$ 是：

$$
N\rtimes_\phi H \coloneqq \set{(n, h) | n \in N, h \in H} \\\\
(n_1, h_1) \cdot (n_2, h_2) = (n_1 \cdot \phi_{h_1}(n_2), h_1 h_2)
$$

典型的例子是 $D_{2n} = Z_n \rtimes Z_2$.

我们有 $N$ 是如下满同态的核，故 $N$ 是正规子群：

$$
\begin{aligned}
\pi: N\rtimes H & \longrightarrow H \cr
    (n, h) & \longmapsto h
\end{aligned}
$$

{% <theorem title="识别半直积"> %}
对群 $G$ 的正规子群 $N$ 及子群 $H$，若 $N \cap H = \set{e}$，则 $NH \cong N\rtimes H$.
{% </theorem> %}

$$\mathrm{Ad}_h(n) = hnh^{-1}$$

## Pólya 计数法
使用 Pólya 计数法是为了解决这样的问题：我们对所有的可能计数，并且将具有特定对称性的视作同一种（见下面的例子）。

{% <theorem title="Burnside 引理"> %}
$\varphi$ 对应的轨道数为 $\sum_{g \in G} |X(g)| / |G|$，其中 $X(g) = \set{x | x^g = x}$.
{% </theorem> %}

对所有满足 $x^g = x$ 的数对的算两次（分别对 $g, x$ 计数）。

{% <question title="立方体染色Ⅰ"> %}
一个立方体六个面颜色不同，有多少种旋转下不同的染色？
{% </question> %}

旋转对称群大小 $|G| = |\mathrm{Orb}(x)|\cdot |\mathrm{Stab}(x)| = 24$.

集合 $\Omega$ 是六个面的染色状态构成的集合，作用 $\varphi$ 使得 $\varphi(g)$ 是进行 $g$ 对应的旋转操作。所求即它的轨道数。

$X(g)$ 只在单位元处取到 $720$，其余情况为空集。故所求为 $30$.

{% <question title="立方体染色Ⅱ"> %}
一个立方体，使用三种颜料染色，有多少种旋转下不同的染色？
{% </question> %}
定义同上。

对 $G$ 中元素分类，算得 $57$.

| 类型 | 个数 | $X(g)$ 值 |
| :-: | :-: | :-: |
| 不动 | $1$ | $3^6$ |
| 旋转 90° | $6$ | $3^2\cdot 3$ |
| 旋转 180° | $3$ | $3^2\cdot 3^2$ |
| 绕一对对棱中点连成的轴旋转 180° | $6$ | $3^3$ |
| 绕顶点旋转 120° | $8$ | $3^2$ |

## 共轭作用
共轭作用是在 $G$ 的集合上的作用：

$$
\begin{aligned}
\varphi(g) \colon G & \longrightarrow G \cr
    a & \longmapsto gag^{-1}
\end{aligned}
$$

记 $G$ 上 $\set{x}$ 的**中心化子** $C_G(x) = \set{a | xa = ax}$，**中心** $Z(G) = \set{x | gx=xg,  \forall g}$. 有 $x\in Z(G)\iff |\mathrm{Orb}(x)| = 1$.

那么我们可以写出**类方程**：

$$|G| = |Z(G)| + \sum_{y_i \notin Z(G)} [G : C_G(y_i)]$$

称阶是素数 $p$ 的幂的群为 **$p$-群**。

{% <example> %}
$p$-群的中心非平凡。
{% </example> %}

对 $y_i \notin Z(G)$，有 $C_G(y_i)$ 是真子群，$p \mid [G : C_G(y_i)]$，由类方程知 $p \mid \\#Z(G)$，从而中心非平凡。

## Sylow 定理
对有限群 $G$ 和素数 $p$ 使 $p^l$ 恰整除 $|G|$，以下设 $|G| = p^l m$，称 $G$ 的 $p^l$ 阶子群为其 **Sylow $p$-子群**。

记 $H$ 在 $G$ 中**正规化子**是：

$$N_G(H) = \set{G | gHg^{-1} = H}$$

{% <theorem title="第一 Sylow 定理"> %}
Sylow $p$-子群存在。其推论是对 $k \leq l$，$p^k$ 阶子群均存在。
{% </theorem> %}

对 $|G|$ 归纳：
- 若 $p\mid Z(G)$，由于 $Z(G)$ 交换，可以分解为循环群的直积[^1]，分解中可提取 $p$ 有关部分；对 $G / Z(G)$ 用归纳假设
- 若 $p\nmid Z(G)$，由类方程，存在一个 $p\nmid \mathrm{Orb}(g_i)$，有 $p^l\parallel C_G(g_i)$，对 $C_G(g_i)$ 用归纳假设

{% <theorem title="第二 Sylow 定理"> %}
对 $p^k$ 整除 $|G|$ 及 Sylow $p$-子群 $P$，$p^k$ 阶子群必为 $P$ 的某个共轭的子群。其推论是 Sylow $p$-子群两两共轭，每个 $p$-子群都在某个 Sylow $p$-子群中。
{% </theorem> %}

对一个 $p^k$ 阶子群 $H$，考察 $H$ 在 $G/P$ 上的左平移作用 $\varphi(h)(aP) = haP$，有 $|\mathrm{Orb}(aP)|$ 整除 $|H|$，且：

$$[G : P] = \sum_{aP} 1 = \sum |\mathrm{Orb}(aP)| \not\equiv 0 \pmod{p}$$

故至少一个 $|\mathrm{Orb}(aP)| = 1$，这个 $a$ 给出 $H\subseteq aPa^{-1}$.

{% <theorem title="第三 Sylow 定理"> %}
设 Sylow $p$-子群个数 $n_p$，则 $n_p \equiv 1\pmod{p}$ 且 $n_p \mid m$.
{% </theorem> %}

由上一定理，对 $G$ 的 Sylow $p$-子群 $P$，有 $P$ 是 $N_G(P)$ 唯一的正规子群。

令 $\Omega$ 为 Sylow $p$-子群的集合，考察 $P$ 在 $\Omega$ 上的共轭作用 $\varphi(p)(Q) = pQp^{-1}$，有 $|\mathrm{Orb}(Q)| = 1$ 当且仅当 $Q = P$，否则整除 $|P|$，

$$n_p = \sum |\mathrm{Orb}(Q)|\equiv 1\pmod{p}$$

又由下式知 $n_p \mid m$：

$$n_p = |G/N_G(P)| = \frac{|G|}{|N_G(P)|} = \frac{p^l \cdot m}{p^l \cdot [N_G(P) : P]}$$

{% <example> %}
对素数 $p < q$，考察 $pq$ 阶群的分类。
{% </example> %}

Sylow $q$-子群恰一个，从而正规。而 $n_p = 1$ 或 $q$，对前一情形，有 $P\cap Q = \set{e}, G = QP$，可知：

$$G\cong P\oplus Q\cong \Z_p\oplus \Z_q\cong \Z_{pq}$$

对 $n_p = q \equiv 1 \pmod{p}$，有 $G = Q \rtimes P$，对应于同态：

$$
\begin{aligned}
\Z_p \simeq P &\stackrel \eta \longrightarrow \mathrm{Aut}(Q) \cong \mathrm{Aut}(\Z_q) \cong \Z_q^\times \\\\
y &\longmapsto (x \mapsto yxy^{-1} = y^a) \longmapsto a
\end{aligned}
$$

在同构意义下这个半直积是唯一的。因为另外的同态必形如：

$$
\begin{aligned}
\eta_i: \Z_p &\longrightarrow \Z_q^\times \\\\
    \sigma &\longmapsto a^i
\end{aligned}
$$

然后就可给出如下同构：

$$
\begin{aligned}
Q \rtimes_{\eta_1} \Z_p &\longrightarrow Q \rtimes_{\eta_i} \Z_p \\\\
    (y, c) &\longmapsto (y, ic)
\end{aligned}
$$

{% <theorem> %}
对有限群 $G$ 的正规子群 $N$，商映射 $\pi: G \twoheadrightarrow G/N$，Sylow $p$-子群 $H$，有 $\pi(H)$ 是 $G/N$ 的 Sylow $p$-子群；$N \cap H$ 是 $N$ 的 Sylow $p$-子群。
{% </theorem> %}

{% <example> %}
132 阶群不可能是单群。
{% </example> %}

假设是单群，$n_2, n_3, n_{11} \neq 1$，有 $n_{11} = 12$，由于对应的 Sylow 子群只有平凡交，至少存在 120 个元素阶 11；同理至少存在 8 个元素阶 3；至少存在 7 个元素阶 2，而 $135 > 132$ 矛盾。

---

[^1]: 通过以下步骤证明：
* 将 $G$ 用自由群表示法表示为 $\braket{g_1, g_2, \dots, g_n \mid \text{rules}}$
* 由于是交换群，可以将一个规则（形如 $g_1 g_2^{-1} g_1^2$）任意交换顺序写成 $3x_1-x_2=0$ 的形式
* 所有的规则写成线性方程组，表示为
$$M \begin{pmatrix} g_1 \cr \vdots \cr g_n \end{pmatrix} = \mathbf{0}$$
* 由于我们在整数上操作，它是 Euclid 整环，可以将矩阵 $M$ 转化为 Smith 标准型
* 其因子给出了 $G$ 的分解
