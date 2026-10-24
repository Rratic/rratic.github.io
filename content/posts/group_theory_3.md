+++
title = "群论（三）：直积有关的讨论"
date = 2026-10-11

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

写作业和与同学讨论时有感于：直积虽然定义简单且有[良好的泛性质](@/posts/category_theory_1.md)，但其很多事实是非平凡的。本文一为证明技术的练习，二为戒骄戒躁，三为展示此问题之勾连四方。

<!-- more -->

## 子群的刻画
考察 $H \leq A \times B$，其嵌入与投射的复合给出两个投射 $H \to A$ 及 $H \to B$，现在不妨设它们是满的（不然只需考虑对应的 $H \leq A' \times B'$）。

{% <theorem title="Goursat 引理"> %}
对上述 $H$，存在 $A_1 \unlhd A, B_1 \unlhd B$ 及同构 $\varphi: A/A_1 \to B/B_1$，使得：

$$H = \set{(a, b) | \varphi(aA_1) = bB_1}$$
{% </theorem> %}

我们令 $A_1 = \set{a | (a, 1) \in H}$，$B_1$ 同理，则它们是正规子群。

我们取以下的 $\varphi$，这里 $[b]$ 是 $[a]$ 对应的唯一使 $(a, b) \in H$ 的选择：

$$
\begin{aligned}
\varphi \colon A/A_1 &\longrightarrow B/B_1 \cr
    [a] &\longmapsto [b]
\end{aligned}
$$

它良定义，且是同构，于是结论成立。

如果从范畴论来看，有 $H$ 是这里诱导的 $A \to B/B_1$ 与 $B \to B/B_1$ 的一个拉回。故我们说子群都是“纤维积”：

$$H \cong A \times_{B/B_1} B$$

{% <theorem> %}
如若要求 $H \unlhd A \times B$，则易证它是 $1, A, B, A \times B$ 四者之一。如若 $A, B$ 中心平凡，则子群也一定是四者之一。
{% </theorem> %}

考虑更一般的 $H \leq G_1 \times \cdots \times G_n$ 情形，令 $I = \set{i | H \cap G_i = G_i}$，

考察 $h = (g_1, \dots, g_n) \in H$，对于 $i \notin I$，对所有 $g \in G_i$ 都有 $ghg^{-1} = h$，故 $g_i = 1$，由此：

$$H = \prod_{i \in I} G_i$$

{% <question title="习题（Alibaba 2022）"> %}
设 $G_1, \dots, G_n$ 是非交换的单群，$n \geq 2$，令 $H \leq G_1 \times \cdots \times G_n$ 满足所有投射 $H \to H_i \times H_j$ 满（$i < j$），证明 $H = G_1 \times \cdots \times G_n$.
{% </question> %}

## 直积的消去
{% <theorem title="消去律"> %}
对有限群 $H \times K_1 \cong H \times K_2$，有 $K_1 \cong K_2$.
{% </theorem> %}

这没有什么简单的证法。需要用到大定理：

{% <theorem title="Remak–Krull–Schmidt 定理"> %}
对有限群 $G$ 的完全分解：

$$
G = H_1 \times \cdots \times H_s \\\\
G = K_1 \times \cdots \times K_t
$$

存在双射 $\sigma: [s] \to [t]$ 使得 $H_i \cong K_{\sigma(i)}$.
{% </theorem> %}

Remak 在其 1911 年的博士论文中证明了这一点（实际上加强到存在 $G$ 的正规自同构 $\varphi$ 使得 $\varphi(H_i) = K_{\sigma(i)}$）。

我们称一个自同态是正规的，如果它与所有的内自同构交换。实际上投射 $\varepsilon_i: G \to H \hookrightarrow G$ 是正规自同态，且按逐点加法：

$$1 = \varepsilon_1 + \cdots + \varepsilon_n$$

容易证明引理：自同态的和 $\alpha + \beta$ 是自同态，当且仅当 $[\alpha(G), \beta(G)] = 1$，因此 $\varepsilon_i + \varepsilon_j$ 等也是自同态。

现在刻画不可分解群。对不可分解群 $H$ 的正规自同态 $\alpha$，其核与像正规，$H = \ker \alpha \times \operatorname{Im} \alpha$，故其中之一平凡。故 $\alpha$ 要么是自同构，要么幂零。

定义正规自同态 $\varphi_{ij}$ 是复合得到的：

$$H_i \to G \to K_j \to G \to H_i$$

固定 $i$，若所有 $\varphi_{ij}$ 幂零，则 $\sum_j \varphi_{ij}$ 幂零，但这个和是 $\mathrm{id} _ {H _ i}$，故至少一个 $\varphi_{ij}$ 是同构。

不妨重排使得 $\varphi_{11}$ 是同构。有 $[H_1, K_1] = 1$，从而作为子群 $H_1 = K_1$，然后就可以考虑 $G / H_1, G / K_1$，归纳。

实际上这里的核心观察可以推广为 Fitting 引理：对有限维[表示](@/posts/representation_theory_1.md) $V$ 及其自同态 $\phi$，对充分大的 $r$ 有 $V = \ker \phi^r \oplus \operatorname{Im} \phi^r$，如果 $V$ 不可分解则 $\phi$ 要么自同构要么幂零。于是给出定理的模论证法。另一种证法是使用特征标理论。

实际上“有限”条件可以弱化为：满足升链条件（正规子群链 $H_1 \leq H_2 \leq \cdots$ 会从某一处起 $H_i = H_{i+1} = \cdots$）与降链条件（正规子群链 $H_1 \geq H_2 \geq \cdots$ 会从某一处起 $H_i = H_{i+1} = \cdots = \set{1}$）。证明略去。

## Grothendieck 群
除了有限群范畴外，还有什么范畴有直积消去律？实际上有：非空有限集范畴、有限维向量空间范畴、有限 Abel 群范畴、有限生成 Abel 群范畴、PID 上有限生成模范畴、有限环范畴、有限连通图范畴（Sabidussi 定理）、有限偏序集范畴（Hashimoto 定理）、有限布尔代数范畴……

考虑在直积运算下，会构成一个交换幺半群。我们可以定义一个交换幺半群 $M$ 的 Grothendieck 群：

$$
G(M) \coloneqq (M \times M) / \sim \\\\
(a, b) \sim (c, d) \iff a + d + m = b + c + m,\\, \exists m
$$

易见它是 Abel 群，等价类 $[(a, b)]$ 含义是形式差。一个直观的例子可以参考 [0=1−1=−1+1=0](https://www.bilibili.com/video/BV1Gjba6XEWo/) 中用有限集定义整数。

有一个典范的幺半群同态 $i(a) = [(a, 0)]$，有 $G(M)$ 是 $M$ 的“最小 Abel 群化”，即具有泛性质：对任意 Abel 群 $A$ 及幺半群同态 $\varphi: M \to A$，存在唯一 $\psi: G(M) \to A$ 使图表交换。

对于有有限积的范畴 $\mathcal C$，设对象的同构类 $[X]$ 就可定义 $[X] + [Y] = [X \otimes Y]$，从而可定义 Grothendieck 群：

$$K_0(\mathcal C) \coloneqq G(M(\mathcal C))$$

那么所谓消去律等价于说以下自然的映射是单射：

$$M(\mathcal C) \to K_0(\mathcal C)$$

如果 $\mathcal C$ 是 Abel 范畴（核与余核存在、单态射是其余核的核、满态射是其核的余核、态射可分解为单态射与满态射乘积的加法范畴），则可以对 $0 \to A \to B \to C \to 0$ 让 $[B] = [A] + [C]$ 来定义。
