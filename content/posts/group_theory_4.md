+++
title = "群论（四）：特征子群；幂零群"
date = 2026-10-20

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

## 特征子群
### 内自同构
回忆自同构群 $\mathrm{Aut}(G)$ 为全体 $G$ 的自同构构成的群，我们定义**内自同构群** $\mathrm{Inn}(G)$ 为全体 $G$ 作用于自身的共轭（$\sigma(a) = g^{-1}ag$，称为内自同构）构成的群。所谓外自同构群则指：

$$\mathrm{Out}(G) \coloneqq \mathrm{Aut}(G) / \mathrm{Inn}(G)$$

{% <theorem title="置换群的自同构"> %}
我们知道共轭作用 $\mathrm{Ad}: S_n \to \mathrm{Aut}(S_n)$ 是单的。对 $n \neq 6$ 它是同构。
{% </theorem> %}

$n \neq 6$ 的例子略。$n = 6$ 时有一个不是内自同构的同构：

$$
\psi((12)) = (12)(34)(56) \\\\
\psi((23)) = (14)(25)(36) \\\\
\psi((34)) = (13)(24)(56) \\\\
\psi((45)) = (12)(36)(45) \\\\
\psi((56)) = (14)(23)(56)
$$

其一种看法是，考虑自然的作用 $\mathrm{PGL}_2(\mathbb F_5) \curvearrowright \mathbb P^1(\mathbb F_5)$，由于 $\mathrm{PGL}_2(\mathbb F_5) \cong S_5$，我们考虑通过左平移作用：

$$S_6 \curvearrowright S_6 / \mathrm{PGL}_2(\mathbb F_5)$$

### 定义与性质
{% <definition title="特征子群"> %}
称 $G$ 的子群 $H$ 为它的**特征子群** $H \text{ char } G$，如果对任意自同构 $\sigma$ 都有 $\sigma(H) = H$.
{% </definition> %}

我们可以仿照它把正规子群的定义重写为：对任意内自同构 $\sigma$ 都有 $\sigma(H) = H$.

{% <example> %}
对群 $G$，典型的特征子群有：其中心 $Z(G)$；其幂子群 $G^n$；其导群 $G'$；其 Frattini 子群（所有极大子群的交）；其 Fitting 子群（所有幂零正规子群的积）；其挠子群（所有有限阶元素构成的群）；其 Ω 子群（所有阶整除 $p^i$ 的元素构成的群）。
{% </example> %}

容易证明，若 $K \text{ char } H$ 且 $H \unlhd G$，则 $K \unlhd G$；进一步，特征子群有传递性。

## 幂零群
### 中心链
类似于[导群](@/posts/group_theory_2.md)，我们定义 $G^0 = G, G^{i+1} = [G, G^i]$，得到**降中心链**：

$$G^0 \geq G^1 \geq G^2 \geq \cdots$$

可以证明 $G^i \unlhd G$ 及 $G^{(i)} \leq G^i$；如果某个 $G^c = \set{1}$，则称 $G$ 是**幂零**的，称最小的 $c$ 是其幂零阶。

我们可以考虑一个对偶的视角：令 $Z_0(G) = 1, Z_1(G) = Z(G)$，并令：

$$
\begin{CD}
    G @>\pi>> G/Z_i(G) \\\\
    @A\mathrm{inj}AA @AA\mathrm{inj}A \\\\
    Z_{i+1}(G) @>>> Z(G/Z_i(G))
\end{CD}
$$

也就是说 $Z_{i+1}(G)$ 是 $G$ 中满足 $Z_{i+1}(G)/Z_i(G) \cong Z(G/Z_i(G))$ 的子群。得到**升中心链**：

$$Z_0(G) \leq Z_1(G) \leq Z_2(G) \leq \cdots$$

{% <theorem> %}
$G^c$ 平凡当且仅当 $Z_c(G) = G$，在此种条件下，有 $G^{c-i} \leq Z_i(G)$.
{% </theorem> %}

假设幂零，设 $c$ 最小使得 $G^c$ 平凡或者 $Z_c(G) = G$，对 $c$ 归纳。

记 $\pi: G \to G/Z(G)$，对 $H \leq G$ 记其在 $\pi$ 的像 $\bar H$，我们将证明（中间是归纳假设）：

$$G^c = \set{1} \iff (\bar G)^{c-1} = \set{1} \iff Z_{c-1}(\bar G) = \bar G \iff Z_c(G) = G$$

对左侧等价，讨论即可。对右侧等价，考虑 $Z_i(\bar G) = \overline{Z_{i-1}(G)}$，用类似这样的讨论：

![图表](/images/algebra/nilpotent_group.png)

### 结构
{% <theorem> %}
$p$-群都是幂零群。
{% </theorem> %}

所有 $p$-群[中心非平凡](@/posts/group_theory_1.md)，故升中心链可到达 $G$.

{% <theorem> %}
对 $p$-群 $P$，对非平凡正规子群 $H$，$H \cap Z(P)$ 非平凡；$H$ 是其真子群则是 $N_P(H)$ 的真子群。
{% </theorem> %}

对正规子群，考察 $P$ 在 $H$ 上的共轭作用，有：

$$H = \bigsqcup_i \mathrm{Orb}(a_i)$$

$\mathrm{Stab}(a_i) = P$ 当且仅当 $a_i \in Z(P)$，从而：

$$|H \cap Z(P)| \equiv \sum_i |\mathrm{Orb}(a_i)| \equiv |H| \equiv 0 \pmod p$$

对真子群，对 $|P|$ 归纳，若 $Z(P) \not\subseteq H$，则由 $Z(P) \subset N_P(H)$ 知成立；否则考察 $H/Z(P), P/Z(P)$.

{% <theorem title="幂零群分类定理"> %}
对有限群 $G$ 有阶 $n = p_1^{\alpha_1} p_2^{\alpha_2} \cdots p_r^{\alpha_r}$，设 $P_i$ 是某个 Sylow $p_i$-子群，则以下条件等价：
1. $P$ 幂零
2. $H$ 是其真子群则是 $N_P(H)$ 的真子群
3. 所有 Sylow 子群正规
4. $G \cong P_1 \times P_2 \times \cdots \times P_r$
{% </theorem> %}

## 大定理
回忆 Hölder 纲领，我们希望从 $N$ 与 $G/N$ 拼出 $G$ 的结构，不过这比较复杂。

{% <theorem title="Schur–Zassenhaus 定理"> %}
对 $N \unlhd G$，若 $|H|$ 与 $|G/N|$ 互素，
1. 存在 $H \leq G$ 使得 $\pi|_H: H \to G/N$ 是同构，称为“补”
2. 若 $N$ 与 $G/N$ 之一可解，则所有的“补”互相共轭
{% </theorem> %}

使用群上同调理论可以说明定理对交换的 $N$ 成立。然后可以归结到交换的情形（证明略）。

这个定理与 Burnside 正规补定理可以用来证明 P. Hall 定理。
