+++
title = "示性类（一）：Stiefel-Whitney 类"
date = 2026-11-05

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

本文对应于 Milnor 的 *Characteristic Classes* 1-4 节。

<!-- more -->

## 基本定义
这里我们扩展一下光滑流形的定义，允许无穷维。设 $A$ 是指标集，$\R^A$ 包含所有 $A \to \R$ 函数，给予相对拓扑（拓扑空间 $Y$，$f: Y \to M \subseteq \R^A$ 连续当且仅当所有 $f_\alpha: Y \to \R$ 连续）。对 $U \subseteq \R^n$，称 $f: U \to M \subseteq \R^A$ **光滑**同样当且仅当所有 $f_\alpha: U \to \R$ 光滑。

我们把 $n$ 维**光滑流形**定义为 $M \subseteq \R^A$，满足对 $x \in M$ 存在开集 $U \subseteq \R^n$ 的光滑函数 $h: U \to \R^A$，将 $U$ 同胚地映到 $x$ 的一个开邻域 $V$；且对每个 $u$ 矩阵 $(\partial h_\alpha(u) / \partial u_j)$ 是秩 $n$ 的（即 $\partial h / \partial u_1, \dots, \partial h / \partial u_n$ 线性无关）。

回忆[向量丛的定义](@/posts/smooth_manifolds_1.md)，将纤维记作 $F_b(\xi)$，全空间 $B \times \R^n$ 的平凡丛用 $\varepsilon^n$ 标记。切丛算子 $T$ 于是成为光滑流形范畴到自身的函子。由于这里定义采用外蕴的观点，法丛 $\nu_M$ 的定义是容易的。对 $\R\mathrm P^n$，定义**典范线丛** $\gamma_n^1$：

$$
\begin{aligned}
    \pi: E(\gamma_n^1) \subset \R\mathrm P^n \times \R^{n+1} &\longrightarrow \R\mathrm P^n \cr
        (\set{\pm x}, v) &\longmapsto \set{\pm x} \quad (v = kx)
\end{aligned}
$$

如果局部平凡化 $h: U \times \R^n \to \pi^{-1}(U)$ 中 $U$ 可以取成整个底空间，则称这个丛是平凡丛。

{% <theorem> %}
一个 $\R^n$ 丛 $\xi$ 是平凡的当且仅当 $\xi$ 容许 $n$ 个无处相关的截面。
{% </theorem> %}

先证明，对 $B$ 上向量丛 $\xi, \eta$，连续函数 $f: E(\xi) \to E(\eta)$ 将每个向量空间 $F_b(\xi)$ 同构地映到对应的 $F_b(\eta)$，则 $f$ 给出 $\xi \cong \eta$. 设 $\xi$ 的局部坐标系 $(U, g)$ 与 $\eta$ 的局部坐标系 $(V, h)$，可验证下式是同胚：

$$(U \cap V) \times \R^n \xrightarrow{h^{-1} \circ f \circ g} (U \cap V) \times \R^n$$

故设无处相关的截面 $s_1, \dots, s_n$，证明只需要使用：

$$
\begin{aligned}
    f: B \times \R^n &\longrightarrow E \cr
        (b, x) &\longmapsto x_1 s_1(b) + \cdots + x_n s_n(b)
\end{aligned}
$$

{% <question title="习题 2-E"> %}
设 $\mu, \mu': E(\xi) \to \R$ 是同一个向量丛 $\xi$ 上两个不同的欧氏度规（连续、限制在每个纤维上正定、二次），证明存在同胚 $f: E(\xi) \to E(\xi)$ 将每个纤维同构到自身，使得 $\mu \circ f = \mu'$.
{% </question> %}

[回忆对正定的 $A$ 有唯一的 $A = (\sqrt A)^2$](@/posts/linear_algebra_2_final.md)，只要 $tI + X$ 的特征值在 $(0, 2t)$ 中，就可有效展开：

$$\sqrt{tI + X} = \sqrt t \left(I + \frac 1 {2t} X - \frac 1 {8t^2} X^2 + \cdots\right)$$

从而 $A \mapsto \sqrt A$ 是光滑的。考察 $F_b(\xi)$ 作为线性空间的一组基，$\mu, \mu'$ 对应矩阵 $A, A'$，则 $f$ 取每个纤维 $\sqrt{A}v \mapsto \sqrt{A'}v$ 即可。

## 新向量丛的构造
这一节介绍几种从已有向量丛构造新向量丛的方法。

### 诱导丛
对 $\pi: E \to B$ 与 $\bar B \subset B$，$\bar \pi: \pi^{-1}(\bar B) \to \bar B$ 给出了 $\xi$ 的一个限制。更一般地，对 $f: B_1 \to B$ 可以定义**诱导丛** $f^\ast \xi$ 使得以下图表是[拉回](@/posts/category_theory_1.md)（$E_1 = \set{(b, e) \in B_1 \times E | f(b) = \pi(e)}$）：

$$
\begin{CD}
    E_1 @>\hat f>> E \cr
    @V\pi_1VV @VV\pi V \cr
    B_1 @>>f> B
\end{CD}
$$

由于 $\pi$ 是淹没，光滑向量丛的诱导也是光滑向量丛。

{% <definition title="丛映射"> %}
从 $\eta$ 到 $\xi$ 的丛映射是连续函数 $g: E(\eta) \to E(\xi)$，将每个 $F_b(\eta)$ 同构到某个 $F_{b'}(\xi)$.
{% </definition> %}

令 $\bar g(b) = b'$，有 $\bar g: B(\eta) \to B(\xi)$ 连续。又，$\eta \cong \bar g^\ast \xi$，因为可以取泛性质给出的 $h: E(\eta) \to E(\bar g^\ast \xi)$，由之前结论它是同构。

### 积与 Whitney 和
两个向量丛 $\xi_1, \xi_2$ 的积即：

$$
\pi_1 \times \pi_2: E_1 \times E_2 \to B_1 \times B_2 \\\\
(\pi_1 \times \pi_2)^{-1} (b_1, b_2) = F_{b_1}(\xi_1) \times F_{b_2}(\xi_2)
$$

设 $\Delta: B \to B \times B$ 是对角线嵌入，对 $B$ 上的丛 $\xi_1, \xi_2$，定义其 **Whitney 和**：

$$\xi_1 \oplus \xi_2 = \Delta^\ast(\xi_1 \times \xi_2)$$

读者可见可以将 $E$ 视作 $\set{(e_1, e_2, b) | \pi_1(e_1) = \pi_2(e_2) = b}$，故 $F_b(\xi_1 \oplus \xi_2) \cong F_b(\xi_1) \oplus F_b(\xi_2)$.

### 正交补
称 $\xi$ 是 $\eta$ 的子丛即要求 $E(\xi) \subseteq E(\eta)$，每个纤维对应是子向量空间。此时，在每个纤维取正交补即得**正交补** $\xi^\perp$，满足 $\eta = \xi \oplus \xi^\perp$，易验证确实满足局部平凡化条件。

对 $M \subseteq N$ 及 $N$ 配备的 Riemann 度规，切丛 $\tau_M$ 的正交补即法丛，且有：

$$\tau_M \oplus \nu \cong \tau_N|_M$$

对浸入 $f: M \to N$ 及 $N$ 配备的 Riemann 度规，将如下分解中的正规补 $\nu_f$ 称为 $f$ 的法丛：

$$f^\ast \tau_N = \tau_M \oplus \nu_f$$

### 向量空间的连续函子
我们可以考虑一般的 $T: \mathbf{Vect} _{\R} \times \cdots \times \mathbf{Vect} _{\R} \to \mathbf{Vect} _{\R}$ 函子，并要求对分量连续，可以定义丛 $T(\xi_1, \dots, \xi_k)$，且如果 $\xi_1, \dots, \xi_k$ 是光滑向量丛则 $T(\xi_1, \dots, \xi_k)$ 也有光滑结构。

张量积对应于丛的张量积；直和对应于 Whitney 和；对偶函子 $V \mapsto \mathrm{Hom}(V, \R)$ 将丛 $\xi$ 对应于对偶丛 $\mathrm{Hom}(\xi, \varepsilon^1)$.

## Stiefel-Whitney 类
### 四个公理
回忆对交换环 $\Lambda$ 可以定义[奇异链群](@/posts/homology_1.md) $C_n(X; \Lambda)$，考虑其对偶：

$$C^n(X; \Lambda) \coloneqq \mathrm{Hom}_\Lambda(C_n(X; \Lambda), \Lambda)$$

可以由下式给出余边缘，进而 $\ker \delta_n / \operatorname{Im} \delta_{n-1}$ 给出奇异上同调群 $\mathsf H^n(X; \Lambda)$：

$$
\begin{aligned}
\delta_n: C^n(X; \Lambda) &\longrightarrow C^{n+1}(X; \Lambda) \\\\
    c &\longmapsto (\alpha \mapsto (-1)^{n+1} c(\partial \alpha))
\end{aligned}
$$

这里我们总是取 $\Lambda = \Z/2$，给出理论的 4 条公理（其存在唯一性在之后证明）：

{% <info title="公理 1"> %}
每个向量丛 $\xi$ 对应一族上同调类，称为 Stiefel-Whitney 类：

$$\mathrm w_i(\xi) \in \mathsf H^i(B(\xi); \Z/2)$$
{% </info> %}

注意对道路连通的 $B$ 有 $\mathsf H^0(B(\xi); \Z/2) \cong \Z/2$，约定取类 $\mathrm w_0(\xi)$ 等于 $1$；对 $n$ 维向量丛，$i > n$ 时 $\mathrm w_i(\xi)$ 为零。

{% <info title="公理 2（自然性）"> %}
若 $f: B(\xi) \to B(\eta)$ 来自于一个丛映射，则：

$$\mathrm w_i(\xi) = f^\ast \mathrm w_i(\eta)$$
{% </info> %}

{% <info title="公理 3（Whitney 积定理）"> %}
$$\mathrm w_k(\xi \oplus \eta) = \sum_{i=1}^k \mathrm w_i(\xi) \smile \mathrm w_{k-i}(\eta)$$
{% </info> %}

之后我们会省略 cup product 符号。

{% <info title="公理 4"> %}
对 $\R\mathrm P^1$ 上的典范线丛 $\gamma_1^1$，其 $\mathrm w_1(\gamma_1^1)$ 非零。
{% </info> %}

由四条公理易见 $\xi \cong \eta$ 时 Stiefel-Whitney 类相同。平凡丛 $\varepsilon$ 在 $i > 0$ 时 $\mathrm w_i(\varepsilon) = 0$，因为存在从 $\varepsilon$ 到单点上向量丛的丛映射。进而 $\mathrm w_i(\varepsilon \oplus \eta) = \mathrm w_i(\eta)$.

如果 $n$ 维丛 $\xi$ 有 $k$ 个无处线性相关的截面，则可以拆成 $\xi = \varepsilon \oplus \varepsilon^\perp$，从而 $\mathrm w_{n-k+1}(\xi) = \cdots = \mathrm w_n(\xi) = 0$.

当 $\xi \oplus \eta$ 时，可以归纳地将 $\mathrm w_i(\eta)$ 解出，写成关于 $\mathrm w_i(\xi)$ 的多项式。为此我们引入：

{% <definition> %}
用 $\mathsf H^\Pi(B; \Z/2)$ 表示由如下无限序列构成的环：

$$a = a_0 + a_1 + \cdots$$

其中 $a_i \in \mathsf H^i(B; \Z/2)$，乘法按多项式意义。由于在 $\bmod 2$ 意义下，它是交换的。
{% </definition> %}

$n$ 维丛的全 Stiefel-Whitney 类于是定义成它的元素：

$$\mathrm w(\xi) = 1 + \mathrm w_1(\xi) + \cdots + \mathrm w_n(\xi) + 0 + \cdots$$

前导 $1$ 的序列总是有逆，记作 $\bar{\mathrm w}$，可以用幂级数展开写出：

$$
\begin{split} \bar{\mathrm w}
&= [1 + (\mathrm w_1 + \mathrm w_2 + \cdots)]^{-1} \\\\
&= 1 - (\mathrm w_1 + \mathrm w_2 + \cdots) + (\mathrm w_1 + \mathrm w_2 + \cdots)^2 - \cdots \\\\
&= 1 - \mathrm w_1 + (\mathrm w_1^2 - \mathrm w_2) + \cdots
\end{split}
$$

{% <theorem title="Whitney duality theorem"> %}
嵌入欧氏空间的流形 $M$ 切丛 $\tau_M$，法丛 $\nu$，则 $\mathrm w(\nu) = \bar{\mathrm w}(\tau_M)$.
{% </theorem> %}

一个典型例子是，由于 $\mathbb S^n$ 的法丛的全类平凡，其切丛的全类平凡。

### 射影空间的丛
我们先刻画 $\R\mathrm P^n$ 的 $\pmod 2$ 上同调类。实际上对 $0 \leq i \leq n$ 有 $\mathsf H^i(\R\mathrm P^n; \Z/2)$ 是 $2$ 阶循环群（对更高的 $i$ 是零），且设 $\mathsf H^1(\R\mathrm P^n; \Z/2)$ 生成元 $a$ 则 $\mathsf H^i(\R\mathrm P^n; \Z/2)$ 生成元 $a^i$（$i$ 重 cup product）。

考虑嵌入 $j: \R\mathrm P^1 \to \R\mathrm P^n$，有 $\mathrm w_1(\gamma_n^1)$ 非零，故 $\mathrm w(\gamma_n^1) = 1 + a$，其在平凡丛 $\varepsilon^{n+1}$ 的正交补就有：

$$\mathrm w(\gamma^\top) = 1 + a + \cdots + a^n$$

讨论可知 $\tau(\R\mathrm P^n) \cong \mathrm{Hom}(\gamma_n^1, \gamma^\perp)$，从而有：

$$
\begin{split} \tau \oplus \varepsilon^1
&\cong \mathrm{Hom}(\gamma_n^1, \gamma^\perp) \oplus \mathrm{Hom}(\gamma_n^1, \gamma_n^1) \\\\
&\cong \mathrm{Hom}(\gamma_n^1, \varepsilon^{n+1}) \\\\
&\cong \mathrm{Hom}(\gamma_n^1, \varepsilon^1)^{n+1} \\\\
&\cong (\gamma_n^1)^{n+1}
\end{split}
$$

$$\mathrm w(\tau) = 1 + \binom{n + 1}{1} a + \cdots + \binom{n + 1}{n} a^n$$

于是可以得到一个 $\pmod 2$ 意义下的 Pascal 三角（去掉右侧列）。有 $\mathrm w(\tau(\R\mathrm P^n)) = 1$ 当且仅当 $n + 1$ 为 $2$ 的幂。从而只有这些是有可能可平行化的。实际上只有其中 $\R\mathrm P^1, \R\mathrm P^3, \R\mathrm P^7$ 可平行化。如果存在 $\R$ 上 $n$ 可除代数则 $\R\mathrm P^{n-1}$ 可平行化，由此给出关于可除代数的结论。

由于 $\mathrm w(\R\mathrm P^9) = 1 + a^2 + a^8$，其逆是 $1 + a^2 + a^4 + a^6$，故如果 $\R\mathrm P^9$ 能浸入 $\R^{n+k}$，则 $k$ 至少是 $6$；类似地可以看到 [Whitney 系定理](@/posts/smooth_manifolds_3.md)浸入的界。

### Stiefel-Whitney 数
对于闭、可能不连通的光滑 $n$ 维流形，有一个基本同调类：

$$\mu_M \in \mathsf H_n(M; \Z/2)$$

故对 $v \in \mathsf H^n(M; \Z/2)$ 可以定义 Kronecker index $\braket{v, \mu_M} \in \Z/2$.

对非负整数 $r_1, \dots, r_n$ 满足 $r_1 + 2r_2 + \cdots + nr_n = n$，定义 $M$ 关于单项式 $\mathrm w_1(\tau_M)^{r_1} \cdots \mathrm w_n(\tau_M)^{r_n}$ 的 **Stiefel-Whitney 数**是：

$$\braket{\mathrm w_1(\tau_M)^{r_1} \cdots \mathrm w_n(\tau_M)^{r_n}, \mu_M}$$

{% <theorem title="(Pontrjagin)"> %}
若 $B$ 是紧 $n + 1$ 维光滑流形，边界为 $M$，则 $M$ 的所有 Stiefel-Whitney 数均为零。
{% </theorem> %}

取 $\mu_B \in \mathsf H_{n+1}(B, M; \Z/2)$ 及自然将 $\mu_B$ 映到 $\mu_M$ 的：

$$\partial: \mathsf H_{n+1}(B, M) \to \mathsf H_n(M)$$

取定 $\tau_B$ 上欧氏度规，有：

$$\tau_B|_M \cong \tau_M \oplus \varepsilon^1$$

$$\mathsf H^n(B) \xrightarrow{i^\ast} \mathsf H^n(M) \xrightarrow{\delta} \mathsf H^{n+1}(B, M)$$

$$\braket{\mathrm w_1(\tau_M)^{r_1} \cdots \mathrm w_n(\tau_M)^{r_n}, \partial \mu_B} = \braket{\delta(\mathrm w_1(\tau_M)^{r_1} \cdots \mathrm w_n(\tau_M)^{r_n}), \mu_B} = 0$$

{% <theorem title="(Thom)"> %}
$M$ 的所有 Stiefel-Whitney 数均为零，则可以被实现为某个紧光滑流形的边界。
{% </theorem> %}

证明略。

我们说两个 $n$ 维闭光滑流形 $M_1, M_2$ 属于同一（无定向）配边类，如果 $M_1 \sqcup M_2$ 可以被实现为一个紧光滑流形的边界。可知属于同一配边类当且仅当所有对应 Stiefel-Whitney 数相等。
