+++
title = "示性类（一）：Stiefel-Whitney 类"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

Milnor 的 *Characteristic Classes* 1-4 节。

## 基本定义
这里我们扩展一下光滑流形的定义，允许无穷维。设 $A$ 是指标集，$\R^A$ 包含所有 $A \to \R$ 函数，给予相对拓扑（拓扑空间 $Y$，$f: Y \to M \subseteq \R^A$ 连续当且仅当所有 $f_\alpha: Y \to \R$ 连续）。对 $U \subseteq \R^n$，称 $f: U \to M \subseteq \R^A$ **光滑**同样当且仅当所有 $f_\alpha: U \to \R$ 光滑。

我们把 $n$ 维**光滑流形**定义为 $M \subseteq \R^A$，满足对 $x \in M$ 存在开集 $U \subseteq \R^n$ 的光滑函数 $h: U \to \R^A$，将 $U$ 同胚地映到 $x$ 的一个开邻域 $V$；且对每个 $u$ 矩阵 $(\partial h_\alpha(u) / \partial u_j)$ 是秩 $n$ 的（即 $\partial h / \partial u_1, \dots, \partial h / \partial u_n$ 线性无关）。

回忆[向量丛的定义](@/posts/smooth_manifolds_1.md)，将纤维记作 $F_b(\xi)$. 切丛算子 $T$ 于是成为光滑流形范畴到自身的函子。由于这里定义采用外蕴的观点，法丛 $\nu_M$ 的定义是容易的。对 $\R\mathrm P^n$，定义典范线丛 $\gamma_n^1$：

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
这一节介绍几种从已有向量丛构造新向量丛的方法。对 $\pi: E \to B$ 与 $\bar B \subset B$，$\bar \pi: \pi^{-1}(\bar B) \to \bar B$ 给出了 $\xi$ 的一个限制。更一般地，对 $f: B_1 \to B$ 可以定义诱导丛 $f^\ast \xi$ 使得以下图表是[拉回](@/posts/category_theory_1.md)（$E_1 = \set{(b, e) \in B_1 \times E | f(b) = \pi(e)}$）：

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
