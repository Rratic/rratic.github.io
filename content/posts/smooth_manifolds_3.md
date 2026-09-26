+++
title = "【草稿】光滑流形（三）：嵌入"
date = 2026-09-23

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

## 零测集
{% <theorem title="引理"> %}
若 $A \subset \R^n$ 紧，且对任意 $c$，$A \cap \set{c} \times \R^{n-1}$ 在 $(n - 1)$ 维零测，则 $A$ 零测。
{% </theorem> %}

分析即可。

{% <theorem title="定理"> %}
若 $A \subset \R^n$ 零测，$F: A \to \R^n$ 光滑，则 $F(A)$ 零测。
{% </theorem> %}

这里光滑定义为开集 $U \supset A$ 上光滑映射（也记为 $F$）的限制。对每一点 $p$，存在 $B_p$ 使得 $F$ 在其上是 Lipschitz 的。由 $\R^n$ 的第二可数性，$\set{B_p}$ 有可数子覆盖，记为 $\set{B_i}$. 令 $A_i = B_i \cap A$.

对每个 $A_i$ 设对应 Lipschitz 常数 $L_i$，依零测定义分析知像零测，其可数并也零测。

{% <definition title="零测"> %}
$A$ 在光滑流形 $M$ 上零测，如果对每个光滑图册 $(U, \varphi)$ 有 $\varphi(A \cap U)$ 零测。
{% </definition> %}

实际上只需要对一族可以覆盖 $A$ 的图卡检查即可。由此，之前定理的结论容易推广到光滑流形间的光滑映射。

{% <theorem title="Sard 定理"> %}
设光滑流形 $M, N$，光滑映射 $F: M \to N$，则临界值集在 $N$ 中零测。
{% </theorem> %}

这里临界点是指 Jacobi 矩阵秩小于 $n$ 的点。此定理一个推论是 $\dim M < \dim N$ 时 $F(M)$ 在 $N$ 中零测（光滑改为连续时这个结果不正确，反例是空间填充曲线）。

对 $m = \dim M$ 归纳（$m = 0$ 时平凡）。

对 $m \geq 1$，考察 $F$ 是从 $\R^m$ 的开集 $U$ 到 $\R^n$ 的光滑映射，令 $U$ 的坐标系 $(x^1, \dots, x^m)$，陪域的坐标系 $(y^1, \dots, y^n)$. 我们记临界值集 $C$，并令：

$$C_k = \left\\{x \in C \middle| \frac {\partial f_j} {\partial x_i} = 0, \\, 1 \leq i \leq k\right\\}$$

$$C \supseteq C_1 \supseteq C_2 \supseteq \cdots$$

由连续性，$C$ 及所有的 $C_k$ 在 $U$ 中闭。先证明 $F(C \setminus C_1)$ 零测：考虑 $F$ 限制在 $C \setminus C_1$ 上，对其中一点 $a$，不妨设 $\partial F^1 / \partial x^1 (a) \neq 0$. 这意味着可以在 $a$ 的某个邻域上定义新的光滑坐标 $(F^1, x^2, \dots, x^m)$，其中：

$$\operatorname{Jac} F = \begin{pmatrix} 1 & 0 \cr \ast & \frac {\partial F^i} {\partial v^j} \end{pmatrix}$$

使用归纳条件，再用引理，再取可数并即可。类似地可以证明 $F(C_k \setminus C_{k+1})$ 零测。

最后证明 $k > m / n - 1$ 时 $F(C_k)$ 零测。对 $a \in U$ 考察包含它的闭方块 $E \subseteq U$. 取待定的 $K$ 将 $E$ 分成 $K^m$ 块。对一块 $E_i \ni x$ 及 $a_i \in C_k \cap E_i$，令 $A$ 为 $F$ 在 $E$ 中所有 $(k+1)$ 阶导绝对值的上界，用 Taylor 定理有：

$$|F(x) - F(a_i)| \leq A' |x - a_i|^{k+1}$$

$$\mathrm{Vol}(F(C_k \cap E)) \leq A'' K^{m - n - nk}$$

## 嵌入
### 定义
考虑光滑流形 $M, N$ 及光滑映射 $f: M \to N$，其微分：

$$\mathrm df| _p: T _p M \to T _{f(p)} N$$

{% <definition title="浸入（immersion）"> %}
称 $f$ 是浸入，如果对每个 $p$，有 $\mathrm df|_p$ 是单射。
{% </definition> %}

这等价于说：

$$\operatorname{rank} J(\mathrm df|_p) = \dim M$$

{% <definition title="嵌入（embedding）"> %}
称 $f$ 是嵌入，如果它是浸入，且 $M \to f(M)$ 是同胚（后者取子空间拓扑）。
{% </definition> %}

所谓正则嵌入（regular/proper embedding）是指对每个 $N$ 的紧集 $K$，其原像 $f^{-1}(K)$ 在 $M$ 中是紧的。注意有紧流形 $M$ 到 Hausdorff 的 $N$ 的单射浸入一定是嵌入，嵌入一定是正则嵌入。如果单射浸入是正则（proper）的，则它是嵌入。

{% <definition title="浸没（submersion）"> %}
称 $f$ 是浸没，如果 $f$ 在每一点都是正则点（不是临界点），也即 $\mathrm df|_p$ 处处满射。
{% </definition> %}

### Whitney 定理
{% <theorem title="引理"> %}
设 $M \subseteq \R^N$ 是紧致光滑 $n$ 维子流形。若 $N > 2n + 1$，则存在满秩线性映射 $\pi: \R^N \to \R^{N-1}$，使得 $\pi|_M$ 仍是光滑嵌入。
{% </theorem> %}

把 $\pi$ 取成沿某条直线 $L$ 到超平面的投影。它在 $M$ 上不是单射，当且仅当 $L$ 平行于某条割线 $p-q$；它不是浸入，当且仅当 $L$ 平行于某个非零切向量。因而只需在 $\R\mathrm P^{N-1}$ 中避开以下两个集合：

$$
\begin{align*}
	\kappa &: M \times M \setminus \Delta_M \to \R\mathrm P^{N-1} &
		\kappa(p, q) &= [p - q] \cr
	\tau &: TM \setminus (M \times \set{\mathbf 0}) \to \R\mathrm P^{N-1} &
		\tau(p, w) &= [w]
\end{align*}
$$

前一个定义域维数为 $2n$，后一个定义域维数为 $2n-1$，而陪域维数 $N-1$. 由 Sard 定理的推论，它们的像都是零测集，故可以取像外的一条直线作投影方向，所得限制既单又是浸入；由于 $M$ 紧，它就是嵌入。

{% <theorem title="引理"> %}
设 $M$ 是光滑 $n$ 维流形，若对某个 $N$ 它可以光滑嵌入到 $\R^N$，则它可以正则光滑嵌入到 $\R^{2n+1}$.
{% </theorem> %}

对 $\R^n$ 中的 $1$ 维线性子空间 $S$ 及 $R > 0$，定义以 $S$ 为轴、$R$ 为半径的管：

$$T_R(S) = \set{x \in \R^N | d(x, y) < R, \\, \exists y \in S}$$

设 $F: M \to \R^N$ 是一个光滑嵌入，$G: \R^N \to \mathbb B^N$ 是微分同胚，$f: M \to \R$ 是光滑穷竭函数[^exhaustion]，令：

$$
\begin{aligned}
\Psi: M & \longrightarrow \R^N \times \R \cr
    p & \longmapsto (G \circ F(p), f(p))
\end{aligned}
$$

那么 $\Psi$ 是一个单射浸入。将 $M$ 对应到它的像，则可以视作 $\R^{N+1}$ 的一个正则光滑嵌入子流形，且包含在某个管中。用前一引理找到合适的投影方向，我们知道两个轴不平行的管的交是有界的，故可以不断操作至 $2n + 1$ 维。

{% <theorem title="Whitney 嵌入定理"> %}
$n$ 维光滑流形可以正则嵌入 $\R^{2n+1}$.
{% </theorem> %}

只需证可以光滑嵌入某个 $\R^N$. 先考虑 $M$ 紧情形。取有限覆盖 $\set{B_1, \dots, B_m}$，其中 $B_i$ 是某个局部坐标系 $B_i' \supseteq \bar B_i$ 的球。令 $\rho_i: M \to \R$ 在 $\bar B_i$ 上为 $1$，被 $B_i'$ 支撑。定义 $F: M \to \R^{nm + m}$ 是：

$$F(p) = (\rho_1(p)\varphi_1(p), \dots, \rho_m(p)\varphi_m(p), \rho_1(p), \dots, \rho_m(p))$$

读者易验证这是单射浸入，从而是嵌入。

现在考虑 $M$ 不紧。对光滑穷竭函数 $f$ 由 Sard 定理，对 $i$ 有正则值（原像均是正则点）$a_i, b_i$ 在 $(i, i+1)$ 中。定义：

$$
D_0 = f^{-1}((-\infty, 1]), \\, D_i = f^{-1}([i, i+1]) \\\\
E_0 = f^{-1}((-\infty, a_1]), \\, E_i = f^{-1}([b_{i-1}, a_{i+1}])
$$

取 $\varphi_i: E_i \to \R^{2n+1}$；$\rho_i: M \to \R$ 在 $D_i$ 为 $1$ 被 $E_i$ 支撑。定义 $F: \R^{2n+1} \times \R^{2n+1} \times \R$ 为：

$$F(p) = \left(\sum_{i \text{ even}} \rho_i(p) \varphi_i(p), \sum_{i \text{ odd}} \rho_i(p) \varphi_i(p), f(p)\right)$$

易见 $F$ 是单射浸入且正则，故是嵌入。

{% <theorem title="Whitney 浸入定理"> %}
$n$ 维光滑流形可以浸入 $\R^{2n}$.
{% </theorem> %}

证明略。

{% <theorem title="强 Whitney 嵌入定理"> %}
对 $n > 0$，所有 $n$ 维光滑流形可以光滑嵌入 $\R^{2n}$.
{% </theorem> %}

{% <theorem title="强 Whitney 浸入定理"> %}
对 $n > 1$，所有 $n$ 维光滑流形可以光滑浸入 $\R^{2n-1}$.
{% </theorem> %}

上面两定理通过很精密的代拓技巧得到，超出本文范围。关于浸入的最优界是 $\R^{2n-a(n)}$，其中 $a(n)$ 是 $n$ 的二进制展开中 $1$ 的数量。关于嵌入，$3$ 维流形最优界是嵌入 $\R^5$，但仍有很多最优界尚未知晓。

---

[^exhaustion]: 拓扑空间 $M$ 上的穷竭函数 $f$ 是连续的 $M \to \R$，满足对任意 $c$，$f^{-1}(-\infty, c]$ 紧。对光滑流形，取可数基 $\set{V_j}_{j=1}^\infty$ 及对应单位分解 $\set{\psi_i}$，令：

$$f(p) = \sum_{j=1}^\infty j \psi_j(p)$$
