+++
title = "嵌入"
draft = true

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

## 零测集
{% admonition(type="theorem", title="引理") %}
若 $A \subset \R^n$ 紧，且对任意 $c$，$A \cap \set{c} \times \R^{n-1}$ 在 $(n - 1)$ 维零测，则 $A$ 零测。
{% end %}

分析即可。

{% admonition(type="theorem", title="定理") %}
若 $A \subset \R^n$ 零测，$F: A \to \R^n$ 光滑，则 $F(A)$ 零测。
{% end %}

这里光滑定义为开集 $U \supset A$ 上光滑映射（也记为 $F$）的限制。对每一点 $p$，存在 $B_p$ 使得 $F$ 在其上是 Lipschitz 的。由 $\R^n$ 的第二可数性，$\set{B_p}$ 有可数子覆盖，记为 $\set{B_i}$. 令 $A_i = B_i \cap A$.

对每个 $A_i$ 设对应 Lipschitz 常数 $L_i$，依零测定义分析知像零测，其可数并也零测。

{% admonition(type="definition", title="零测") %}
$A$ 在光滑流形 $M$ 上零测，如果对每个光滑图册 $(U, \varphi)$ 有 $\varphi(A \cap U)$ 零测。
{% end %}

实际上只需要对一族可以覆盖 $A$ 的图卡检查即可。由此，之前定理的结论容易推广到光滑流形间的光滑映射。

{% admonition(type="theorem", title="Sard 定理") %}
设光滑流形 $M, N$，光滑映射 $F: M \to N$，则临界值集在 $N$ 中零测。
{% end %}

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

## Whitney 定理
{% admonition(type="theorem", title="引理") %}
设 $M \subseteq \R^N$ 是紧致光滑 $n$ 维子流形。若 $N > 2n + 1$，则存在满秩线性映射 $\pi: \R^N \to \R^{N-1}$，使得 $\pi|_M$ 仍是光滑嵌入。
{% end %}

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

{% admonition(type="theorem", title="引理") %}
设 $M$ 是光滑 $n$ 维流形，若对某个 $N$ 它可以光滑嵌入到 $\R^N$，则它可以正则光滑嵌入到 $\R^{2n+1}$.
{% end %}

对 $\R^n$ 中的 $1$ 维线性子空间 $S$ 及 $R > 0$，定义以 $S$ 为轴、$R$ 为半径的管：

$$T_R(S) = \set{x \in \R^N | d(x, y) < R, \\, \exists y \in S}$$

{{ todo() }}

{% admonition(type="theorem", title="Whitney 嵌入定理") %}
所有 $n$ 维光滑流形同胚于一个正则嵌入 $\R^{2n+1}$ 的子流形。
{% end %}

{{ todo() }}

{% admonition(type="theorem", title="强 Whitney 嵌入定理") %}
对 $n > 0$，所有 $n$ 维光滑流形可以光滑嵌入 $\R^{2n}$.
{% end %}

{% admonition(type="theorem", title="强 Whitney 浸入定理") %}
对 $n > 1$，所有 $n$ 维光滑流形可以光滑浸入 $\R^{2n-1}$.
{% end %}

上面两定理通过很精密的代拓技巧得到，超出本文范围。关于浸入的最优界是 $\R^{2n-a(n)}$，其中 $a(n)$ 是 $n$ 的二进制展开中 $1$ 的数量。关于嵌入，$3$ 维流形最优界是嵌入 $\R^5$，但仍有很多最优界尚未知晓。
