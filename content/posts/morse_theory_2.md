+++
title = "Morse 理论（二）：在李群上的应用"
date = 2026-10-18

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

今天讲的讨论班（似乎是我第一次讲讨论班）的内容。前置是 [Morse 理论第一部分](@/posts/morse_theory_1.md)与[所需的 Riemann 几何](@/posts/riemannian_geometry_1.md)。

<!-- more -->

## 对称空间
称连通 Riemann 流形是**对称空间**，如果对任一点 $p$，存在等距同构 $I_p$ 保持 $p$，且让过 $p$ 的测地线反向。

对测地线 $\gamma$ 及 $p = \gamma(0), q = \gamma(c)$，有 $I_qI_p(\gamma(t)) = \gamma(t + 2c)$，故 $I_p I_q$ 保持沿 $\gamma$ 的向量场。故对称空间中测地线可以无限延申，故 $M$ 测地完备，故 $I_p$ 唯一。

又，对沿 $\gamma$ 平行的向量场 $U, V, W$，设 $p = \gamma(0), q = \gamma(c)$，令 $T = I_{\gamma(c/2)} I_{\gamma(0)}$，另一沿 $\gamma$ 平行的向量场 $X$，有：

$$\braket{R(U_q, V_q)W_q, X_q} = \braket{R(T_\ast U_p, T_\ast V_p) T_\ast W_p, T_\ast X_p} = \braket{R(U_p, V_p)W_p, X_p}$$

故 $\braket{R(U, V)W, X}$ 沿着 $\gamma$ 常值，作 $\nabla_{\dot \gamma}$ 然后用 $X$ 任意性得 $R(U, V)W$ 沿 $\gamma$ 平行。

我们称满足这个性质的 Riemann 流形是局部对称的，一个事实是完备、单连通、局部对称的 Riemann 流形是对称空间。

对局部对称流形的测地线 $\gamma: \R \to M$，设 $p = \gamma(0), V = \dot \gamma(0)$，令线性变换：

$$
\begin{aligned}
K_V: T_pM &\longrightarrow T_pM \\\\
    W &\longmapsto R(V, W)V
\end{aligned}
$$

记它的特征值是 $e_1, \dots, e_n$.

{% <theorem> %}
沿着 $\gamma$ 与 $p$ 共轭的点是那些 $\gamma(\pi k / \sqrt{e_i})$，其中 $k$ 是非零整数，$e_i$ 是正的特征值。$\gamma(t)$ 重数即满足 $\pi / \sqrt{e_i} \mid t$ 的 $e_i$ 个数。
{% </theorem> %}

注意到 $K_V$ 自伴，取单位正交基使得 $K_V(U_i) = e_iU_i$，通过平行移动让 $U_i$ 补充定义到整个 $\gamma$，由局部对称处处有：

$$R(V, U_i)V = e_iU_i$$

故每一个沿着 $\gamma$ 的向量场 $W$ 都可以写成：

$$W(t) = w_1(t)U_1(t) + \cdots + w_n(t)U_n(t)$$

于是 Jacobi 方程 $\ddot W + K_V(W) = 0$ 即：对每个 $i$，

$$\ddot w_i + e_iw_i = 0$$

因为考虑的点在 $0$ 处消没，有：

$$
w_i(t) = \begin{cases}
    \boxed{c_i \cdot \sin(\sqrt{e_i} t)} & e_i > 0 \\\\
    c_i t & e_i = 0 \\\\
    c_i \cdot \sinh(\sqrt{-e_i} t) & e_i < 0
\end{cases}
$$

## 李群作为对称空间
对于李群（有有限维实光滑流形结构、群运算是光滑映射），考察对左、右平移不变的 Riemann 度规。对于交换群只需任取 $\braket{\cdot, \cdot}_e$ 然后用左平移不变推广。对于紧群，取 Haar 测度[^haar-measure] $\mu$ 及任一度规，然后让：

$$\braket{\braket{V, W}} = \int_{G \times G} \braket{L_{\sigma\ast}R_{\tau\ast}(V), L_{\sigma\ast}R_{\tau\ast}(W)} \mathrm d\mu(\sigma) \mathrm d\mu(\tau)$$

{% <theorem> %}
若李群 $G$ 有左、右平移不变的 Riemann 度规，则 $G$ 是对称空间。
{% </theorem> %}

$$I_\tau(\sigma) = \tau \sigma^{-1} \tau$$

{% <theorem> %}
$G$ 中满足 $\gamma(0) = e$ 的测地线恰是 $G$ 的 $1$-参数子群（$C^\infty$ 的 $\R \to G$ 同态）。
{% </theorem> %}

一方面，$\gamma(t)\gamma(u)\gamma(t) = \gamma(u + 2t)$，有 $\gamma(nt) = \gamma(t)^n$，故对 $t'/t''$ 有理数有 $\gamma(t' + t'') = \gamma(t')\gamma(t'')$，用连续性知 $\gamma$ 是同态。

另一方面，$1$-参数子群被 $e$ 处切向量唯一决定。

我们称向量场 $X$ 是**左不变**的。如果对任意 $a, b$ 有 $(L _a) _\ast (X_b) = X _{a \cdot b}$，所有左不变的向量场在 $[,]$ 下构成一个李代数，记作 $\mathfrak g$.

对左不变的向量场的 $X$，其积分曲线给出 $1$-参数子群的左平移，从而 $\nabla_X X = 0$，有 $[X, Y] = 2\nabla_X Y$，故：
- $\braket{[X, Y], Z} = \braket{X, [Y, Z]}$
- $R(X, Y)Z = \frac 1 4 [[X, Y], Z]$
- $\braket{R(X, Y)Z, W} = \frac 1 4 \braket{[X, Y], [Z, W]}$，故截面曲率 $\geq 0$

{% <theorem> %}
若李群 $G$ 有左、右平移不变的 Riemann 度规，$\mathfrak g$ 的中心平凡，则 $G$ 紧。从而，紧的半单李群导致其基本群有限。
{% </theorem> %}

对单位向量 $X_1$，扩展到单位正交基 $X_1, \dots, X_n$，有：

$$\mathrm{Ric}(X_1, X_1) = \sum_{i=1}^n \braket{R(X_1, X_i)X_1, X_i} > 0$$

由紧知恒 $\geq c > 0$，由 [Myers 的定理](@/posts/riemannian_geometry_1.md)知 $G$ 紧。

{% <theorem> %}
若单连通李群 $G$ 有左、右平移不变的 Riemann 度规，则可拆成 $G' \times \R^k$，其中 $G'$ 中心平凡、紧，$\R^k$ 表示加法李群。
{% </theorem> %}

设 $\mathfrak g$ 的中心 $c$，其正交补：

$$\mathfrak g' = \set{X | \braket{X, C} = 0, \\, \forall C \in c}$$

这是子李代数，因为：

$$\braket{[X, Y], C} = \braket{X, [Y, C]}$$

$\mathfrak g = \mathfrak g' \oplus c$ 给出 $G = G' \times G''$，后者单连通且交换，故同构于某个 $\R^k$.

{% <theorem title="Bott 的定理"> %}
对紧、单连通李群 $G$，$\Omega(G)$ 的 CW-复形同伦型中没有奇数维胞腔，每个偶数维数只有有限个胞腔。
{% </theorem> %}

实际上这样的 CW-复形总是无穷维的，如 $\mathsf H_{2i}\Omega(\mathbb S^3) = \Z$；整个定理对不紧的群也成立，因为任意连通李群可以形变收缩成紧的李群。

取 $p, q$ 不沿任意测地线共轭，由 Morse 理论基本定理，每个 $p$ 到 $q$ 指数 $\lambda$ 测地线对应一个 $\Omega_p^q(G)$ 同伦型中的 $\lambda$ 维胞腔。由 Myers 的定理的一个推论，根据 Ricci 张量处处正定，每个维数 $\lambda$ 只有有限个胞腔。

取起点 $p$ 的测地线 $\gamma$，设 $V = \dot \gamma(0) \in T_pG \cong \mathfrak g$，根据上一节的定理，共轭点只与 $K_V$ 的特征值有关。我们令：

$$
\begin{aligned}
\operatorname{Ad} V: \mathfrak g &\longrightarrow \mathfrak g \\\\
    W &\longmapsto [V, W]
\end{aligned}
$$

$$K_V = \frac 1 4 [[V, W], V] = - \frac 1 4 (\operatorname{Ad} V) \circ (\operatorname{Ad} V)$$

$\operatorname{Ad} V$ 反对称，故可取 $\mathfrak g$ 的单位正交基使之为：

$$
\begin{pmatrix}
0 & a_1 \\\\
-a_1 & 0 \\\\
~ & ~ & 0 & a_2 \\\\
~ & ~ & -a_2 & 0 \\\\
~ & ~ & ~ & ~ & \ddots
\end{pmatrix}
$$

$$
(\operatorname{Ad} V) \circ (\operatorname{Ad} V) =
\begin{pmatrix}
-a_1^2 \\\\
~ & -a_1^2 \\\\
~ & ~ & -a_2^2 \\\\
~ & ~ & ~ & -a_2^2 \\\\
~ & ~ & ~ & ~ & \ddots
\end{pmatrix}
$$

特征值成对出现，故每个共轭点重数为偶，由指数定理，所有测地线指数为偶。

## 最短测地线的整个流形
之前我们考虑了对一般位置 $p, q$ 的 $\Omega_p^q$，实际上选取特定位置的点可以得到许多有用的结果。如考虑 $\mathbb S^{n+1}$ 的对径点，其间测地线对应于赤道 $\mathbb S^n$.

以下设 $M$ 完备，点 $p, q$ 的距离 $\rho(p, q) = \sqrt{d}$，用 $\Omega^c$ 表示 $E^{-1}([0, c])$，那么 $\Omega^d$ 即最短测地线的空间。

{% <theorem> %}
若 $\Omega^d$ 是拓扑流形，且所有 $p$ 到 $q$ 非最短的测地线指数 $\geq \lambda_0$，则对 $0 \leq i < \lambda_0$，相对同伦群 $\pi_i(\Omega, \Omega^d)$ 是零。
{% </theorem> %}

证明略。其推论是，对 $i \leq 2\lambda_0 - 2$，嵌入 $\pi_i(\Omega^d) \to \pi_i(\Omega)$ 是同构，使用相对 Hurewicz 定理 $\mathsf H_i(\Omega^d) \cong \mathsf H_i(\Omega)$；又，一个著名定理告诉我们 $\pi_i(\Omega) \cong \pi_{i+1}(M)$.

{% <theorem title="Freudenthal 悬挂定理"> %}
对 $i \leq 2n - 2$，有同构 $\pi_i(\mathbb S^n) \cong \pi_{i+1}(\mathbb S^{n+1})$.
{% </theorem> %}

取对径点，非最短的测地线至少经过两个重数 $n$ 的点，故条件 $\lambda_0 = 2n$ 成立；使用赤道的对应。

## 酉群的 Bott 周期性定理
回忆 $\mathrm U(n) \subset \Complex^{n \times n}$ 包含的是那些保内积的线性变换。我们对矩阵定义：

$$\exp A = I + A + \frac 1 {2!} A^2 + \frac 1 {3!} A^3 + \cdots$$

我们有以下性质：
- $\exp (A^\ast) = (\exp A)^\ast$
- 对 $A, B$ 交换有 $\exp(A + B) = \exp(A) \exp(B)$，特别地 $(\exp A)(\exp -A) = I$
- $\exp$ 将 $\Complex^{n \times n}$ 中 $0$ 的一个邻域映到 $I$ 的一个邻域

如果 $A + A^\ast = 0$，则 $\exp A$ 酉；若 $\exp A$ 酉且 $A$ 在 $0$ 的充分小邻域内，则 $A + A^\ast = 0$；由此，$\mathrm U(n)$ 是 $\Complex^{n \times n}$ 的光滑子流形；$T_I \mathrm U(n)$ 对应于 $n \times n$ 的 $A + A^\ast = 0$ 矩阵构成空间。这也可以构成 $\mathrm U(n)$ 的李代数 $\mathfrak g$.

值得注意的是 $\exp: T_I \mathrm U(n) \to U(n)$ 与在 Riemann 流形中定义的指数映射一致（实际上后者应该得名于这里）。

我们取一个特定的 Riemann 度规，可以证明它关于左、右平移不变：

$$\braket{A, B} = \mathrm{tr}(AB^\ast) = \sum_{i,j} A_{ij} \overline{B_{ij}}$$

对于 $A \in \mathfrak g$，可以用 $T \in \mathrm U(n)$ 对角化成（$a_i$ 实数）：

$$
TAT^{-1} = \begin{pmatrix}
    \mathrm ia_1 \\\\
    ~ & \ddots \\\\
    ~ & ~ & \mathrm ia_n
\end{pmatrix}
$$

对于 $S \in \mathrm U(n)$，可以用 $T \in \mathrm U(n)$ 对角化成（$a_i$ 实数）：

$$
TST^{-1} = \begin{pmatrix}
    e^{\mathrm ia_1} \\\\
    ~ & \ddots \\\\
    ~ & ~ & e^{\mathrm ia_n}
\end{pmatrix}
$$

对比可知 $\exp$ 是满的。

我们可以用同样的方式处理 $\mathrm{SU}(n)$，即 $\mathrm{U}(n)$ 的行列式 $1$ 的子群。由 $\det(\exp A) = e^{\operatorname{tr} A}$ 知其李代数是那些 $A + A^\ast = 0, \operatorname{tr} A = 0$.

为了应用 Morse 理论，我们考察从 $I$ 到 $-I$ 的测地线。考察何种 $A \in T_I \mathrm{U}(n)$ 使得 $\exp A = -I$，一定形如（$k_i$ 奇整数）：

$$
TAT^{-1} = \begin{pmatrix}
    \mathrm ik_1\pi \\\\
    ~ & \ddots \\\\
    ~ & ~ & \mathrm ik_n\pi
\end{pmatrix}
$$

其长度是 $\sqrt{\operatorname{tr} AA^\ast} = \pi \sqrt{k_1^2 + \cdots + k_n^2}$，故最短测地线需要 $k_i = \pm 1$. 此时 $A$ 由 $\set{v | Av = \mathrm i\pi v}$ 与 $\set{v | Av = -\mathrm i\pi v}$ 完全决定。又两者直和是 $\Complex^n$，故可以说 $A$ 被前者决定，即 $I$ 到 $-I$ 最短测地线空间对应到 $\Complex^n$ 全体子空间构成的空间。

但这个空间有不同维数的东西，不容易处理。转而考虑 $\mathrm{SU}(2m)$，迹零限制了我们考察的是全体 $m$ 维子空间。即 $G_m(\Complex^{2m})$. 又，可以计算知 $I$ 到 $-I$ 的非最短测地线指数 $\geq 2m + 2$，从而对 $i \leq 2m$：

$$\pi_i G_m(\Complex^{2m}) \cong \pi_{i+1} \mathrm{SU}(2m)$$

又，使用同伦论方法可以给出 $\pi_i(G_m(\Complex^{2m})) = \pi_{i-1} \mathrm U(m)$；对 $i \leq 2m$ 有 $\pi_{i-1} \mathrm U(m) \cong \pi_{i-1} \mathrm U(m+1) \cong \pi_{i-1} \mathrm U(m+2) \cong \cdots$；对 $j \neq 1$ 有 $\pi_j \mathrm U(m) \cong \pi_j \mathrm{SU}(m)$.

我们得到周期性定理：

$$\pi_{i-1} \mathrm U \cong \pi_{i+1} \mathrm U \\, (i \geq 1)$$

从而得到的结果是：

$$
\mathbf 0 = \pi_0 \mathrm U \cong \pi_2 \mathrm U \cong \pi_4 \mathrm U \cong \cdots \\\\
\Z = \pi_1 \mathrm U \cong \pi_3 \mathrm U \cong \pi_5 \mathrm U \cong \cdots
$$

## 正交群的 Bott 周期性定理
定义 $\R^n$ 上的一个**复结构**是指 $J \in \mathrm O(n)$ 满足 $J^2 = -I$，这样的复结构构成的空间记作 $\Omega_1(n)$.

{% <theorem> %}
$\mathrm O(n)$ 中从 $I$ 到 $-I$ 的全体最短测地线构成空间同胚于 $\Omega_1(n)$.
{% </theorem> %}

取 $A \in T_I \mathrm O(n)$，对于 $n = 2m$ 可以对角化成（$a_i > 0$）：

$$
TAT^{-1} = \begin{pmatrix}
0 & a_1 \\\\
-a_1 & 0 \\\\
~ & ~ & \ddots \\\\
~ & ~ & ~ & 0 & a_m \\\\
~ & ~ & ~ & -a_m & 0
\end{pmatrix}
$$

$$
T(\exp \pi A)T^{-1} = \begin{pmatrix}
\cos \pi a_1 & \sin \pi a_1 \\\\
-\sin \pi a_1 & \cos \pi a_1 \\\\
~ & ~ & \ddots \\\\
~ & ~ & ~ & \cos \pi a_m & \sin \pi a_m \\\\
~ & ~ & ~ & -\sin \pi a_m & \cos \pi a_m
\end{pmatrix}
$$

易见最短测地线的条件是 $a_1 = \cdots = a_m = 1$，此时 $A^2 = -I$；反过来也同理。

计算可得非最短的测地线指数 $\geq 2m - 2$，故对 $i \leq n - 4$ 有：

$$\pi_i\Omega_1(n) \cong \pi_{i+1} \mathrm O(n)$$

我们可以重复此操作，考虑 $J$ 到 $-J$ 的测地线。考察 $n$ 有足够高的 $2$ 的因子，固定复结构 $J_1, \dots, J_{k-1}$ 两两反交换[^clifford-algebra]。定义 $\Omega_k(n)$ 是满足与 $J_1, \dots, J_{k-1}$ 都反交换的 $J$ 构成的空间。

$$\Omega_k(n) \subseteq \Omega_{k-1}(n) \subseteq \cdots \subseteq \Omega_1(n) \subseteq \mathrm O(n)$$

可证：$\Omega_k(n)$ 是 $\mathrm O(n)$ 的光滑子流形，且其每个测地线是 $\mathrm O(n)$ 的测地线；$\Omega_l(n)$ 中从 $J_l$ 到 $-J_l$ 的最短测地线构成空间同胚于 $\Omega_{l+1}(n)$；$J\exp A$ 是复结构当且仅当 $A, J$ 反交换；$J\exp A$ 与 $J_1, \dots, J_{k-1}$ 均反交换当且仅当 $A$ 与它们交换。

我们可以这样将 $\Omega_k(n)$ 嵌入 $\Omega_{k+1}(n + n')$：取 $\R^{n'}$ 上固定的反交换复结构 $J_1', \dots, J_k'$，则 $J \in \Omega_k(n)$ 对应一个 $J \oplus J_k'$.

令 $\Omega_k$ 是 $n \to \infty$ 时 $\Omega_k(n)$ 的正向极限，并赋予正向极限拓扑，记无限正交群 $\mathrm O = \Omega_0$，则 $\Omega_{k+1} \to \Omega\Omega_k$ 是同伦等价。可证有同构：

$$\pi_h \mathrm O \cong \pi_{h-1} \Omega_1 \cong \cdots \cong \pi_1 \Omega_{h-1}$$

接下来通过琐碎的计算，知 $\Omega_4(16r)$ 微分同胚于 $\mathrm{Sp}(2r)$；$\Omega_8(16r)$ 微分同胚于 $\mathrm O(r)$，故无限正交群有一个 $8$ 的周期。

| $i \mod 8$ | $\pi_i \mathrm O$ | $\pi_i \mathrm{Sp}$ |
| :---: | :---: | :---: |
| 0 | $\Z_2$ | $0$ |
| 1 | $\Z_2$ | $0$ |
| 2 | $0$ | $0$ |
| 3 | $\Z$ | $\Z$ |
| 4 | $0$ | $\Z_2$ |
| 5 | $0$ | $\Z_2$ |
| 6 | $0$ | $0$ |
| 7 | $\Z$ | $\Z$ |

---

[^haar-measure]: 在局部紧 Hausdorff 拓扑群上，有 Haar 测度使得：对非空开集 $\mu(U) > 0$；对 $t \in G$ 与可测集 $A$ 有 $\mu(tA) = \mu(A)$；测度是正规测度。
[^clifford-algebra]: 这使得 $\R^n$ 成为了某个 Clifford 代数上的模。之后的结果也可用 Clifford 代数来解释。
