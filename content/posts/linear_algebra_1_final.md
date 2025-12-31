+++
title = "高等代数Ⅰ期末复习笔记"
description = "往年题选做和可能有用的知识扩充 & 考后总结。"
date = 2025-12-29
updated = 2025-12-30

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "线性代数"]
+++

<style>
	img:where(.dark,.dark *) {
        --tw-invert:invert(100%);
        filter: var(--tw-blur,)var(--tw-brightness,)var(--tw-contrast,)var(--tw-grayscale,)var(--tw-hue-rotate,)var(--tw-invert,)var(--tw-saturate,)var(--tw-sepia,)var(--tw-drop-shadow,)
    }
</style>

本文用于准备高等代数实验班 2025 - 2026(I) 的期末考试。

## 往年题选做
{% admonition(type="question", title="2019 P1") %}
考虑实线性空间：

$$V = \\{f \in \mathbb{R}[x] | \deg (f) \leq 3\\}$$

定义 $T_1, T_2 \in L(V)$ 为：

$$T_1(f) = \sum_{k=0}^3 f(k) (x-2019)^k,\ T_2(f) = \sum_{k=0}^3 (D^kf)(x-2020)^k$$

求 $\det T_1$ 和 $\det T_2$.
{% end %}

对 $T_1$，取基 $\\{1, (x-2019), (x-2019)^2, (x-2019)^3\\}$，得到的是一个 Vandermonde 行列式，我们知道可化为 $\prod_{1 \leq i < j \leq n} (a_j - a_i)$.

$T_2$ 同理，得到对角矩阵。

{% admonition(type="question", title="2019 P2") %}
取定 $z \in \mathbb{C}$，有 $f, g \in \mathbb{Q}[x] \setminus \\{0\\}$ 满足 $f(z) = 0, g(z) \neq 0$，证：
1. 存在 $h_1 \in \mathbb{Q}[x]$ 使 $h_1(z) = g(z)^{-1}$
2. 存在 $h_2 \in \mathbb{Q}[x] \setminus \\{0\\}$ 使 $h_2(g(z)) = 0$
{% end %}

(1) 是因为可以对 $f$ 与 $g$ 作辗转相除，得到 $p(z)f(z) - q(z)g(z) = 1$.

(2) 是因为设 $\deg f = n$，则 $\\{h(z) | h \in \mathbb{Q}[x]\\} \subset \mathbb{C}$ 作为 $\mathbb{Q}$-线性空间是至多 $n$ 维的，从而 $1, g(z), \cdots, [g(z)]^n$ 线性相关。

{% admonition(type="question", title="2019 P5") %}
设 $V$ 有限维实线性空间，$W_1, \cdots, W_m \subset V$ 子空间，$\alpha_1, \cdots, \alpha_m \in V$，假设：

$$\dim W_i = \dim V - 1, \quad \bigcap_{i=1}^m W_i = \\{0\\}, \quad \alpha_i \notin W_i$$

设 $T(L(V))$ 满足如下条件：对任意 $i$ 存在 $j$ 使得 $T(\alpha_i + W_i) \subseteq \alpha_j + W_j$，证：$\det T = \pm 1$.
{% end %}

我们来刻画 $W_i$，取 $f_i$ 使 $f_i(\alpha_i) = 1$ 且 $f_i|_{W_i} \equiv 0$. 由条件知 $f_1, \cdots, f_m$ 构成 $V^\ast$ 的基，记它们构成集合 $E$.

考虑转置 $T^t$，我们回忆它是 $T^t(g) = g \circ T$，则对 $T(\alpha_i + \beta) = \alpha_j + \gamma$ 有 $(T^tf_j - f_i)(\alpha_i + \beta) = 0$，故有 $T^tf_j = f_i$.

现在我们知道 $T^t(E) = E$，故存在某个 $(T^t|_E)^k = \mathrm{id}$.

{% admonition(type="question", title="2020 P4") %}
$A \in \mathbb{Z}^{n\times n}$ 满足 $|\det A| = 2$，证存在 $Z \in \mathbb{Z}^{n\times 1}$，使得对任意 $Y \in \mathbb{Z}^{n\times 1}$ 存在 $X \in \mathbb{Z}^{n\times 1}$ 使得 $AX - Y$ 为 $\mathbf{0}$ 或 $Z$.
{% end %}

对 $n$ 维整列向量称 $v \sim w$ 如果 $v - w$ 是 $A$ 乘以某个 $n$ 维整列向量的值。考虑将 $\det A$ 视作体积考虑格点，知只有两个等价类。但是这不容易严格说明。

---

先证对 $A$ 可找到 $|\det P|, |\det Q| = 1$ 的整系数矩阵使得 $PAQ$ 是对角阵。首先，通过初等行变换（注意对应矩阵都有行列式为 $1$）进行辗转相除、交换两行可以得到一个行阶梯阵。可以通过初等列变换得到对角阵。

此时 $AX = Y$ 有解即 $(PA)(QX) = PY$ 有解，易见。

{% admonition(type="question", title="2022 P7") %}
设 $V$ 为实线性空间 $\mathbb{R}^{2022}$，求正整数 $r$ 的最小值，使得存在 $L \in M^r(V)$，当 $T \in L(V)$ 满足 $L(T\alpha_1, \cdots, T\alpha_r) = L(\alpha_1, \cdots, \alpha_r)$ 时总有 $\det T = 1$.
{% end %}

$r = 1$ 时可强行分析。

对于 $r = 2$，设 $V^\ast$ 的基 $f_1, \cdots, f_{2n}$，令 $L = \sum_{k=1}^n f_{2k-1} \wedge f_{2k}$，则条件即：

$$\sum_{k=1}^n (T^t f_{2k-1}) \wedge (T^t f_{2k}) = \sum_{k=1}^n f_{2k-1} \wedge f_{2k}$$

两边对外积取 $n$ 次幂（即 $\underbrace{e\wedge \cdots \wedge e}_n$），有：

$$n! (T^t f_1)\wedge \cdots \wedge (T^t f_{2n}) = n! f_1\wedge \cdots \wedge f_{2n}$$

{% admonition(type="question", title="2022 P8") %}
考虑 $\mathbb{C}[x]$ 的子空间 $V = \mathrm{span}\\{x^{k^2} | k \in \\{0, 1, \cdots, 8\\}\\}$，求正整数 $n$ 的最小值，使得对 $\mathbb{C}$ 的任意 $n$ 元子集 $S$ 总存在 $z_1, \cdots, z_9 \in S$ 与 $f_1, \cdots, f_9 \in V$ 使得 $f_i(z_j) = \delta_{ij}$.
{% end %}

记 $L_z(f) = f(z)$，则对 $|S| = 65$ 有 $\bigcap_{z \in S} \ker(L_z) = \\{0\\}$，从而 $\mathrm{span}\\{L_z | z \in S\\} = V^\ast$，存在 $L_{z_1}, \cdots, L_{z_9}$ 为基。

另一方面，取 $x^{64}-1$ 的根知 $|S| = 64$ 不行。

{% admonition(type="question", title="2023 P1 (2)") %}
设 $f_1, \cdots, f_4$ 是 $\mathbb{R}^4$ 的标准基的对偶基，线性映射 $T \in L(\mathbb{R}^4)$ 满足：

$$(T^t f_1) \wedge (T^t f_2) + (T^t f_3) \wedge (T^t f_4) = f_1 \wedge f_3 + f_2 \wedge f_4$$

求 $\det T$ 的所有可能值。
{% end %}

易见 $T$ 交换 $e_2, e_3$ 是一解。

考虑自己 $\wedge$ 自己，取 $L = f_1 \wedge f_3 \wedge f_2 \wedge f_4 \in \Lambda^4(\mathbb{R}^4)$，直接使用 $\det$ 定义知 $\det T = -1$.

{% admonition(type="question", title="2023 P2") %}
$n = 2024$，对复线性空间 $V = \mathbb{C}^n$，求最大整数 $r$ 使得对任意线性映射 $T: V \to V^\ast$，存在 $V$ 的基 $\alpha_1, \cdots, \alpha_n$ 使得：

$$T(\alpha_i) \in \\{\alpha_{n-r+i}, \cdots, \alpha_n\\}^0, \quad i = 1, \cdots, r$$
{% end %}

将 $T$ 看成双线性形式，则易见 $r = 2024, 2023$ 不行。

对 $r = 2022$ 与给定的 $T$，进行操作：
1. 第 $1$ 次取 $n - 1$ 维子空间 $W_1$ 及 $\alpha_1 \in V\setminus W_1$
2. 第 $k$ 次取 $n - k$ 维子空间 $W_k \subset W_{k-1} \cap \ker T(\alpha_{k-1})$ 及 $\alpha_k \in W_{k-1}\setminus W_k$
3. 第 $n$ 次取 $\alpha_n \in W_{n-1}\setminus \\{0\\}$

---

考虑基 $\alpha_1, \cdots, \alpha_n$ 下的对偶 $V^\ast \to V, f \mapsto (f(\alpha_1), \cdots, f(\alpha_n))$.

现在 $T$ 可被视作 $V \to V$，表示成矩阵 $A$. 换基操作 $e \to Pe$ 会把 $A$ 变为 $(P^{-1})^tA(P^{-1})$.

原题即证任意矩阵可通过合同变换变为主对角线上方略短对角线上方全为 $0$ 的矩阵。

{% admonition(type="question", title="2023 P3") %}
$n = 2024$，$\alpha = (x_1, \cdots, x_n) \in \mathbb{Z}^n$ 为非零向量且 $\gcd (x_1, \cdots, x_n) = 1$. 证存在含 $\alpha$ 的 $n$ 元集合 $S$ 通过整系数线性组合可以构成 $\mathbb{Z}^n$.
{% end %}

一个自然的想法是归纳。

假设 $\gcd(x_{n-1}, x_n) = d, x_{n-1} = du, x_n = dv$，设 $\lambda u + \mu v = 1$，则取前 $n-2$ 个向量为 $(x_1, \cdots, x_{n-2}, d)$ 对应的向量（最后一位添上 $0$），再取 $(0, \cdots, 0, \mu, -\lambda)$ 即可。

---

另一个想法是，结论等价于说构造一个第一行是 $x_1, \cdots, x_n$ 的行列式为 $\pm 1$ 的矩阵。

假设我们现在有一个行列式非零的矩阵行列式有素因子 $p$，则 $F_p$ 下它是线性相关的，设有 $k_1v_1 + \cdots + k_mv_m = 0$，在 $\mathbb{Z}$ 下是 $k_1v_1 + \cdots + k_mv_m = pw$，现在将某个不是 $\alpha$ 的 $v_i$ 换成 $w$，行列式将减小。

---

从 Bézout 定理的形式衍生出的一种想法是：

令 $\Omega = \\{A \in \mathbb{Z}^{n\times n} | \det A = \pm 1\\}$，则 $A^{-1} \in \mathbb{Z}$（我们回顾伴随矩阵 $\operatorname{adj} A$ 定义为 $(\operatorname{adj} A) _{ij} = (-1)^{i+j} M _{ji}$，满足 $A^{-1} = \frac{\operatorname{adj} A}{\det A}$）。

现在证存在 $A \in \Omega$ 使得 $\alpha A = e_1$. 考虑 $\alpha A$ 各分量出现的最小正整数值，设为 $d$. 我们可以让某个 $\beta = \alpha A$ 的第一个分量为 $d$，其余分量在 $0$ 到 $d-1$ 间，由最小性 $\beta = de_1$，现在易知 $d=1$.

{% admonition(type="question", title="2023 P4") %}
矩阵 $A \in \mathbb{R}^{5\times 5}$，$A^2$ 是对角线均为 $1$ 的上三角矩阵，且：

$$a_{11}=a_{12}=a_{21}=a_{55}=0 \quad a_{13}=a_{31}=a_{45}=a_{54}=1$$

求 $a_{22}$ 的所有可能值。
{% end %}

由条件知 $\det A = \pm 1$，考虑：

$$
\begin{pmatrix}
1 & 0 & 0 & 0 & 0 \\\\
0 & 1 & 0 & 0 & 0 \\\\
0 & 0 & 1 & 0 & 0 \\\\
a_{41} & a_{42} & a_{43} & a_{44} & 1 \\\\
a_{51} & a_{52} & a_{53} & 1 & 0
\end{pmatrix} A =
\begin{pmatrix}
0 & 0 & 1 & a_{14} & a_{15} \\\\
0 & a_{22} & a_{23} & a_{24} & a_{25} \\\\
1 & a_{32} & a_{33} & a_{34} & a_{35} \\\\
0 & 0 & 0 & 1 & \ast \\\\
0 & 0 & 0 & 0 & 1
\end{pmatrix}
$$

两边求行列式，即得 $a_{22} = \pm 1$.

事后来看配一个几乎是下三角矩阵的东西是有道理的，但此题还是太难。

{% admonition(type="question", title="2024 P4") %}
$n = 2024$，任意域 $F$，令 $V = \\{h \in F[x] | \deg h < n \\}$，设 $f, g$ 为首一 $n$ 次多项式，$T_1(h)$ 是 $fh$ 除以 $g$ 的余式，$T_2(h)$ 是 $gh$ 除以 $f$ 的余式。证：$\det T_1 = \det T_2$.
{% end %}

考虑 $F$ 的代数闭包 $\bar{F}$，分解 $f(x) = (x-\alpha_1)\cdots(x-\alpha_n), g(x) = (x-\beta_1)\cdots(x-\beta_n)$.

在基 $1, x-\beta_1, (x-\beta_1)(x-\beta_2), \cdots, (x-\beta_1)\cdots(x-\beta_{n-1})$ 中考虑 $T_1$ 对应的矩阵，是一个上三角阵，行列式为 $f(\beta_1)\cdots f(\beta_n) = \prod (\beta_i - \alpha_j)$. $T_2$ 同理，使用 $n$ 是偶数。

## 内容扩充
同学推荐了往年的一个习题课。在知乎上找到讲义如下：

---

[2021.09.14](https://zhuanlan.zhihu.com/p/415730532)

在最后提供了一个使用 mod p 约化证明 $x^5 + 4x + 2$ 的 Galois 群是 $S_5$ 的例子。

---

[2021.09.28](https://zhuanlan.zhihu.com/p/415738112)

在最后提出了子空间的 $(+, \cap)$ 生成问题，给出了 3 个子空间对应的 28 节点 Hasse 图（实际上是“3 个生成元的自由模格 $M_{28}$”）。

4 个子空间可以生成无穷多个子空间（考虑 $\langle e_1 \rangle, \langle e_2 \rangle, \langle e_3 \rangle, \langle e_1+e_2+e_3 \rangle$）。子空间的 $(+, \cap)$ 还可以与射影几何对应（Fano 公理）。

---

[2021.10.12](https://zhuanlan.zhihu.com/p/421177569)

一般的基在泛函分析中称为 Hamel 基。若要讨论无限求和，Banach 空间中有 Schuder 基，Hilbert 空间有 Hilbert 基等。

讲了若干集合论的事情。

---

[2021.10.19](https://zhuanlan.zhihu.com/p/423394766)

对 $V = V_1 \oplus V_2$，分析 $U \subseteq V$ 与它们的“相对位置关系”：

设 $\Omega = \\{U_1 \oplus U_2 | U_1 \subseteq V_1, U_2 \subseteq V_2\\}$. 则其中包含于 $U$ 的最大者 $(U\cap V_1) \oplus (U\cap V_2)$，包含 $U$ 的最小者 $((U+V_2)\cap V_1) \oplus ((U+V_1)\cap V_2) = (U+V_1) \cap (U+V_2)$.

实际上存在：

![一堆同构](/images/algebra/subspace_of_direct_sum_congs.png)

介绍了直和、直积的泛性质及商空间的同构定理。

---

[2021.10.26](https://zhuanlan.zhihu.com/p/430522522)

提供了[习题 2.3:6](@/posts/linear_algebra_1_midterm.md) 的四种证法。

> 希望好好体会两个旗有公共基的四种证法以及对每个证法更精细的分析，这对未来学习有限李型群、李群、代数群等内容都大有益处。而 Grassmannian 的 Scuhbert 胞腔分解，在相交理论中也会扮演重要角色。

---

[2021.11.02](https://zhuanlan.zhihu.com/p/430532869)

矩阵运算技术：
- 分块运算，本质是直和分解
- 多项式 $f(A), g(A)$ 可交换，甚至可以定义指数运算 $e^x = 1 + x + \frac{1}{2!}x^2 + \frac{1}{3!}x^3 + \cdots$

例如，对二阶矩阵 $A = \begin{pmatrix} a & b \\\\ c & d \end{pmatrix}$ 有 $A^2 - (a+d)A + (ad-bc)I_2 = 0$，求 $f(A)$ 时只需先作带余除法。

幂零阵 $A^n = 0$ 可以用于计算 $(I+A)^m$.

对首一多项式 $f(x) = x^n + a_{n-1}x^{n-1} + \cdots + a_0$，友阵满足 $f(A) = 0$.

$$
A = \begin{pmatrix}
0 & & & & -a_0 \\\\
1 & 0 & & & -a_1 \\\\
& \ddots & \ddots & & \cdots \\\\
& & 1 & 0 & -a_{n-2} \\\\
& & & 1 & -a_{n-1}
\end{pmatrix}
$$

华罗庚恒等式给出：

$$(I - BA)^{-1} = I + B(I-AB)^{-1}A$$

有一个打洞技巧：

$$
\begin{pmatrix}
I & \\\\
-CA^{-1} & I
\end{pmatrix}
\begin{pmatrix}
A & B \\\\
C & D
\end{pmatrix}
\begin{pmatrix}
I & -A^{-1}B \\\\
& I
\end{pmatrix} =
\begin{pmatrix}
A & O \\\\
O & D-CA^{-1}B
\end{pmatrix}
$$

对任意 $n\times m$ 阵 $A$ 都存在可逆阵 $P, Q$ 使:

$$
A =
P \begin{pmatrix}
I_r & \\\\
& O
\end{pmatrix} Q
$$

对复矩阵存在更细致的广义逆 Moore-Penrose 广义逆。

简单考察了 $\mathrm{SL}(2, \mathbb{Z})$.

---

[2021.11.09](https://zhuanlan.zhihu.com/p/431031430)

[2021.11.16](https://zhuanlan.zhihu.com/p/433942827)

[2021.11.23](https://zhuanlan.zhihu.com/p/454229103)

介绍了商空间、核、像的泛性质（吐槽了 universal 在日语中译为“宇宙际”）。存在有限双积和零对象的范畴称为加法范畴。

$(\bigoplus V_\alpha)^\ast \cong \bigotimes V_\alpha$，而 $(\bigotimes V_\alpha)^\ast$ 则是更大的东西。

强调很多题可以通过构造双线性形式做。对双线性 $\Phi: V\times W \to F$，自然地诱导出 $L: V \to W^\ast$ 与 $R: W \to V^\ast$，有 $\operatorname{rank} L = \operatorname{rank} R$，记作 $\operatorname{rank} \Phi$.

有 $L$ 单等价于 $R$ 满等价于 $\operatorname{rank} \Phi = \dim V$.

---

[2021.11.30](https://zhuanlan.zhihu.com/p/454233978)

逆序数即是 $S_n$ 作为 Coxeter 群上的长度函数。

有生成函数：

$$\sum_{\sigma \in S_n} x^{l(\sigma)} = \prod_{i=1}^{n-1} (1+x+\cdots+x^i)$$

介绍了 $\mathrm{GL}_n$ 的双陪集分解（Bruhat 分解）：

$$\mathrm{GL} _n = \bigsqcup _{\sigma \in S _n} \mathcal{B} W _\sigma \mathcal{B}$$

其中 $\mathcal{B}$ 是 Borel 子群，由所有可逆上三角阵构成，$W _\sigma$ 指对应的置换阵。

讨论了计数问题。介绍了代数簇视角；$S_n$ 的标准表示。

---

[2021.12.07](https://zhuanlan.zhihu.com/p/454235601)

讨论了大量计算行列式技巧（加边技巧，分块，拆成乘积）。

介绍了 Dedekind 行列式。

---

[2021.12.14](https://zhuanlan.zhihu.com/p/454239621)

特征多项式 $f(x) = \det (xI_n-A)$ 将 $A$ 零化（Cayley-Hamilton 定理）。

介绍了实形式、共轭对合。

反对称矩阵（对角线全为 $0$）的秩一定是偶数。

对反对称矩阵 $A \in F^{2r\times 2r}$，Pfaffian 值定义为：

$$\sum_{\sigma\in\Omega} \operatorname{sgn}(\sigma) a_{\sigma(1)\sigma(2)}a_{\sigma(3)\sigma(4)} \cdots a_{\sigma(2r-1)\sigma(2r)}$$

---

[2021.12.17/18](https://zhuanlan.zhihu.com/p/454241146)

考察了一些具体的 F-（结合）代数同态。

介绍了结式，用于判断 $f, g$ 在 $\bar{F}$ 中是否有公共根。

定义判别式 $\mathrm{Disc}(f) = a^{2n-2} \prod_{i < j} (c_i - c_j)^2$. 有 $\mathrm{Res}(f, f') = (-1)^{n(n-1)/2}a \mathrm{Disc}(f)$.

## 考后总结
还是炸了。填空题没什么可说的，第三题错估了条件的强度做了太久。

{% admonition(type="question", title="2025 P1 (3)") %}
记 $\Lambda_d = \\{(x, y, z) \in \mathbb{Z}^3 \mid x+y+z \equiv 0 \mod d\\}$，求数集 $\\{\det T | T \in L(\mathbb{R}^3), T(\Lambda_3) \subseteq \Lambda_5\\}$.
{% end %}

是 $\frac{5}{3} \mathbb{Z}$. 因为可以 $(x, y, z) \to (x, y, x+y+z)$ 然后 $(x, y, 3k) \to (x, y, 5k)$ 然后 $(x, y, t) \to (x, 4x+y, 4y+t)$.

{% admonition(type="question", title="2025 P4") %}
复矩阵 $A \in \mathbb{C}^{2n\times 2n}$ 满足 $A^2 = -3I_{2n}$，将它表示为分块矩阵（$A_{ij} \in \mathbb{C}^{n\times n}$）：

$$
A = \begin{pmatrix}
A_{11} & A_{12} \\\\
A_{21} & A_{22}
\end{pmatrix}
$$

证明：

$$\det (I_n + A_{11})^3 = \det (I_n - A_{22})^3$$
{% end %}

首先注意到 $(I_{2n}+A)^3 = -8I_{2n}$.

另一方面 $(I_{2n}+A)(I_{2n}-A) = 4I_{2n}$，有：

$$
(I_{2n}+A) \begin{pmatrix}
I_n & -A_{12} \\\\
0 & I_n-A_{22}
\end{pmatrix} =
\begin{pmatrix}
I_n + A_{11} & 0 \\\\
A_{21} & 4I_n
\end{pmatrix}
$$

---

实际上强行展开是可以做的，最终化为 $\det (-8I_n - A_{12}(3A_{21}-A_{22}A_{21})) = \det (-8I_n - (3A_{21}-A_{22}A_{21})A_{12})$.

使用 Sylvester 行列式定理（$\det (I+AB) = \det (I+BA)$）。
