+++
title = "高等代数Ⅱ期末复习笔记"
date = 2026-06-15

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

本文用于准备高等代数Ⅱ的期末考试。这半学期的内容主要是内积空间、内积空间上的线性变换，以及双线性形式。

<!-- more -->

## 内积
### 定义
内积一般是在 $F = \R \text{ or } \Complex$ 上说的。

{% admonition(type="definition", title="内积") %}
$F$-线性空间 $V$ 上的内积是一个函数 $\langle \cdot, \cdot \rangle: V \times V \to F$ 满足：
- $\braket{c\alpha + \beta, \gamma} = c\braket{\alpha, \gamma} + \braket{\beta, \gamma}$
- $\braket{\beta, \alpha} = \overline{\braket{\alpha, \beta}}$
- 对 $\alpha \neq 0$ 有 $\braket{\alpha, \alpha} > 0$
{% end %}

这里前两个条件称为“$1\frac{1}{2}$-线性”，第三个条件称为正定性。

取定了内积的线性空间称为内积空间，有限维实内积空间称为 Euclid 空间，复内积空间也称酉空间。

$F^{n \times 1}$ 上的**标准内积**是 $\braket{\alpha, \beta} = \sum_{i=1}^n x_i \bar{y_i} = \alpha^\top \bar\beta = \beta^\ast \alpha$。$F^{m \times n}$ 上标准内积是 $\braket{A, B} = \mathrm{tr}(B^\ast A)$.

这里 $A^\ast$ 指转置共轭。若 $A^\ast = A$，称 $A$ **Hermite**（$F = \R$ 时称 $A$ **对称**）。在此基础上若还对非零列向量 $\alpha$ 有 $\alpha^\ast A \alpha > 0$，称 $A$ **正定**。易见 $Q \in \mathrm{GL}_n(F)$ 时 $Q^\ast Q$ 正定。

{% admonition(type="theorem", title="定理") %}
给定 $\dim V = n$ 及一组基 $\mathcal{B}$, 对任一内积存在唯一正定矩阵 $A$ 使得：

$$\braket{\alpha, \beta} = [\beta] _\mathcal{B}^\ast A [\alpha] _\mathcal{B}$$
{% end %}

读者易证。

---

对单线性映射 $T: V \to W$ 及 $W$ 上内积，有 $\braket{\alpha, \beta}_V = \braket{T\alpha, T\beta}_W$ 是 $V$ 
上内积。对内积空间 $V$ 定义 $\alpha$ 的**长度**为 $\lVert \alpha \rVert \coloneqq \sqrt{\braket{\alpha, \alpha}}$.

我们有极化恒等式：
- $F = \R$ 时 $\braket{\alpha,\beta} = \frac{1}{4}(\lVert\alpha+\beta\rVert^2 - \lVert\alpha-\beta\rVert^2)$
- $F = \Complex$ 时 $\braket{\alpha,\beta} = \frac{1}{4}\sum_{k=1}^4 i^k \lVert\alpha + i^k \beta\rVert^2$

### 内积空间
容易验证长度满足 Cauchy–Schwarz 不等式 $|\braket{\alpha,\beta}| \leq \lVert\alpha\rVert \lVert\beta\rVert$ 与三角不等式 $\lVert\alpha+\beta\rVert \leq \lVert\alpha\rVert + \lVert\beta\rVert$.

若 $\braket{\alpha,\beta} = 0$ 则称 $\alpha, \beta$ **正交**，记作 $\alpha \perp \beta$. 一族向量若两两正交，称为正交集；若还有 $\lVert\alpha\rVert = 1$，称为标准正交集；若是基称为**标准正交基**。

$F = \R$ 时，对非零 $\alpha,\beta$ 可定义角度 $\angle(\alpha,\beta) \coloneqq \arccos \frac{\braket{\alpha,\beta}}{\lVert\alpha\rVert\lVert\beta\rVert}$.

取标准正交基 $\set{\alpha_1, \dots, \alpha_n}$ 有：

$$\left\langle \sum x_j \alpha_j, \sum y_j \alpha_j \right\rangle = \sum x_j \bar{y_j}$$

$$\beta = \sum \braket{\beta, \alpha_k} \alpha_k$$

{% admonition(type="theorem", title="Gram–Schmidt 正交化") %}
设 $\set{\beta_1, \dots, \beta_n}$ 是 $V$ 的基，存在标准正交基 $\set{\alpha_1, \dots, \alpha_n}$ 且对任意 $k$ 均有：

$$\mathrm{span}\set{\beta_1, \dots, \beta_k} = \mathrm{span}\set{\alpha_1, \dots, \alpha_k}$$
{% end %}

只需要依次取：

$$\alpha_m = \beta_m - \sum_{j=1}^{m-1} \frac{\braket{\beta_m, \alpha_j}}{\lVert\alpha_j\rVert^2} \alpha_j$$

{% admonition(type="theorem", title="Bessel 不等式") %}
对正交集 $S = \set{\alpha_1, \dots, \alpha_n}$ 不含零向量及 $\beta \in V$ 有：

$$\sum_{k=1}^n |\braket{\beta, \alpha_k}|^2 \leq \lVert\beta\rVert^2$$

等号成立当且仅当 $\beta \in \operatorname{span} S$.
{% end %}

对 $S \subseteq V$，定义其**正交补** $S^\perp \coloneqq \set{\alpha \in V | \alpha \perp \beta, \forall \beta \in S}$. 对有限维内积空间与子空间 $W$ 有 $(W^\perp)^\perp = W$ 及 $V = W \oplus W^\perp$.

我们把沿 $W^\perp$ 到 $W$ 上的投影称为 $W$ 上的**正交投影**：

$$P_W \beta \coloneqq \sum_{j=1}^m \braket{\beta, \alpha_j}\alpha_j$$

这是 $\beta$ 在 $W$ 中的最佳逼近。

### 伴随变换
对有限维内积空间，记映射 $\Phi$ 为：

$$
\begin{aligned}
\Phi: V & \simeq V^\ast \cr
	\beta & \mapsto (\alpha \mapsto \braket{\alpha, \beta})
\end{aligned}
$$

{% admonition(type="definition", title="伴随") %}
对任意 $T \in L(V)$，存在唯一的 $T^\ast \in L(V)$ 满足：

$$\braket{T\alpha, \beta} = \braket{\alpha, T^\ast\beta}$$

称为其（关于内积的）**伴随变换**。
{% end %}

在标准正交基 $\mathcal{B}$ 下有 $[T^\ast] _\mathcal{B} = [T] _\mathcal{B}^\ast$.

当 $T^\ast = T$ 时称它是**自伴**的。

### 等距变换
对 $T \in L(V, W)$ 若 $\braket{T\alpha, T\beta} = \braket{\alpha, \beta}$ 则称 $T$ 保内积。使用极化恒等式可以证明保内积等价于保长度，因此也称保内积变换为**等距变换**。

对有限维线性空间 $V, W$, 它们作为内积空间同构当且仅当 $\dim V = \dim W$.

实内积空间 $V$ 作为内积空间的自同构称为**正交变换**，构成正交群 $\mathrm{O}(V)$; 复的则称**酉变换**与酉群 $\mathrm{U}(V)$. 所谓 $\mathrm{SO}, \mathrm{SU}$ 额外加了行列式为 $1$ 的要求。

对 $\dim V < \infty$ 与 $T \in L(V)$ 有 $T$ 等距与 $T^\ast T = I$ 等价。

对应地，在 $\Complex^{n \times n}$ 上我们依据 $A^\top A = I$ 定义正交群 $\mathrm{O}(n)$ 与复正交群 $\mathrm{O}(n, \Complex)$; 依据 $A^\ast A = I$ 定义酉群 $\mathrm{U}(n)$.

{% admonition(type="theorem", title="QR 分解") %}
对 $A \in \mathrm{GL}_n(F)$ 存在唯一分解 $A = QR$, 其中 $Q \in \mathrm{O}(n)$ 或 $\mathrm{U}(n)$, $R$ 是正对角元的上三角阵。
{% end %}

使用 Schmidt 正交化即可。进一步有 Iwasawa 分解 $A = A_k A_a A_n$, 其中 $A_k$ 是前面的 $Q$, $A_a$ 是正对角元的对角阵，$A_n$ 是对角元 $1$ 的上三角阵。

### 正规变换
称 $T$ **正规**，如果 $T$ 与 $T^\ast$ 可交换。

{% admonition(type="theorem", title="正交/酉对角化") %}
对 $F$ 上的有限维内积空间 $V$ 与 $T \in L(V)$ 有：
- $F = \R$ 时存在标准正交基 $\mathcal{B}$ 使 $[T]_\mathcal{B}$ 对角 $\iff$ $T$ 自伴
- $F = \Complex$ 时存在标准正交基 $\mathcal{B}$ 使 $[T]_\mathcal{B}$ 对角 $\iff$ $T$ 正规
{% end %}

左推右易证，考察右推左：

首先，当 $W$ 是 $T$-不变子空间时，$W^\perp$ 是 $T^\ast$-不变子空间。故 $T$ 自伴时 $W^\perp$ 是 $T$-不变子空间。进而 $T$ 自伴时，$f_T$ 在 $\Complex$ 中只有实根，且特征子空间两两正交。从而在 $T$ 自伴时结论可证。

$T$ 正规时，一种方法是取 $T_1 = (T + T^\ast) / 2, T_2 = (T - T^\ast) / 2\mathrm{i}$，它们自伴且可交换。

---

另一种证法先证明：对 $T$ 正规，当 $W$ 是 $T$-不变子空间时，$W$ 是 $T^\ast$-不变子空间。取 $W$ 和 $W^\perp$ 的有序标准正交基，则 $A$ 形如 $\begin{pmatrix} B & C \cr 0 & D \end{pmatrix}$. 计算得到 $BB^\ast + CC^\ast = B^\ast B$. 两边取迹得 $C = 0$，引理得证。故 $T$ 的特征子空间两两正交，从而结论可证。

---

第三种证法先证明 Schur 三角化定理：设 $V$ 有限维复内积空间，$T \in L(V)$，存在有序标准正交基使得 $[T]_{\mathcal{B}}$ 上三角。我们知道存在 $T$-不变全旗，恰当取基使得标准正交即可（或者使用 QR 分解）。而正规的上三角矩阵只能是对角阵。

{% admonition(type="theorem", title="特征值刻画") %}
设 $V$ 有限维复内积空间，$T$ 正规，则：
1. $T$ 自伴 $\iff \sigma(T) \subset \R$
2. $T$ 反自伴 $\iff \sigma(T) \subset \mathrm{i}\R$
3. $T$ 酉 $\iff \sigma(T) \subset \set{e^{\mathrm{i}\theta}}$
{% end %}

取使 $T$ 对角的基来看。

{% admonition(type="question", title="2021 P3") %}
求所有满足如下性质的正整数 $k$：对任意对称非对角矩阵 $A \in \R^{2021 \times 2021}$，矩阵 $A^k + A$ 总不是对角矩阵。
{% end %}

$k$ 为偶时有反例：取 $A$ 每一元均为 $-1/n$，有 $A^2 = -A$.

$k$ 为奇时令 $f(x) = x^k + x$，只需找 $g$ 使得 $g(f(A)) = A$. 考察正交对角化 $A = QDQ^{-1}$，只需对每个对角元 $c$ 有 $g(f(c)) = c$ 即可。

{% admonition(type="question", title="2016 P6") %}
证明任意 $3 \times 3$ 复矩阵酉相似于形如的 $\begin{pmatrix} \ast & 0 & \ast \cr \ast & \ast & 0 \cr \ast & 0 & \ast \end{pmatrix}$ 矩阵。
{% end %}

只需证对线性变换 $T$ 存在标准正交基满足 $\braket{T\alpha_2, \alpha_1} = \braket{T\alpha_2, \alpha_3} = \braket{T\alpha_3, \alpha_2} = 0$. 取特征向量 $\alpha_2$，只需再取出 $\alpha_3$ 即可。

## 内积空间上的线性变换
### 线性空间上的形式
我们考察之前所提及的“$1\frac{1}{2}$-线性”，记满足这样的集合为 $\text{Form}(V)$.

对 $f \in \text{Form}(V)$ 有 $f$ 在 $\mathcal{B}$ 下的矩阵：

$$([f] _\mathcal{B}) _{ij} = f(\alpha_j, \alpha_i)$$

对于换基 $(\alpha_1', \dots, \alpha_n') = (\alpha_1, \dots, \alpha_n) P$ 有：

$$[f] _{\mathcal{B}'} = P^\ast [f] _\mathcal{B} P$$

我们定义 $f \in \text{Form}(V)$ Hermite 是指 $f(\alpha, \beta) = \overline{f(\beta, \alpha)}$, $F = \R$ 时仍可称对称。

在 Hermite 的基础上，我们可以依 $f(\alpha, \alpha)$ 取值定义正定、负定、半正定等。易见 $A$ 正定时 $A$ 可逆，且对 $P \in \mathrm{GL}_n(F)$ 有 $P^\ast AP$ 正定。

{% admonition(type="theorem", title="Cholesky 分解") %}
对 $A \in F^{n\times n}$ 正定，存在唯一正对角元上三角阵 $R$ 使得 $A = R^\ast R$.
{% end %}

考察 $F^{n\times 1}$ 上的内积 $f(X, Y) = Y^\ast AX$ 及标准内积 $f_0(X, Y) = Y^\ast X$. 取内积空间的同构 $L_R: (F^{n\times 1}, f) \to (F^{n\times 1}, f_0)$ 就有 $A = R^\ast R$.

另取 $P$ 也是 $(F^{n\times 1}, f) \to (F^{n\times 1}, f_0)$ 的同构，则有 $PR^{-1}$ 正交/酉。对 $P$ 使用 QR 分解结论即可。

---

我们可以进一步推出正定时 $\det A = |\det (R)|^2 > 0$.

---

我们定义 $A$ 的第 $k$ 个**顺序主子式**：

$$\Delta_k(A) \coloneqq \det(A_{1:k,1:k})$$

{% admonition(type="theorem", title="LU 分解") %}
对任意域 $F$ 及 $A \in \mathrm{GL}_n(F)$ 以下条件等价：
1. 对 $k = 1, \dots, n-1$ 有 $\Delta_k(A) \neq 0$
2. 存在 $L, U \in \mathrm{GL}_n(F)$ 使得 $L$ 下三角，$U$ 对角元 $1$ 且上三角，满足 $A = LU$
{% end %}

只需证 (1) 推 (2). 往证存在严格上三角阵 $N$ 使得 $A(N + I_n)$ 下三角。归纳写出即可。

{% admonition(type="theorem", title="正定矩阵判定方法") %}
对 $A \in F^{n\times n}$ Hermite 有 $A$ 正定当且仅当所有顺序主子式大于 $0$.
{% end %}

只需证右推左。

作 LU 分解然后令 $D = (U^\ast)^{-1}L$，则有 $A = U^\ast DU$. 有 $D$ Hermite 且下三角，故对角。然后分块计算即可。

注：这个判定方法不能拓展到半正定。

### 内积空间上的形式
在有限维内积空间 $V$ 上，使用标准正交基 $\mathcal{B}$ 有线性同构：

$$
\begin{aligned}
\mathrm{Form}(V) & \simeq F^{n\times n} \cr
f & \mapsto [f]_\mathcal{B} 
\end{aligned}
$$

$$
\begin{aligned}
L(V) & \simeq F^{n\times n} \cr
T & \mapsto [T]_\mathcal{B} 
\end{aligned}
$$

由此给出一个 $\mathrm{Form}(V) \to L(V)$ 的同构，记 $f$ 映到 $T_f$.

不依赖于 $\mathcal{B}$ 的定义是：

$$f(\alpha, \beta) = \braket{T_f\alpha, \beta}$$

现在有：$f$ Hermite $\iff$ $[f]_\mathcal{B}$ Hermite $\iff$ $[T _f] _\mathcal{B}$ Hermite $\iff$ $T_f$ 自伴。

我们定义 $T \in L(V)$ 正定，如果它自伴并且 $\braket{T\alpha, \alpha} > 0$.

{% admonition(type="theorem", title="主轴定理") %}
对 $f \in \mathrm{Form}(V)$ Hermite 存在标准正交基使得 $[f]_\mathcal{B}$ 实对角。
{% end %}

因为 $T_f$ 自伴。

对 $F = \R$ 与 $f$ 正定，$f(\alpha, \alpha) = 1$ 决定了一个椭球面。

---

若 $F = \Complex$，用 Schur 三角化定理知存在标准正交基使 $[f]_\mathcal{B}$ 上三角。

### 谱分解
{% admonition(type="theorem", title="谱分解") %}
设 $T$ 自伴（$F = \R$）或正规（$F = \Complex$），$\sigma(T) = \set{c_1, \dots, c_k}$，对任意 $f \in F[x]$ 有：

$$f(T) = \sum_{i=1}^k f(c_i) P_i$$
{% end %}

对 $\alpha = \sum \alpha_i$ 考察 $f(T) \alpha$ 即可。

这给出的推论是：每个 $P_i$ 都是 $T$ 的多项式。

我们可定义：

$$\phi(T) = \sum_{i=1}^k \phi(c_i) P_i$$

保持自伴/正规。

对半正定 $T$ 令 $\phi(x) = \sqrt{x}$ 有 $\sqrt{T}$ 将满足 $(\sqrt{T})^2 = T$. 用 $\braket{T\alpha, \alpha} = \lVert \sqrt{T} \alpha \rVert^2$ 有 $\braket{T\alpha, \alpha} = 0 \implies T\alpha = 0$.

---

称 $\det \sqrt{T^\ast T}$ 为 $T$ 的**奇异值**。我们有：

$$
\lVert \sqrt{T^\ast T} \alpha \rVert^2 =
\braket{\sqrt{T^\ast T} \alpha, \sqrt{T^\ast T} \alpha} =
\braket{T^\ast T \alpha, \alpha} =
\braket{T \alpha, T \alpha} =
\lVert T \alpha \rVert^2
$$

特别地，这给出 $\ker \sqrt{T^\ast T} = \ker T$.

{% admonition(type="theorem", title="极分解") %}
对 $T \in L(V)$ 有：
1. 存在 $T = UN$ 使 $U$ 正交/酉，$N$ 半正定
2. 必有 $N = \sqrt{T^\ast T}$
3. $T$ 可逆当且仅当 $N$ 正定，此时 $U$ 唯一
{% end %}

假设 $T$ 可逆，则 $N$ 可逆，对 $U = TN^{-1}$ 有：

$$\lVert U\alpha \rVert = \lVert T (N^{-1} \alpha) \rVert = \lVert N (N^{-1} \alpha) \rVert = \lVert \alpha \rVert$$

对一般情况，存在 $U_1: \mathrm{Im}(N) \to \mathrm{Im}(T)$. 再任取 $U_2: \mathrm{Im}(N)^\perp \to \mathrm{Im}(T)^\perp$，令 $U = U_1 \oplus U_2$ 即可。

{% admonition(type="question", title="2016 P7") %}
设 $V$ 有限维复内积空间，$S, T \in L(V)$ 正规，证明 $ST$ 正规的充要条件是 $TS$ 正规。
{% end %}

考虑 $S = UN$，有 $U, N$ 可交换。验证 $T, N$ 可交换：令 $R = TS^\ast S - S^\ast ST$ 有 $\mathrm{tr}(R^\ast R) = 0$，从而 $R = 0$.

故有 $U^{-1}STU = NTU = TUN = TS$.

{% admonition(type="theorem", title="奇异值分解") %}
对 $A \in F^{n\times n}$ 存在分解 $A = U_1DU_2$ 使 $D$ 是对角元非负实数的对角阵，$U_1, U_2$ 正交/酉。
{% end %}

考虑极分解 $A = UN$ 再分解 $N = PDP^{-1}$.

{% admonition(type="question", title="2024 P3") %}
证明对 $A, B \in \R^{n \times n}$ 以下两个条件等价：
1. 存在 $X, Y \in \R^{n \times n}$ 使得 $\begin{pmatrix} A & X \cr Y & B \end{pmatrix} \in \mathrm{O}(2n)$
2. 对任意 $\alpha \in \R^{n \times 1}$ 有 $\lVert A\alpha \rVert \leq \lVert \alpha \rVert$，并且存在 $P, Q \in \mathrm{O}(n)$ 使得 $A = PBQ$
{% end %}

对 (1) 推 (2)，对大矩阵用定义知 $A^\top A + Y^\top Y = YY^\top + BB^\top = I$. 故 $\lVert A\alpha \rVert^2 = \lVert \alpha \rVert^2 - \lVert Y\alpha \rVert^2 \leq \lVert A\alpha \rVert^2$.

又，考虑极分解，有：

$$A^\top A = I - Y^\top Y = I - P_Y^{-1}(YY^\top)P_Y = P_Y^{-1}(BB^\top)P_Y = (BP_B^{-1}P_Y)^\top(BP_B^{-1}P_Y)$$

故 $BP_B^{-1}P_Y$ 的极分解形如 $P'\sqrt{A^\top A}$，有 $A = (P_AP'^{-1})B(P_B^{-1}P_Y)$.

对 (2) 推 (1)，考虑奇异值分解 $A = RDS$，取对角阵 $C$ 使得 $D^2 + C^2 = I$，则：

$$
\begin{pmatrix} R & ~ \cr ~ & P^{-1}R \end{pmatrix}
\begin{pmatrix} D & -C \cr C & D \end{pmatrix}
\begin{pmatrix} S & ~ \cr ~ & SQ^{-1} \end{pmatrix} =
\begin{pmatrix} A & \ast \cr \ast & B \end{pmatrix}
$$

### 正规变换进一步性质
对 $\theta \in \R$ 记：

$$
Q_\theta = \begin{pmatrix}
	\cos \theta & -\sin \theta \cr
	\sin \theta & \cos \theta
\end{pmatrix}
$$

{% admonition(type="theorem", title="定理") %}
设 $V$ 有限维实内积空间，$T \in L(V)$ 正规，则存在标准正交基使得：

$$[T]_{\mathcal{B}} = \mathrm{diag}(a_1, \dots, a_l, r_1 Q _{\theta _1}, \dots, r_m Q _{\theta _m})$$
{% end %}

法一：

首先，对 $T$ 正规及 $T$-不变子空间 $W$ 有 $T_W$ 正规。这需要用到之前证的 $W$ 是 $T^\ast$-不变的。我们可以给出正交直和分解 $V = \oplus_{i=1}^k V_i$ 使得每个 $V_i$ 是 $T$-不变的且 $T_{V_i}$ 单纯正规。而后每个 $V_i$ 维数为 $1$ 或 $2$，讨论即可。

---

法二：

首先有 $\ker (T)^\perp = \mathrm{Im}(T^\ast)$ 及 $\mathrm{Im}(T)^\perp = \ker (T^\ast)$. 故知 $\sigma(T^\ast)$ 是 $\sigma(T)$ 的共轭版本，且特征空间维数相等。

特别地，$T$ 正规时对任意 $c$ 有 $\ker (T - cI) = \ker (T^\ast - \bar{c}I)$. 对互素多项式 $f, g$ 有 $\ker f(T) \perp \ker g(T)$.

故 $T$ 的准素循环分解可取为正交直和分解。然后讨论即可。

---

法三：

{% admonition(type="theorem", title="QS 分解") %}
对酉阵 $U$ 存在分解 $U = QS$ 使得 $Q$ 实正交，$S$ 酉对称，且存在复多项式 $f$ 满足 $S = f(U^\top U)$.
{% end %}

取 $f$ 满足 $f(c_i)^2 = c_i$ 有 $S^2 = U^\top U$.

其一个推论是，若两个矩阵酉相似，则它们正交相似。

从而 $A, B$ 正规则以下条件等价：
1. $A, B$ 正交（$F = \R$）相似/酉（$F = \Complex$）相似
2. $A, B$ 在 $F$ 上相似
3. $f_A = f_B$.

我们只需考察：

$$f_T = \prod_{i=1}^l (x - a_i) \prod_{j=1}^m \left(x^2 - (2r_j\cos \theta_j)x + r_j^2\right)$$

---

上述法二有一个推论：若 $A$ 正规，则 $A^\ast$ 为 $A$ 的多项式。我们取 $f$ 使得 $f(c_i) = \bar{c_i}$. 考虑 $A$ 的酉对角化知 $f(A) = A^\ast$ 成立。

## 双线性形式
### 双线性形式
记双线性形式集合 $M_2(V)$. 取一组基，令 $[f]_{\mathcal{B}}$ 的 $(i, j)$ 元 $f(\alpha_i, \alpha_j)$，注意这和之前的定义是转置。

我们称 $A, B \in F^{n\times n}$ **合同**，如果存在 $P$ 使得 $B = P^\top A P$.

我们定义：

$$L_f(\alpha)(\beta) = R_f(\beta)(\alpha) = f(\alpha, \beta)$$

从而有 $\mathrm{rank}(L_f) = \mathrm{rank}(R_f) = \mathrm{rank}([f]_{\mathcal{B}})$，记作 $\mathrm{rank}(f)$.

### 对称、反对称与二次型
记对称、反对称、交错的双线性形式集合为 $S^2(V)$，$A^2(V)$ 与 $\Lambda^2(V)$. 易见 $\Lambda^2(V) \subseteq A^2(V)$ 且在特征不为 $2$ 时取等（进而 $M_2(V) = S_2(V) \oplus A_2(V)$）。

称 $q: V \to F$ 是**二次型**，如果存在某个 $f \in M^2(V)$ 使得 $q(\alpha) = f(\alpha, \alpha)$，记全体为 $Q(V)$.

在特征不为 $2$ 时：
- 取 $\Phi(f)(\alpha) = f(\alpha, \alpha)$，则 $\Phi| _{S^2(V)}$ 是 $S^2(V) \to Q(V)$ 的线性同构，因为 $\ker \Phi = \Lambda^2(V)$，从而可以定义对称阵 $[q] _{\mathcal{B}}$
- 对 $f \in S^2(V)$ 有极化恒等式 $f(\alpha,\beta) = (q(\alpha + \beta) - q(\alpha - \beta)) / 4$

{% admonition(type="theorem", title="定理") %}
- 对 $\operatorname{char} F \neq 2$ 及 $f \in S^2(V)$，存在有序基使 $[f]_{\mathcal{B}}$ 对角
- 对 $f \in \Lambda^2(V)$ 存在有序基使 $[f]_{\mathcal{B}}$ 形如

$$\mathrm{diag}\left(\begin{pmatrix} 0 & 1 \cr -1 & 0 \end{pmatrix} \cdots \begin{pmatrix} 0 & 1 \cr -1 & 0 \end{pmatrix}, 0, \dots, 0 \right)$$
{% end %}

我们定义 $W^\perp = \set{\beta \in V | f(\alpha, \beta) = 0, \forall \alpha \in W}$，则若 $f|_W$ 非退化就有 $V = W \oplus W^\perp$. 讨论即可。

称非退化交错双线性形式为**辛形式**，则在某个基下形如 $\begin{pmatrix} 0 & I_m \cr -I_m & 0 \end{pmatrix}$.

{% admonition(type="theorem", title="合同标准形") %}
对 $f$ 对称：
- 若 $F$ 代数闭且特征非 $2$，则存在有序基使 $[f]_{\mathcal{B}} = \mathrm{diag}(I_r, 0, \dots, 0)$
- 若 $F = \R$，则存在有序基使 $[f] _{\mathcal{B}} = \mathrm{diag}(I _{r_1}, -I _{r_2}, 0, \dots, 0)$
{% end %}

第二种情况的 $r_1, r_2$ 被 $f$ 决定，$r_1$ 为使 $f|_{W \times W}$ 正定的最大子空间维数。两者称为**正惯性指数**、**负惯性指数**，数对 $(r_1, r_2)$ 称为 $f$ 的符号。定理称 Sylvester 惯性定理。

{% admonition(type="question", title="2025 P3 (2)") %}
考虑实线性空间 $\R^{3 \times 3}$ 上的对称双线性函数 $f(A, B) = \mathrm{tr}(AB)$，求 $f$ 的正惯性指数。
{% end %}

取 $\R^{3 \times 3} = S \oplus A$，其中 $S$ 是那些对称矩阵，$A$ 是那些反对称矩阵。有 $S$ 使 $f|_{S \times S}$ 正定，维数最大，故正惯性指数 $6$.

{% admonition(type="question", title="2023 P2") %}
设 $V$ 为 $2023$ 维实线性空间，$f$ 为 $V$ 上的非退化双线性函数，$T \in L(V)$满足：

$$f(T\alpha, T\beta) = f(\alpha, \beta)$$

1. 证明 $1$ 为 $T^2$ 的特征值
2. 进一步假设 $f$ 对称并且 $T$（在 $\R$ 上）可对角化，证明：

$$\mathrm{rank}(T^2 - I) \leq 2\min\set{r_1, r_2}$$
{% end %}

对 (1)，由 $T^\top AT = A$ 知 $ATA^{-1} = (T^\top)^{-1}$. 由 $T^\top \sim T$ 知 $T \sim T^{-1}$. 用特征多项式知成立。

对 (2)，由在 $\R$ 上可对角化，考察 $V = \bigoplus V_\lambda$. 对 $\alpha \in V_\lambda, \beta \in V_\mu$ 有 $(1 - \lambda\mu) f(\alpha, \beta) = 0$. 因此可以将 $V$ 拆成 $V_1, V_{-1}$ 及一族 $V_\lambda \oplus V_{1/\lambda}$ 两两在 $f$ 下正交。讨论即可。

### 自同构群
我们记 $(V, f)$ 的自同构群：

$$\mathrm{Aut}(V, f) = \set{T \in \mathrm{GL}(V) | f(T\alpha, T\beta) = f(\alpha, \beta)}$$

对 $A \in F^{n\times n}$，所求同构于：

$$G_A = \set{M \in \mathrm{GL}_n(F) | M^\top AM = A}$$

对 $f$ 对称非退化，记不定正交群/伪正交群：

$$\mathrm{O}(p, q) = G_{\mathrm{diag}(I_p, -I_q)}$$

特别地，$\mathrm{O}(3, 1)$ 是 Lorentz 群。

对 $f$ 交错非退化，记辛群：

$$\mathrm{Sp} _{2m}(F) = G _{\begin{pmatrix} 0 & I _m \cr -I _m & 0 \end{pmatrix}}$$

{% admonition(type="question", title="2023 P4") %}
求实线性空间 $\R^{4\times 4}$ 的与 $\mathrm{O}(3, 1)$ 不相交的子空间的最大维数。
{% end %}

取 $\set{A | a_{44} = 0}$ 即可。
