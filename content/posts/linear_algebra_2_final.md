+++
title = "【草稿】高等代数Ⅱ期末复习笔记"
date = 2026-06-01

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "线性代数"]
+++

本文用于准备高等代数Ⅱ的期末考试。这半学期的内容主要是内积空间、内积空间上的线性变换，以及？？？。

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

$T$ 正规时设 $T_1 = (T + T^\ast) / 2, T_2 = (T - T^\ast) / 2\mathrm{i}$, 则它们自伴且可交换。

{% admonition(type="theorem", title="定理") %}
对 $F$ 上的有限维内积空间 $V$ 与 $T \in L(V)$ 有：
- $F = \R$ 时存在标准正交基 $\mathcal{B}$ 使 $[T]_\mathcal{B}$ 对角 $\iff$ $T$ 自伴
- $F = \Complex$ 时存在标准正交基 $\mathcal{B}$ 使 $[T]_\mathcal{B}$ 对角 $\iff$ $T$ 正规
{% end %}

也称为正交/酉对角化。

{{ todo() }}

## 内积空间上的线性变换
### 线性空间上的形式
我们考察之前所提及的“$1\frac{1}{2}$-线性”，记满足这样的集合为 $\text{Form}(V)$.

对 $f \in \text{Form}(V)$ 有 $f$ 在 $\mathcal{B}$ 下的矩阵：

$$([f] _\mathcal{B}) _{ij} = f(\alpha_k, \alpha_j)$$

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

现在有：$f$ Hermite $\Leftrightarrow$ $[f]_\mathcal{B}$ Hermite $\Leftrightarrow$ $[T _f] _\mathcal{B}$ Hermite $\Leftrightarrow$ $T_f$ 自伴。

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

保持自伴/正规。对半正定 $T$ 令 $\phi(x) = \sqrt{x}$ 有 $\sqrt{T}$ 将满足 $(\sqrt{T})^2 = T$.

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

{% admonition(type="theorem", title="奇异值分解") %}
对 $A \in F^{n\times n}$ 存在分解 $A = U_1DU_2$ 使 $D$ 是对角元非负实数的对角阵，$U_1, U_2$ 正交/酉。
{% end %}

考虑极分解 $A = UN$ 再分解 $N = PDP^{-1}$.

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
