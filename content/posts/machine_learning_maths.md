+++
title = "机器学习理论中的数学基础"
date = 2026-11-25

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学"]
+++

定义矩阵 $A$ 的 $1$-范数是 $\max_j \sum |a_{ij}|$，对于 $n \times 1$ 维，这就是向量的 $1$-范数。矩阵 $A$ 的 $2$-范数是最大奇异值，也就是 $\sqrt{\lambda_{\text{max}} (A^\top A)}$，对于 $n \times 1$ 维，这就是向量的 $2$-范数（即默认的范数）。

定义矩阵 $A$ 的 Frobenius 范数是 $\sqrt{\mathrm{tr}(A^\top A)}$，对于 $n \times 1$ 维，这和 $2$-范数是一致的。

对于 $A, B \in F^{n \times m}$，定义其 **Hadamard 积** $A \odot B$ 是指逐点的乘积。

定义向量化算子 $\mathrm{vec}$ 将 $n \times m$ 的矩阵转成 $nm \times 1$ 的矩阵，则有：

$$\mathrm{vec}(A \odot B) = \mathrm{diag}(\mathrm{vec}(A)) \cdot \mathrm{vec}(B)$$

对于 $A \in F^{n \times m}$ 与 $B \in F^{s \times t}$，定义是 **Kronecker 积** $A \otimes B$ 是 $ns \times mt$ 维的：

$$
\begin{pmatrix}
    a_{11}B & \cdots & a_{1n}B \\\\
    \vdots & \ddots & \vdots \\\\
    a_{n1}B & \cdots & a_{nn}B
\end{pmatrix}
$$

---

经常需要对矩阵乘法组成的式子求梯度。只考虑实矩阵与 $\beta \in \R^{d \times 1}$，$f(\beta) \in \R^{n \times m}$，这里采取分母布局，**约定** $\nabla_\beta f$ 是 $d \times (nm)$ 维的，满足 $\mathrm d(\mathrm{vec}(f)) = (\nabla_\beta A)^\top \mathrm d\beta$，其中 $\mathrm{vec}$ 是指将 $n \times m$ 维摊平成 $nm \times 1$ 维。

通常使用微分来计算较为清晰。考虑 $\lVert\beta\rVert^2$，有：

$$
\begin{split} \mathrm d\lVert\beta\rVert^2
&= \mathrm d\beta^\top \beta \\\\
&= (\mathrm d\beta^\top) \beta + \beta^\top (\mathrm d\beta) \\\\
&= (\mathrm d\beta)^\top \beta + \beta^\top (\mathrm d\beta) \\\\
&= (\beta^\top (\mathrm d\beta))^\top + \beta^\top (\mathrm d\beta) \\\\
&= 2\beta^\top (\mathrm d\beta)
\end{split}
$$

故有 $\nabla_\beta \lVert\beta\rVert^2 = 2\beta$ 成立。如果直接计算，我们总结：对于常值 $X \in \R^{n \times d}$，有 $\nabla_\beta (X\beta) = X^\top$；对于 $X \in \R^{n \times m}$ 有 $(\nabla _\beta (X^\top)) _{k, i, j} = (\nabla _\beta X) _{k, j, i}$；对 $X \in \R^{n \times m}, Y \in \R^{m \times p}$ 有：

$$\nabla_\beta (XY) = (\nabla_\beta X)(Y \otimes I_n) + (\nabla_\beta Y)(I_p \otimes X^\top)$$

对 $X \in \R^{n \times m}$，函数 $f$ 逐点作用，有：

$$\nabla_\beta f(X) = (\nabla_\beta X) \cdot \mathrm{diag}(\mathrm{vec}(f'(X)))$$

---

对于矩阵 $A \in \Complex^{m \times n}$ 及 $G \in \Complex^{n \times m}$，Moore–Penrose 方程是以下四个方程：

$$
\begin{cases}
    AGA = A \\\\
    GAG = G \\\\
    (AG)^\dagger = AG \\\\
    (GA)^\dagger = GA
\end{cases}
$$

若 $G$ 满足全部四个方程，称它是 $A$ 的 **Moore–Penrose 广义逆**，记作 $A^+$；其它情况按满足方程编号记作 $A^{(1)}, A^{(1, 2)}$ 等。

对于 $A$ 行满秩（必有 $m \leq n$），$AA^\dagger$ 有逆，$A^\dagger (AA^\dagger)^{-1}$ 即是一个 Moore–Penrose 广义逆。

{% <theorem> %}
对于行满秩的 $X \in \R^{n \times d}$，$\min_{\beta} \lVert\beta\rVert_2 \text{ s.t. } X\beta = y$ 的解是 $X^+ y$.
{% </theorem> %}

用 Lagrange 乘子法，考虑的是：

$$L(\beta, \lambda) = \frac 1 2 \beta^\top \beta - \lambda^\top (X\beta - y)$$

$\nabla_\beta L = 0$ 给出 $\beta = X^\top \lambda$，代入得 $XX^\top \lambda = y$，从而 $\lambda = (XX^\top)^{-1} y$，即 $\beta = X^+ y$.

---

定义一个函数 $k(x, y)$ 是半正定（PSD）的，如果对有限集 $\set{\alpha_1, \dots, \alpha_n}$，和 $\sum_{i, j = 1}^n \alpha_i \alpha_j k(x_i, x_j)$ 总是非负的，也就是说取 $n$ 行及对应的列，矩阵半正定。这等价于它可以写成 $\braket{\phi(x), \phi(y)}$.

Schur 乘积定理指出，对半正定矩阵 $A, B$，有 $A \odot B$ 半正定。

---

{% <theorem title="下降引理"> %}
对于 $L$-光滑函数 $f$ 有：

$$f(y) \leq f(x) + \braket{\nabla f(x), y - x} + \frac L 2 \lVert y - x \rVert^2$$
{% </theorem> %}

$$
\begin{split} f(y) - f(x)
&= \int_0^1 \braket{\nabla f(x + t(y - x)), y - x} \mathrm dt \\\\
&= \int_0^1 \braket{\nabla f(x), y - x} \mathrm dt + \int_0^1 \braket{\nabla f(x + t(y - x)) - \nabla f(x), y - x} \\\\
&\leq \braket{\nabla f(x), y - x} + \frac L 2 \lVert y - x \rVert^2
\end{split}
$$

分析梯度下降的收敛性时常用 Telescoping Sum，其实就是裂项法。对于随机梯度下降，可以先推导成 $\mathbb E[f(x_{t+1}) | x_t] = C \cdot f(x_t) + \cdots$ 再两边取期望。
