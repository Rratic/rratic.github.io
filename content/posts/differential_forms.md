+++
title = "【草稿】微分与微分形式"
date = 2025-11-17

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "微分几何"]
+++

参考阅读
- *Introduction to Smooth Manifolds* (GTM 218)

---

我们回顾光滑流形是在普通的拓扑流形上加上一个光滑结构（包含一组光滑坐标卡）。

$C^\infty (M)$ 是指所有的光滑函数 $f: M\to\mathbb{R}$ 构成的 $\mathbb{R}$-线性空间；光滑函数是指对每个点 $p$ 和包含它的光滑坐标卡 $(U, \varphi)$，有 $f\circ \varphi^{-1}$ 在 $\mathbb{R}^n$ 的多元微积分意义下是光滑的。

对 $f, g \in C^\infty (M)$，我们定义 $fg$ 是指 $m\mapsto f(m)g(m)$.

---

对一个光滑流形（或带边光滑流形）$M$，一个线性映射 $D:C^\infty (M)\to\mathbb{R}$ 是点 $p$ 处的**导子**，如果它满足 Leibniz 律：

$$D(fg)=f(p)D(g)+g(p)D(f)$$

$p$ 处 $C^\infty (M)$ 的全体导子构成 $M$ 在 $p$ 处的**切空间**，记作 $T_pM$.

作为一个简单的例子，我们来证明 $T_p\mathbb{R}^n$ 是由多元微积分意义下的 $\frac{\partial}{\partial x_ i}|_ p$（即 $\frac{\partial f}{\partial x_ i}|_ p$ 是指方向导数 $\nabla_ {e_ i} f|_ p$）张成的空间：
1. 使用 Leibniz 律知 $D(\mathbf{1}) = 0$，故常值函数对应到 $0$.
2. 考虑 $f(x) = f(p) + \sum (x_i-p_i)g_i(x)$，其中 $g_i(x) = \int_0^1 \frac{\partial f}{\partial x_i} (p+t(x-p)) \mathrm{d}t$ 是光滑的。我们将值看作常值函数，$x_i$ 看作投射函数，然后让 $D$ 作用在该式上。

使用此可进一步证明：若 $M$ 是 n 维光滑流形，则任一 $T_pM$ 都是 n 维向量空间，此结论甚至可以推广到带边光滑流形的边界上。

---

对光滑流形（或带边光滑流形）$M$ 与 $N$ 及光滑映射 $F: M\to N$，在 $M$ 上每一点 $p$ 我们定义 $F$ 在 $p$ 处的**微分**为

$$
\begin{aligned}
\mathrm{d}F|_ p \colon T_pM & \to T_{F(p)}N,\\\\
    v & \mapsto (f \mapsto v(f \circ F))
\end{aligned}
$$

如果我们选取一个指定的坐标卡，继续把 $x_i$ 看作投射函数，那么 $\mathrm{d}x_i|_ p$ 就是指对应的 $\mathrm{d}\pi_i|_ p$，其中 $\pi_i(x_1,\cdots,x_n)= x_i$.

我们称 $M$ 的**切丛**是指

$$TM=\bigsqcup_{p\in M}T_pM$$

又，我们定义点 $p$ 处的**余切空间** $T_p^\ast M$ 是指 $T_pM$ 的对偶空间，那么 $M$ 的**余切丛**是指

$$T^\ast M=\bigsqcup_{p\in M}T_p^\ast M$$

因此我们说整个 $\mathrm{d}F$ 实际上是余切丛的一个截面。

---

$M$ 上的一个**曲线**是指连续映射 $\gamma: J\to M$，其中 $J\subseteq\mathbb{R}$ 是区间；曲线在 $t_0$ 处的**速率**是指

$$\gamma '(t_ 0) = \mathrm{d}\gamma \left(\left.\frac{\mathrm{d}}{\mathrm{d}t}\right|_ {t_ 0}\right) \in T_ {\gamma(t_ 0)}M$$

这可给出切矢的另一定义（函数芽空间的导子）：

考虑有序对 $(f, U)$，其中 $U$ 是开集，$f$ 是 $U\to\mathbb{R}$ 的光滑函数，称 $(f, U)$ 与 $(g, V)$ 等价，如果它们在 $p$ 的某个邻域恒等。称这个等价类为 $f$ 在 $p$ 处的**函数芽**，记作 $C_p^\infty (M)$，它构成 $\mathbb{R}$-线性空间并构成结合代数（考虑 $[(f, U)]_ p[(g, V)]_ p = [(fg, U\cap V)]_ p$）。然后就可以以类似方式定义导子。

另有定义（曲线等价类）：

考虑 $J$ 左端点 $0$ 且 $\gamma(0)=p$ 的曲线，令 $\gamma_1\sim\gamma_2$ 如果对任一光滑函数 $f: M\to\mathbb{R}$ 有 $(f\circ\gamma_1)'(0) = (f\circ\gamma_2)'(0)$，那么这个等价类就是切空间。

除此以外，我们还可以考虑坐标卡，看成 n-元组。

---

现在我们来定义微分形式：
