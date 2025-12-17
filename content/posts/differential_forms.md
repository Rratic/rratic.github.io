+++
title = "光滑流形上的微分与微分形式"
description = "关于 ∂/∂x, df, dx, d, df/dx 等记号本质上/在一般的流形上是什么（并没有到层论视角）。"
date = 2025-11-19
updated = 2025-11-24

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

作为一个简单的例子，我们来证明 $T_p\mathbb{R}^n$ 是由多元微积分意义下的 $\frac{\partial}{\partial x_ i}|_ p$（即把  $\frac{\partial}{\partial x_ i}$ 看作 $e_i$，$\frac{\partial f}{\partial x_ i}|_ p$ 是指方向导数 $\nabla_ {\frac{\partial}{\partial x_ i}} f|_ p$）张成的空间：
1. 使用 Leibniz 律知 $D(\mathbf{1}) = 0$，故常值函数对应到 $0$.
2. 考虑 $f(x) = f(p) + \sum (x_i-p_i)g_i(x)$，其中 $g_i(x) = \int_0^1 \frac{\partial f}{\partial x_i} (p+t(x-p)) \mathrm{d}t$ 是光滑的。我们将值看作常值函数，$x_i$ 看作投射函数，然后让 $D$ 作用在该式上。

使用此可进一步证明：若 $M$ 是 n 维光滑流形，则任一 $T_pM$ 都是 n 维向量空间，此结论甚至可以推广到带边光滑流形的边界上。

因此，我们也会说把这个向量空间里的一个向量*作用*到一个函数上，如 $\frac{\partial f}{\partial x_ i} = \frac{\partial}{\partial x_ i}(f)$.

---

对光滑流形（或带边光滑流形）$M$ 与 $N$ 及光滑映射 $F: M\to N$，在 $M$ 上每一点 $p$ 我们定义 $F$ 在 $p$ 处的**微分**为

$$
\begin{aligned}
\mathrm{d}F|_ p \colon T_pM & \to T_{F(p)}N,\\\\
    v & \mapsto (f \mapsto v(f \circ F))
\end{aligned}
$$

如果我们选取一个指定的坐标卡，继续把 $x_i$ 看作投射函数，那么 $\mathrm{d}x_i|_ p$ 就是指对应的 $\mathrm{d}\pi_i|_ p$，其中 $\pi_i(x_1,\cdots,x_n)= x_i$.

作为一个例子，对 $\mathrm{d}f|_ p: T_p\mathbb{R}^n \to T_p\mathbb{R}^m$，考虑 $T_p\mathbb{R}^k \cong \mathbb{R}^k$ 可以将它表示成 **Jacobi 矩阵**：

$$\mathrm{Jac}(f) = \left(\frac{\partial f_j}{\partial x_i}\right)_{1\leq i\leq n, 1\leq j\leq m}$$

---

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

为了定义微分形式，让我们先回顾张量的定义（注：在讨论张量时，会把一些下标写成上标来便于阅读，由于不会出现幂次，一般不会产生歧义）：

首先，定义 $V_1\otimes\cdots\otimes V_k = \mathcal{F}(V_1\times\cdots\times V_k)/\mathcal{R}$，其中 $\mathcal{F}(S)$ 是指集合 $S$ 上的自由向量空间，$\mathcal{R}$ 是由形如 $(v_1\cdots av_i\cdots v_k)-a(v_1\cdots v_i\cdots v_k)$ 与 $(v_1\cdots v_i+v_i'\cdots v_k) - (v_1\cdots v_i\cdots v_k) - (v_1\cdots v_i'\cdots v_k)$ 的元素生成的子空间。

实际上我们有 $V_1^\ast\otimes\cdots\otimes V_k^\ast \cong L(V_1,\cdots,V_k;\mathbb{R})$.

我们称 $V$ 上的**协变** k-张量是指 $\underbrace{V^\ast\otimes\cdots\otimes V^\ast}_k$，其全体记作 $T^k(V^\ast)$；称 $V$ 上的**反变** k-张量是指 $\underbrace{V\otimes\cdots\otimes V}_k$，其全体记作 $T^k(V)$；称 $(k, l)$-型混合张量是指 $\underbrace{V\otimes\cdots\otimes V}_k \otimes \underbrace{V^\ast\otimes\cdots\otimes V^\ast}_l$，其全体记作 $T^{(k, l)}(V)$.

一个协变 k-张量 $\alpha$ 被称为是**对称**的，如果 $\alpha(\cdots v_i\cdots v_j \cdots) = \alpha(\cdots v_j\cdots v_i \cdots)$，其全体称为 $\Sigma^k(V^\ast)$；我们有一个投射 $\mathrm{Sym}: T^k(V^\ast) \to \Sigma^k(V^\ast)$，它是：

$$(\operatorname{Sym} \alpha)(v_1,\cdots,v_k) = \frac{1}{k!}\sum_{\sigma\in S_k} \alpha(v_{\sigma(1)},\cdots,v_{\sigma(k)})$$

对 $\alpha, \beta \in \Sigma^k(V^\ast)$，可以定义其对称积 $\alpha\beta = \mathrm{Sym}(\alpha\otimes\beta)$.

一个协变 k-张量 $\alpha$ 被称为是**交错/反对称**的，如果 $\alpha(\cdots v_i\cdots v_j \cdots) = -\alpha(\cdots v_j\cdots v_i \cdots)$，其全体称为 $\Lambda^k(V^\ast)$；我们有一个投射 $\mathrm{Alt}: T^k(V^\ast) \to \Lambda^k(V^\ast)$，它是：

$$(\operatorname{Alt} \alpha)(v_1,\cdots,v_k) = \frac{1}{k!}\sum_{\sigma\in S_k} (\operatorname{sgn} \sigma) \alpha(v_{\sigma(1)},\cdots,v_{\sigma(k)})$$

实际上我们可以找到 $\Lambda^k(V^\ast)$ 的一组基。对 $I=(i_1,\cdots,i_k)$，我们定义 $\varepsilon^I$ 是指：

$$
\varepsilon^I(v_1,\cdots,v_k) = \det \begin{pmatrix}
v_1^{i_1} & \cdots & v_k^{i_1} \\\\
\vdots & \ddots & \vdots \\\\
v_1^{i_k} & \cdots & v_k^{i_k} 
\end{pmatrix}
$$

那么对 $\dim V = n, n\geq k$，所有 $I$ 为递增指标的 $\binom{n}{k}$ 个 $\varepsilon^I$ 构成一组基。

我们定义**楔积/外积**是：

$$\omega\wedge\eta = \frac{(k+l)!}{k!l!}\mathrm{Alt}(\omega\otimes\eta)$$

它具有有趣的性质：
* 双线性性
* 结合律 $\omega\wedge(\eta\wedge\xi) = (\omega\wedge\eta)\wedge\xi$
* 反对称性 $\omega\wedge\eta=(-1)^{kl}\eta\wedge\omega$
* $\varepsilon^I\wedge\varepsilon^J = \varepsilon^{IJ}$，其中 $IJ$ 是拼接
* $\omega^1\wedge\cdots\wedge\omega^k(v_1,\cdots,v_k) = \det (\omega^j(v_i))$

作为一个例子，在 $\mathbb{R}^2$ 上有：

$$(\mathrm{d}x\wedge \mathrm{d}y)(v, w) = (\mathrm{d}x\otimes \mathrm{d}y)(v, w) - (\mathrm{d}x\otimes \mathrm{d}y)(w, v) = \mathrm{d}x(v)\mathrm{d}y(w) - \mathrm{d}x(w)\mathrm{d}y(v) = v_1w_2 - v_2w_1$$

---

回到流形上来，我们定义

$$T^{(k, l)}TM = \bigsqcup_{p\in M}T^{(k, l)}(T_pM)$$

就有对 $\Gamma(T^{(k, l)}TM)$（其中 $\Gamma$ 指所有光滑截面构成的空间）的元素 $A$，它将形如

$$A = A_{j_1\cdots j_l}^{i_1\cdots i_k} \frac{\partial}{\partial x^{i_1}}\otimes\cdots\otimes\frac{\partial}{\partial x^{i_k}}\otimes\mathrm{d}x^{j_1}\otimes\cdots\otimes\mathrm{d}x^{j_l}$$

我们称**微分 k-形式**是指：

$$\Lambda^kT^\ast M = \bigsqcup_{p\in M}\Lambda^k(T_p^\ast M)$$

的截面，记 $\Omega^k(M) = \Gamma (\Lambda^kT^\ast M)$.

作为一个有关 1-形式与 2-形式的例子，我们考虑 $\mathrm{d}x\wedge \mathrm{d}y = \mathrm{d}(r\cos\theta)\wedge\mathrm{d}(r\sin\theta) = (\cos\theta\mathrm{d}r-r\sin\theta\mathrm{d}\theta)\wedge (\sin\theta\mathrm{d}r+r\cos\theta\mathrm{d}\theta) = r\mathrm{d}r\wedge\mathrm{d}\theta$.

一个 k-形式 $\omega$ 可以被分解为 $\sum_I \omega_I \mathrm{d}x^I$，其中 $\mathrm{d}x^I$ 是 $\mathrm{d}x^{i_1}\wedge\cdots\wedge\mathrm{d}x^{i_1}$.

---

我们有时会遇到这样的记号：

$$\frac{\mathrm{d}^2 f}{\mathrm{d} x^2} = \frac{\mathrm{d}\left(\frac{\mathrm{d} f}{\mathrm{d} x}\right)}{\mathrm{d} x}$$

其中的 $\mathrm{d}$ 可以被看成**外微分** $\mathrm{d}: \Omega^k(M)\to\Omega^{k+1}(M)$，定义为在每个光滑坐标卡上是：

$$\mathrm{d}\left(\sum_J \omega_J \mathrm{d}x^J\right) = \sum_J \omega_J\wedge\mathrm{d}x^J$$

它满足：
1. 在 $\mathbb{R}$ 上线性
2. 对 $\omega\in\Omega^k(M)$ 及 $\eta\in\Omega^l(M)$，有 $\mathrm{d}(\omega\wedge\eta) = \mathrm{d}\omega\wedge\eta+(-1)^k\omega\wedge\mathrm{d}\eta$
3. $\mathrm{d}\circ\mathrm{d}=0$
4. 对 $f\in\Omega^0(M)=C^\infty(M)$，有 $\mathrm{d}f$ 是 $f$ 的微分

我们称一个 $\omega \in \Omega^k(M)$ 是**闭**的，如果 $\mathrm{d}\omega = 0$，称它是 exact [^translation-exact] 的，如果存在 $\eta \in \Omega^{(k-1)}(M)$ 使 $\omega = \mathrm{d}\eta$，有 exact 的形式一定是闭的。

---

另外，$\frac{\mathrm{d} f}{\mathrm{d} x}$ 被看成是一个 0-形式。

在一维情形下，可以被定义为切映射的商。一般不会考虑多维情形的 $\frac{\mathrm{d} f}{\mathrm{d} x_i}$，有时它会定义为 Fréchet 导数，结果是 $m\times n$ 的矩阵。

也可以把 $\frac{\mathrm{d} f}{\mathrm{d} x}$ 看成缩并 $\iota_X(\mathrm{d}f)$ 或李导数 $\mathcal{L}_X f$.

---

[^translation-exact]: 可译为“恰当”，此处不是指正合。
