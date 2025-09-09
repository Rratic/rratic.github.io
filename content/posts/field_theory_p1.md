+++
title = "域论（一）：高次方程与 Galois 理论"
description = "代数基本定理，低次方程求根公式，域扩张，Galois 扩张，Galois 群，根式可解性判断与 Abel-Ruffini 定理。"
date = 2025-07-09

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "域论", "Galois 理论"]
+++

前置知识
- 数学分析
- 线性代数
- [群论（二）](/posts/group-theory-p2/)

## 背景
### 代数基本定理
{% admonition(type="abstract", title="代数基本定理") %}
一个复系数 $n$ 次多项式 $P(z)$ 恰有 $n$ 个复根（计入重数）。
{% end %}

只需证明非常值复系数多项式有根。以下给出 Frode Terkelsen 的一个简短初等证明。

[由数学分析](/posts/complex-analysis-p1/#continuous-mapping)知 $|P(z)|$ 有最小值，若最小值非零，不妨设其为 $|P(0)| = 1$

设 $P(z) = 1+az^n+z^{n+1}Q(z), a\neq 0$

取 $\omega$，使 $a\omega^n < 0, |\omega Q(\omega)| < \frac{1}{2}|a|$，则有 $|P(\omega)|\leq 1+\frac{1}{2}|a|\omega^n<1$，矛盾。

### 求根公式
对于一、二次方程来说，求根公式（使用初等运算的复合给出所有根的公式）较为简单。

Gerolamo Cardano 在 1545 年出版的著作 *Ars Magna* 中第一次给出三次方程的完整通解。考虑换元将原方程化为 $x^3+px+q=0$，令 $x=u+v$，此时只需同时满足

$$\left\\\{\begin{matrix}
u^3+v^3=-q \\\\
3uv=-p
\end{matrix}\right.$$

如此转化成了二次方程。

此后，其学生 Lodovico Ferrari 提出了四次方程的解法。换元将原方程化为 $x^4+px^2+qx+r=0$，再化为 $(x^2+p+u)^2 = (p+2u)x^2-qx+(p^2-r+u^2+2pu)$，辅助变量 $u$ 使得右边为完全平方式。$\Delta = 0$ 是一个三次方程。

之后 Niels Abel 严格证明了五次方程不存在通用根式解，Évariste Galois 彻底解决了任意次方程的可解性问题。

## 基本概念
### 基本定义
域 $F$ 是一个配备两种运算的集合，满足：
* $(F, +)$ 是交换群，记单位元为 0
* $(F\setminus \\{0\\}, \times)$ 是交换群，记单位元为 1
* 分配律 $a(b+c)=ab+ac$

子域、同态的定义是平凡的。

对域 $E$，子域 $F\subseteq E$，称前者是后者的扩域，$E/F$ 是**域扩张**。此时，$E$ 是 $F$ 上的线性空间，记次数 $[E:F] = \dim_F E$ 为维数（允许 $\infty$）。通过取一组基 $\alpha_i\beta_j$ 可以说明 $[E:F]=[E:K][K:F]$

例如，$\mathbb{Q}(\sqrt{2}) = \\{a+b\sqrt{2}\mid a,b\in \mathbb{Q}\\}$ 是 $\mathbb{Q}$ 的次数为 2 的扩张，基为 $\{1, \sqrt{2}\}$，而 $\mathbb{R}/\mathbb{Q}$ 为无限扩张。

### 特征
域 $F$ 的**特征**是使得 $n\cdot 1=0$ 成立的最小正整数 $n$，如果不存在，则称特征是 0

易说明，若特征是 0，一定存在子域同构于 $\mathbb{Q}$，否则特征一定是素数，同构于子域 $F_p = Z/pZ$，并且由 $(x+y)^p = x^p+y^p$ 有 Frobenius 自同态 $x\mapsto x^p$

{% admonition(type="example", title="p-进数域") %}
对 $q=p^n \frac{a}{b}$，其中 $a$，$b$ 与 $p$ 互素，定义 p-进绝对值 $|q|_p = p^{-n}$

$\mathbb{Q}$ 由 p-进绝对值完备化为 $\mathbb{Q}_p$

可以看成全体有限的 p 进制小数（每一位为 $\mathbb{F}_p$）构成的域。
{% end %}

其特征为 p

{% admonition(type="example", title="形式 Laurent 级数域") %}
对域 $F$，$F((x))$ 是全体 $\sum_{n=k}^\infty a_nx^n, k\in\mathbb{Z}, a_n\in F$ 的域。
{% end %}

其特征与 $F$ 一致。

### 多项式
对 $\alpha\in E$ 若存在非零多项式 $f\in F[x]$ 使 $f(\alpha) = 0$，则称其为**代数元**，否则称为**超越元**。

一个代数元的**极小多项式** $m_{\alpha, F}(x)$ 是一个首一、不可约、满足 $m(\alpha) = 0$ 的多项式。

对 $S\subset E$，由 $S$ 生成的域 $F$ 上的扩张 $F(S)$ 是同时包含 $F$ 和 $S$ 的最小的 $E$ 的子域。若 $S$ 为单元集，且 $E=F(S)$，称其是域 $F$ 的**单扩张**。考虑同态 $\varphi: F[x] \to F(\alpha), f \mapsto f(\alpha)$ 知 $[F(\alpha):F]=\deg m_{\alpha, F}$

### Galois 扩张
如果域扩张 $E/F$ 满足 $E$ 是 $F[x]$ 某个元素的分裂域（即 $f$ 可以写成 $E[x]$ 中一次式的乘积），则称其为**正规扩张**。

如果一个不可约多项式没有重根，则称它**可分**。

给定域扩张 $E/F$，对 $\alpha\in E$，要么是 $F$ 上的超越元，要么是代数元且极小多项式可分，则称其为**可分元**。若所有元素都是可分元，则称它为**可分扩张**。

**Galois 扩张**是指正规且可分的扩张。

{% admonition(type="abstract", title="0 特征域的扩张可分") %}
对 0 特征域 $F$，每个扩张 $E/F$ 都可分。
{% end %}

对不可约的 $f$，由于特征为 0，$f'$ 非零，从而 $\gcd(f, f')=1$，$f$ 没有重根。

## Galois 理论
### Galois 群
Galois 理论给出了域的扩张与其自同构群的联系。

对域扩张 $E/F$，对自同构 $\rm{Aut}(E)$ 中的元素 $\sigma$，称其**保持** $F$，如果 $\sigma(a)=a, \forall a\in F$

以下讨论 $n$ 次首一多项式 $f\in F[x]$ 在 $E/F$ 中分裂，所有根 $\Omega = \\{z_i\\}$ 且 $E=F(\Omega)$

{% admonition(type="abstract", title="命题") %}
对 $\sigma\in \rm{Aut}(E)$ 保持 $F$，它在 $\Omega$ 上是一个置换。
{% end %}

因为 $f(\sigma(z_i)) = \sigma(f(z_i)) = 0, \sigma(z_i)\in\Omega$，且 $\sigma$ 是单射。

---

记 Galois 群 $\rm{Gal}(E/F)$ 为 $\rm{Aut}(E)$ 中所有保持 $F$ 的元素构成的群。

我们可以说明单位元对应的是 $E$ 上的单位映射：

对 $n$ 归纳，$n=1$ 时 $E$ 中元素均形如 $\frac{f(z_1)}{g(z_1)}$ 知成立。又 $F(z_1, \cdots z_n) = (F(z_1))(z_2, \cdots z_n)$

进一步地，$\rm{Gal}(E/F)$ 同构于 $S_n$ 的一个子群。考察

$$
\begin{aligned}
\varphi \colon & \rm{Gal}(E/F) \to S(\Omega),\\\\
        &\sigma \mapsto \sigma |_\Omega
\end{aligned}
$$

有 $\ker\varphi = \\{e\\}$，从而 $\rm{Gal}(E/F)\cong \rm{Im}(\varphi)\leq S(\Omega)$

### 根式可解性
我们用代数的语言定义根式可解性。

对域 $F$ 和非常数多项式 $f\in F[x]$，对应正规扩张 $E$，如果存在一列扩张 $F=K_0\subset K_1\subset \cdots K_t$ 满足 $K_{i+1} = K_i(u)$，其中 $u^k\in K_i, k\in\mathbb{Z}^+$，且 $E\subseteq K_t$，则称 $f$ 根式可解。

{% admonition(type="abstract", title="命题") %}
对 $F\subset K\subset E$，其中 $K/F, E/F$ 是正规扩张，则对任意 $\sigma\in \rm{Gal}(E/F)$，有
- $\sigma\ K = K$
- $\rm{Gal}(E/K)\triangleleft \rm{Gal}(E/F)$
- $\rm{Gal}(E/K)/\rm{Gal}(E/F)\cong \rm{Gal}(F/K)$
{% end %}

第一个结论由定义易得。

$$
\begin{aligned}
\varphi \colon & \rm{Gal}(E/F) \to \rm{Gal}(K/F),\\\\
        &\sigma \mapsto \sigma |_K
\end{aligned}
$$

给出结论二、三。

{% admonition(type="abstract", title="根式扩张导出 Galois 群的次正规群列") %}
对一列扩张 $F=K_0\subset K_1\subset \cdots K_t$ 满足 $K_{i+1} = K_i(u)$，其中 $u^{p_i}\in K_i, p\in\mathbb{P}$，且 $K_t/F$ 正规，$F$ 包含所有 $p_i$ 阶单位根。

则有子群列 $\\{e\\} = G_t \subset \cdots G_1\subset G_0 = \rm{Gal}(K_t/F)$，其中 $G_{i+1}\triangleleft G_i$，$G_i/G_{i+1}$ 为 $\\{e\\}$ 或 $p_{i+1}$ 阶循环群。
{% end %}

令 $G_i = \rm{Gal}(K_t/F)$

---

再由[群论（二）](/posts/group-theory-p2/)知上述为次正规群列。

我们给出

{% admonition(type="abstract", title="n 次方程可解性问题") %}
域 $F$ 和非常数多项式 $f\in F[x]$，正规扩张 $E$，若 $f$ 根式可解，则 $\rm{Gal}(E/F)$ 为可解群。
{% end %}

特别地，

{% admonition(type="abstract", title="Abel-Ruffini 定理") %}
五次方程不存在通用根式解。
{% end %}

此时对应的 $\rm{Gal}(E/F)\cong S_n, n\geq 5$

我们知道 $S_n, n\geq 5$ 不可解，因为其子群 $A_n$ 为单群（通过说明非平凡正规子群一定包含全体三轮换），不可解。

### TO READ
- [为什么要消去二次项才能解一元三次方程？](https://www.zhihu.com/question/450445294/)
- [计算多项式的Galois群](https://zhuanlan.zhihu.com/p/622822909)
- [mod p 约化技巧](https://www.zhihu.com/question/458961859/)
