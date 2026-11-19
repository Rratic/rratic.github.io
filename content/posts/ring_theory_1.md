+++
title = "环论（一）：基本定义与 PID、UFD"
date = 2026-11-19

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "代数学"]
+++

代数学Ⅰ课程下半学期的第一部分。从现在看很多定义的动机来自于数论的研究。

<!-- more -->

## 基本定义
定义**环**是配备了加法与乘法的集合，加法使之称为 Abel 群（记加法单位元 $0$）；乘法满足结合律与两边的分配律，且存在 $1$ 使得 $1 \cdot a = a \cdot 1 = a$；约定不需要 $0 \neq 1$，这是为了将来代数几何的考量。环称为交换的（commutative），如果乘法是交换的。环称为除环（division ring / skew field），如果所有非零元素有乘法逆（左右逆相同且唯一）。交换除环称为**域**。

我们要求环同态是保持 $0, 1$ 的。$\ker \phi$ 是 $\phi^{-1}(0)$，于是有第一同构定理。

环中的非零元素 $a$ 称为**零因子**，如果存在非零元素 $b$ 使得 $ab = 0$ 或 $ba = 0$；称为**单位**，如果存在元素 $b$ 使得 $ab = ba = 1$；没有零因子的交换环称为**整环**。

整环具有消去律这一良好性质，对有限整环，所有的 $\phi_a(x) = ax$ 是单射从而是同构，于是 $\phi_a^{-1}(a)$ 给出 $a$ 的逆，该环是域。对整环，可以定义其分式域。

一个子集 $I$ 称为环 $R$ 的左**理想**，如果它是加法子群，且对 $r \in R$ 有 $rI \subseteq I$（可以理解为“吸收”）；右理想同理，理想即双边理想。对理想我们就可以作商 $R/I$，得到的还是环，于是有第三、第四同构定理。

我们可以对理想做加法、取交；令 $IJ$ 是指形如 $ab$（其中 $a \in I, b \in J$）的元素生成的理想。于是有第二同构定理。

{% <note title="商的含义"> %}
与群的商类似，可以把环的商理解为生成元的关系。例如有：

$$k[x, y, z] / (x - y^2, y - z^3) \cong k[z]$$

例如其中 $x^2 y$ 可以写成：

$$
\begin{split} x^2 y
&= (x - y^2 + y^2)^2 y \cr
&= (x - y^2) \cdot \ast + y^5 \cr
&= (x - y^2) \cdot \ast + (y - z^3 + z^3)^5 \cr
&= (x - y^2) \cdot \ast + (y - z^3) \cdot \ast + z^{15}
\end{split}
$$

又如，考察 $\phi: \R[x] \to \Complex$ 是 $\phi(f) = f(\mathrm i)$，于是有 $\R[x] / (x^2 + 1) \cong \Complex$.
{% </note> %}

## 进一步的定义
对于交换环 $R$，称两个理想 $I, J$ 互素（comaximal），如果有 $I + J = R$.

{% <theorem title="中国剩余定理"> %}
设交换环 $R$ 的理想 $I_1, \dots, I_k$，则以下自然的环同态以 $I_1 \cap \cdots \cap I_k$ 为核：

$$
\begin{aligned}
\phi: R &\longrightarrow R/I_1 \times \cdots \times R/I_k \cr
    x &\longmapsto (x \bmod I_1, \dots, x \bmod I_k)
\end{aligned}
$$

如果 $I_j$ 两两互素，则 $\phi$ 是满射，且 $I_1 \cap \cdots \cap I_k = I_1I_2 \cdots I_k$.
{% </theorem> %}

对 $k$ 归纳。$k = 2$ 时设 $1 = a_1 + a_2$，用 $b = a_1b + ba_2$ 知 $I_1 \cap I_2 = I_1I_2$；又 $\phi(a_1) = (0, 1)$，$\phi(a_2) = (1, 0)$ 知满。归纳过程同理。

---

对环 $R$ 与双边理想 $\mathfrak m$，$\mathfrak m$ 称为**极大理想**如果它是极大的双边理想。使用 Zorn 引理知每个真双边理想一定含于某个极大理想。对交换环，理想 $\mathfrak m$ 极大当且仅当 $R/\mathfrak m$ 是域；对环，$R/\mathfrak m$ 是除环可以推出 $\mathfrak m$ 极大。

交换环 $R$ 中理想 $\mathfrak p$ 称为**素理想**，如果 $ab \in \mathfrak p \implies a \in \mathfrak p \text{ or } b \in \mathfrak p$ 恒成立。有 $\mathfrak p$ 是素理想当且仅当 $R/\mathfrak p$ 是整环。

{% <note title="Zaraski 拓扑"> %}
记交换环 $R$ 的素谱 $\mathrm{Spec}(R)$ 是 $R$ 中所有的素理想构成的集合。

如此给予拓扑结构：闭集 $V(I)$ 形如：包含理想 $I$ 的所有素理想 $\mathfrak p$，于是，
- $V(0) = \mathrm{Spec}(R)$
- $V(R) = \emptyset$
- $\bigcap_j V(I_j) = V(\sum_j I_j)$，这里 $\sum$ 指考虑其中的有限和
- $\bigcup_{i=1}^n V(I_i) = V(\bigcap_{i=1}^n I_i)$
{% </note> %}

交换环的同态 $f: R \to S$ 诱导出一个拓扑空间的连续映射，这是因为对素理想 $\mathfrak p \subseteq S$，有 $f^{-1}(\mathfrak p)$ 是素理想。我们考虑：

$$\varphi: R \xrightarrow{f} S \twoheadrightarrow S/\mathfrak p$$

由于 $\ker \varphi = f^{-1}(\mathfrak p)$，使用第一同构定理有单同态 $R/f^{-1}(\mathfrak p) \to S/\mathfrak p$，由于 $S/\mathfrak p$ 是整环，$R/f^{-1}(\mathfrak p)$ 是整环，故 $f^{-1}(\mathfrak p)$ 素。

## PID 与 ED
所有理想都是主理想（一个元素生成的理想）的整环称为**主理想整环**（PID）。例如，$\Z$ 的所有理想形如 $n\Z$，是主理想。易见 PID 的所有非零素理想是极大理想。

对 PID 性质的进一步讨论需要用到交换代数的技术。

---

对整数 $D \neq 0, 1$ 且无平方因子，考虑二次域 $\mathbb Q(\sqrt D) = \set{x + y\sqrt D | x, y \in \mathbb Q}$，其对应的二次整数环（要求极小多项式是整系数的）是：

$$
\mathcal O = \begin{cases}
    \Z[\sqrt D] & D \equiv 2, 3 \bmod 4 \\\\
    \Z[\frac{1 + \sqrt D} 2] & D \equiv 1 \bmod 4
\end{cases}
$$

我们可以定义范数映射是：

$$
\begin{aligned}
    \mathrm N: \mathbb Q(\sqrt D) &\longrightarrow \mathbb Q \\\\
        x + y \sqrt D &\longmapsto x^2 - Dy^2
\end{aligned}
$$

则有 $u \in \mathcal O^\times$ 当且仅当 $\mathrm N(u) = \pm 1$，可见 $\mathcal O^\times$ 对应 Pell 方程的解。

---

一个整环 $R$ 称为 **Euculid 整环**（ED），如果存在一个“范数” $\mathrm{N}: R \to \Z_{\geq 0}$，使得 $\mathrm{N}(0) = 0$，且对任意 $b \neq 0$ 有“带余除法” $a = bq + r$，其中 $r = 0$ 或 $\mathrm{N}(r) < \mathrm{N}(b)$，这里不要求 $q, r$ 唯一。

ED 说的就是，能够在上面进行辗转相除法。ED 的例子不多，主要有：
- 域，可以取 $\mathrm N(a) = 0$
- 域的多项式环，可以取 $\mathrm N(f) = \deg f$
- 整数环 $\Z$，可以取 $\mathrm N(x) = |x|$
- 虚二次域只有：$\Z[\mathrm i], \Z[\sqrt{-2}], \Z[\frac{1 + \sqrt{-3}} 2], \Z[\frac{1 + \sqrt{-7}} 2], \Z[\frac{1 + \sqrt{-11}} 2]$
- 某些实二次域，如 $\Z[\sqrt 2], \Z[\sqrt 3]$
- 某些分圆整数环

是 PID 但不是 ED 的例子有 $\Z[\frac{1 + \sqrt{-19}} 2]$.

{% <theorem> %}
ED 是 UID.
{% </theorem> %}

对理想 $I$，设 $b \in I \setminus \set{0}$ 在其中范数最小，则有 $I = (b)$.

## UFD
对非零元素 $p \in R$，它称为**素元**，如果 $(p)$ 是素理想（或者说 $p \mid ab$ 推出整除两者之一）。非零且不是单位的元素 $r$ 称为**不可约元**，如果 $r = ab$ 推出其一是单位。称两个元素 $a, b$ **相伴**，如果存在单位 $u$ 使 $a = bu$.

整环中素元总是不可约元；对 UFD 相反的方向也成立。

一个整环 $R$ 称为**唯一分解整环**（UFD），如果对每个非零且不是单位的元素 $r$，它可以写成不可约元的积，且这个积在相伴意义下唯一。之后会证明环 $R$ 是 UFD 则 $R[x_1, \dots, x_n]$ 是 UFD.

在 UFD 中可以定义非零元素的最大公约数。分解为单位及两两不相伴的不可约元，定义：

$$\gcd(u p_1^{c_1} \cdots p_r^{c_r}, v p_1^{d_1} \cdots p_r^{d_r}) = p_1^{\min(c_1, d_1)} \cdots p_r^{\min(c_r, d_r)}$$

{% <theorem> %}
PID 是 UFD.
{% </theorem> %}

对可约元，作分解 $r = a_1 b_1 = a_1 a_2 b_2 = \cdots$，得到一个链 $(r) \subseteq (b_1) \subseteq (b_2) \subseteq \cdots$，使用选择公理可以取其并是某个理想 $(b)$，这个 $b$ 一定在某个 $(b_n)$ 中，所以存在（有限的）分解。由于不可约元是素元，可推出分解唯一。

进一步地，对 PID 中非零元素 $a, b$，有 $(a, b) = (\gcd(a, b))$.

{% <example title="在 Gauss 整数的应用"> %}
由于 $\Z[\mathrm i]$ 是 ED，它是 UFD，我们来刻画它的不可约元。

首先，如果 $\mathrm N(\pi)$ 是素数，则它是不可约元，因为若 $\pi = ab$，则其一的范数为 $1$，由之前结论是单位。又，对不可约元 $\pi$，有 $(\pi) \cap \Z$ 是 $\Z$ 的素理想，设为 $(p)$，有 $p = \pi a$，有 $\pi$ 的范数必为 $p$ 或 $p^2$.

详细讨论两种情况，知在相伴意义下不可约元只有 $1 + \mathrm i$；模 4 余 3 的素数；范数为模 4 余 1 的素数的 $x \pm y\mathrm i$，其中 $x, y$ 非零。用到二平方和定理，可以由 $\Z_p^\times \cong \Z_{p-1}$ 证明。
{% </example> %}

## 多项式可约性
{% <theorem> %}
整环 $R$ 是 UFD 当且仅当 $R[x]$ 是 UFD.
{% </theorem> %}

常数 $a$ 在 $R$ 中是单位当且仅当它在 $R[x]$ 中是单位；在 $R$ 中是不可约当且仅当它在 $R[x]$ 中不可约。于是 $R[x]$ 是 UFD 可以推出 $R$ 是 UFD.

考虑 $R$ 的分式域 $F = \mathrm{Frac}(R)$，则 $F[x]$ 是 ED，从而是 UFD.

Gauss 引理说，若 $p(x)$ 在 $F[x]$ 可约，则在 $R[x]$ 可约（对 $p(x) = A(x)B(x)$ 可以找到 $rA(x), r^{-1}B(x) \in R[x]$）。证明如下：

取 $d$ 是 $A(x), B(x)$ 中系数分母的最小公倍数，$d \cot p(x)$ 可以在 $R[x]$ 分解（记作 $a(x)b(x)$）。若 $d$ 是 $R$ 中单位则已完成；否则取 $d$ 的素因子 $p$，有 $R[x]/(q) = (R/qR)[x]$ 是整环，$0 = \overline{d \cdot p(x)} = \overline{a(x)} \cdot \overline{b(x)}$ 推出其一为零，故可以令 $d = qd', a'(x) = q^{-1}(x), b'(x) = b(x)$，然后重复此操作。

又，非常值多项式 $a(x)$ 在 $R[x]$ 不可约当且仅当它是本原多项式[^primitive]，且在 $F[x]$ 中不可约。

对 $R[x]$ 中的多项式分解时，先将系数最大公约数拆出，得到本原多项式，然后使用 Gauss 引理。$F[x]$ 中唯一分解易给出 $R[x]$ 中唯一分解。

---

我们来讨论多项式可约性的判别。域上二次或三次多项式可约当且仅当它有根。$\Z[x]$ 上可约性比较直观；

{% <theorem title="Einsenstein 判别法"> %}
$R$ 有素理想 $\mathfrak p$，多项式 $f(x) = x^n + c_{n-1}x^{n-1} + \cdots + c_0$，若 $c_i$ 均 $\in \mathfrak p$ 且 $c_0 \notin \mathfrak p^2$，则 $f$ 不可约。
{% </theorem> %}

典型的应用是分圆多项式 $\Phi_p(x) = (x^p - 1) / (x - 1)$ 不可约，对 $\Phi_p(x + 1)$ 用 Einsenstein 判别法即可。

对域 $F$，有多项式 $f$ 不可约当且仅当 $F[x]/(f(x))$ 是域。对分解 $f(x) = p_1^{n_1}(x) \cdots p_r^{n_r}(x)$，使用中国剩余定理有：

$$F(x)/(f(x)) \cong F[x]/(p_1^{n_1}(x)) \times \cdots \times F[x]/(p_r^{n_r}(x))$$

---

[^primitive]: 系数最大公约数 $1$；Serre 指出 Gauss 引理的论证也证明了本原多项式的积是本原多项式。
