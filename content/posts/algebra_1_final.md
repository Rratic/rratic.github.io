+++
title = "代数学Ⅰ期末复习笔记"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "代数学"]
+++

## 环论
### 基本定义
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

### 进一步的定义
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
记环 $R$ 的谱 $\mathrm{Spec}(R)$ 是 $R$ 中所有的素理想构成的集合。

如此给予拓扑结构：闭集 $V(I)$ 形如：包含理想 $I$ 的所有素理想 $\mathfrak p$，于是，
- $V(0) = \mathrm{Spec}(R)$
- $V(R) = \emptyset$
- $\bigcap_j V(I_j) = V(\sum_j I_j)$，这里 $\sum$ 指考虑其中的有限和
- $\bigcup_{i=1}^n V(I_i) = V(\bigcap_{i=1}^n I_i)$

由于对环同态有素理想的原像是素理想，环同态诱导出一个拓扑空间的连续映射。
{% </note> %}
