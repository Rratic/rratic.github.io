+++
title = "光滑流形（四）：相交数与 Euler 示性数"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

## 相交数
对于 $n$ 维紧致定向光滑流形 $M$，设 $A, B \subset M$ 闭子流形，且 $\dim A + \dim B = n$ 且 $A \pitchfork B$，则可以在每一点比较 $T_pM$ 与 $T_pA \oplus T_pB$ 的定向是否一致，用 $\varepsilon_p$ 表示。定义**相交数**：

$$I(A, B) = \sum_{p \in A \cap B} \varepsilon_p$$

这里由横截与紧知交点数有限。若 $M$ 不可定向，则只能定义 “$\operatorname{mod} 2$ 相交数” $I_2(A, B) = \\# (A \cap B) \pmod 2$，若不横截则需要微扰然后说明。

所谓 $2k$ 维闭定向流形 $M$ 中的 $k$ 维闭定向子流形 $N$ 的**自相交数**，就是作扰动使得 $N \pitchfork N'$ 后的相交数，记作 $N \cdot N$.

对于同维数的定向紧致连通光滑流形之间的映射 $f: M \to N$，我们定义**映射度** $\deg f$ 为映射与像流形任意点 $q \in N$ 的定向相交数 $I(f, q)$；模 $2$ 版本同理。

{% <example title="球面映射的度"> %}
考虑 $\mathbb S^n \to \mathbb S^n$ 的映射，恒等映射度为 $1$，常值映射度为 $0$，对径映射度为 $(-1)^{n+1}$.
{% </example> %}

复合

## Euler 示性数
我们正式地将 Euler 示性数定义为 [Betti 数](@/posts/homology_1.md)的交错和：

$$\chi(X) = \sum (-1)^\lambda \beta_\lambda$$

{% <theorem title="Euler–Poincaré 公式"> %}
对有限单纯复形，Euler 示性数等于各维数单形个数的交错和。
{% </theorem> %}

记 $Z_i = \ker \partial_i, B_i = \operatorname{im} \partial_{i+1}$，因为 $\dim C_i = \dim Z_i + \dim B_{i-1} = \beta_i + \dim B_i + \dim B_{i-1}$，有：

$$\chi(X) = \sum (-1)^i \dim C_i - \sum (-1)^i \dim B_i - \sum (-1)^i \dim B_{i-1}$$

由于是有限单纯复形，最高维外及 $B_{-1} = 0$，故成立。

{% <theorem title="Poincaré–Hopf 指标定理"> %}
...
{% </theorem> %}

{% <theorem title="Lefschetz 不动点定理"> %}
...
{% </theorem> %}

乘积流形自相交数
