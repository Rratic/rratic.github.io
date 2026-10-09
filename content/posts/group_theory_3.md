+++
title = "群论（三）：特征子群；幂零群"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "代数学"]
+++

## 特征子群
### 内自同构
回忆自同构群 $\mathrm{Aut}(G)$ 为全体 $G$ 的自同构构成的群，我们定义**内自同构群** $\mathrm{Inn}(G)$ 为全体 $G$ 作用于自身的共轭（$\sigma(a) = g^{-1}ag$，称为内自同构）构成的群。所谓外自同构群则指：

$$\mathrm{Out}(G) \coloneqq \mathrm{Aut}(G) / \mathrm{Inn}(G)$$

{% <theorem title="置换群的自同构"> %}
我们知道共轭作用 $\mathrm{Ad}: S_n \to \mathrm{Aut}(S_n)$ 是单的。对 $n \neq 6$ 它是同构。
{% </theorem> %}

$n \neq 6$ 的例子略。$n = 6$ 时有一个不是内自同构的同构：

$$
\psi((12)) = (12)(34)(56) \\\\
\psi((23)) = (14)(25)(36) \\\\
\psi((34)) = (13)(24)(56) \\\\
\psi((45)) = (12)(36)(45) \\\\
\psi((56)) = (14)(23)(56)
$$

其一种看法是，考虑自然的作用 $\mathrm{PGL}_2(\mathbb F_5) \curvearrowright \mathbb P^1(\mathbb F_5)$，由于 $\mathrm{PGL}_2(\mathbb F_5) \cong S_5$，我们考虑通过左平移作用：

$$S_6 \curvearrowright S_6 / \mathrm{PGL}_2(\mathbb F_5)$$

### 定义与性质
{% <definition title="特征子群"> %}
称 $G$ 的子群 $H$ 为它的**特征子群** $H \text{ char } G$，如果对任意自同构 $\sigma$ 都有 $\sigma(H) = H$.
{% </definition> %}

我们可以仿照它把正规子群的定义重写为：对任意内自同构 $\sigma$ 都有 $\sigma(H) = H$.

{% <example> %}
对群 $G$，典型的特征子群有：其中心 $Z(G)$；其幂子群 $G^n$；其导群 $G'$；其 Frattini 子群（所有极大子群的交）；其 Fitting 子群（所有幂零正规子群的积）；其挠子群（所有有限阶元素构成的群）；其 Ω 子群（所有阶整除 $p^i$ 的元素构成的群）。
{% </example> %}

容易证明，若 $K \text{ char } H$ 且 $H \unlhd G$，则 $K \unlhd G$；进一步，特征子群有传递性。

## 幂零群

## 大定理
{% <theorem title="Hall 定理"> %}
若 $G$ 是 $mn$ 阶可解群，其中 $(n,m)=1$，则它
1. 存在 $m$ 阶子群
2. 任意两个 $m$ 阶子群共轭
3. 若有 $k$ 阶子群满足 $k\mid m$，则存在 $m$ 阶子群包含这个 $k$ 阶子群
{% </theorem> %}
