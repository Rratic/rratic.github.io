+++
title = "同调论（二）：计算方法与应用"
draft = true

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

## 度
对 $f: S^n \to S^n$，其诱导的 $f_\ast: H_n(S^n) \to H_n(S^n)$ 是 $\Z$ 到自身的同态，必形如 $f_\ast(\alpha) = d\alpha$，记 $d = \deg f$ 为 $f$ 的**度**。

我们有 $f \simeq g$ 当且仅当 $\deg f = \deg g$，其中右推左是 Hopf 给出的同伦论结论。

{% <example title="$S^n$ 有非零连续向量场当且仅当 n 奇"> %}
考虑嵌入 $\R^{n+1}$ 的看法，假设有非零连续向量场 $V$，不妨让 $|V(x)| = 1$，令：

$$f_t(x) = (\cos t)x + (\sin t)V(x)$$

让 $t$ 从 $0$ 走到 $\pi$，它给出从恒等映射到对径点映射的同伦，故它们度相同，$(-1)^{n+1} = 1$，$n$ 偶。

$n$ 为奇时只需取：

$$V(x_1, \dots, x_{2k}) = (-x_2, x_1, \dots, -x_{2k}, x_{2k-1})$$
{% </example> %}

{% <theorem> %}
$n$ 为偶时 $\Z_2$ 是唯一能自由作用（非平凡元没有固定点）在 $S^n$ 上的非平凡群。
{% </theorem> %}
