+++
title = "概率论期中复习笔记"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识", "课程"]
tags = ["数学", "概率论"]
+++

基于课程安排，参考 Durrett, *Probability Theory and Examples* 加入一些测度论风格。

古典概率模型和几何概率模型略去。

回忆概率空间是三元组 $(\Omega, \mathcal F, P)$，其中 $\mathcal F$ 是 $\sigma$-代数，$P: \mathcal F \to [0, 1]$.

$(\Omega, \mathcal F)$ 上的测度是非负、可数可加的 $\mu: \mathcal F \to \R$，在 $\mu(\Omega) = 1$ 时称为概率测度。

我们用符号 $A_i \uparrow A$ 表示 $A_1 \sub A_2 \sub \dots$ 且 $\bigcup A_i = A$，符号 $\downarrow$ 反之。易见 $A_i \uparrow A \implies \mu(A_i) \uparrow \mu(A)$.

在 $\R^d$ 上用 $\mathcal R^d$ 表示 Borel 集（包含所有开集的最小 $\sigma$-field）。

{% admonition(type="theorem", title="一维的测度") %}
每个 Stieltjes 测度函数（不降、右连续）$F$ 对应唯一的一个 $(R, \mathcal R)$ 上的测度 $\mu((a, b]) = F(b) - F(a)$.
{% end %}

证明略。在 $F(x) = x$ 时对应的就是 Lebesgue 测度。

{% admonition(type="theorem", title="d 维的测度") %}
$F: \R^d \to [0, 1]$ 满足以下 4 个条件时对应唯一概率测度 $\mu(A) = \Delta_A F$：
- 对所有分量不降
- 对所有分量右连续
- $x_n \downarrow -\infty \implies F(x_n) \downarrow 0$ 及 $x_n \uparrow +\infty \implies F(x_n) \uparrow 1$
- 对所有矩形 $A$ 有 $\Delta_A F \geq 0$
{% end %}
