+++
title = "【草稿】群论（三）：特征子群与 Hall 定理"
description = "（外）自同构，内自同构，特征子群，特征单群与 Hall 定理。"
date = 2025-08-08
updated = 2025-09-29

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "抽象代数", "群论"]
+++

{{ ref_index(to = "group-theory") }}

## 自同构
定义自同构群 $\mathrm{Aut}(G)$ 为全体自同构（也就是 $G\to G$ 的同构）构成的群。

内自同构群 $\mathrm{Inn}(G)$ 为全体内自同构 $\sigma: a\mapsto g^{-1}ag$ 构成的群。

## 特征子群
{% admonition(type="abstract", title="特征子群") %}
称 $G$ 的子群 $H$ 为它的**特征子群**，如果 $\sigma(H)\subseteq H, \forall \sigma\in\mathrm{Aut}(G)$
{% end %}

我们可以仿照它把正规子群的定义重写为 $\sigma(H)\subseteq H, \forall \sigma\in\mathrm{Inn}(G)$

## Hall 定理
{% admonition(type="abstract", title="Hall 定理") %}
若 $G$ 是 $mn$ 阶可解群，其中 $(n,m)=1$，则它
1. 存在 $m$ 阶子群
2. 任意两个 $m$ 阶子群共轭
3. 若有 $k$ 阶子群满足 $k\mid m$，则存在 $m$ 阶子群包含这个 $k$ 阶子群
{% end %}
