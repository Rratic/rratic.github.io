+++
title = "【草稿】群论（三）：特征子群与 Hall 定理"
description = "（外）自同构，内自同构，特征子群，特征单群与 Hall 定理。"
date = 2025-08-08

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["讲义", "数学", "代数", "抽象代数", "群论"]
+++

[索引与符号表](/posts/index-group-theory/)

## 自同构
定义自同构群 $\rm{Aut}(G)$ 为全体自同构（也就是 $G\to G$ 的同构）构成的群。

内自同构群 $\rm{Inn}(G)$ 为全体内自同构 $\sigma: a\mapsto g^{-1}ag$ 构成的群。

## 特征子群

## Hall 定理
{% admonition(type="abstract", title="Hall 定理") %}
若 $G$ 是 $mn$ 阶可解群，其中 $(n,m)=1$，则它
1. 存在 $m$ 阶子群
2. 任意两个 $m$ 阶子群共轭
3. 若有 $k$ 阶子群满足 $k\mid m$，则存在 $m$ 阶子群包含这个 $k$ 阶子群
{% end %}
