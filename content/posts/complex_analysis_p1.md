+++
title = "复分析（一）：速通"
description = "重要概念的速通。"
date = 2025-07-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["讲义", "数学", "分析", "复分析"]
+++

> 复分析会告诉你：性质好的函数性质有多好。

前置知识
- 数学分析

## 复函数
在复数域 $\mathbb{C}$ 上，极限的表述可以参照数学分析中的表述。

{% admonition(type="abstract", title="极限") %}
称 $\lim_{z \to z_0} f(z) = w$，如果对任意 $\omega > 0$，存在 $\delta > 0$，对任意 $0<|z-z_0|<\delta$ 有 $|f(z)-w|<\omega$.
{% end %}

更一般地，可以参考[拓扑](/posts/index-topology/)中的紧性相关定义。
