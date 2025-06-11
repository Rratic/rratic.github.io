+++
title = "希尔伯特第三问题"
description = "给定两个体积相等的多面体，是否总能将一个切成有限个多面体，通过平移、旋转组合成另一个？"
date = 2025-04-09
updated = 2025-04-20

[extra]
math = true
toc = true

[taxonomies]
categories = ["知识"]
tags = ["数学", "笔记", "连续", "具体", "几何", "初等几何"]
+++

## 介绍 {#about}
希尔伯特第三问题是 1900 年希尔伯特《数学问题》演讲[^hilbert23reprinted]提出的 23 个问题中最早得到解决的。

在英文重制文本中，可以看到这样一段描述。
>  Gauss mentions in particular the theorem of Euclid, that triangular pyramids of equal altitudes are to each other as their bases.

这个问题某种程度上等价于：
{% admonition(type="question", title="希尔伯特第三问题") %}
给定两个体积相等的多面体，是否总能将一个切成有限个多面体，通过平移、旋转组合成另一个？
{% end %}

该问题由 Max Dehn 在 1901 年 9 月使用不变量解决。[^paper-dehn]

## 背景 {#background}
### 平面情形 {#background-plane}
在平面中，类似问题的回答是肯定的，这是 Wallace–Bolyai–Gerwien 定理。

只需证明，多边形可以通过剪切拼接变为面积相等的正方形。这可以通过如下操作完成：
1. 将多边形剪成三角形。
2. 三角形可以沿中位线剪，然后接平行四边形。
3. 平行四边形可以剪接成矩形。
4. 矩形可以剪接成正方形（通过把它变为一边长为 $\sqrt{ab}$ 的平行四边形）。
5. 两个正方形可以剪接成大正方形（将正方形并排放置，割出两个斜边 $\sqrt{a^2+b^2}$ 的直角三角形）。

## 原问题 {#original-question}
### Dehn 不变量 {#dehn-invariant}
对一个多面体，其 $n$ 个棱长为 $l_1, l_2, \cdots l_n$，对应的二面角为 $\theta _1, \theta _2, \cdots \theta _n$，则该不变量被定义为：

$$\sum_{i=1}^n l_i\otimes \theta _i \in \mathbb{R}\otimes _\mathbb{Z} \mathbb{R}/\pi\mathbb{Z}$$

这一二元组允许两个过程：
- $(l_1 + l_2, \theta) = (l_1, \theta) + (l_2, \theta)$ 在棱上剪/接。
- $(l, \theta _1 + \theta _2) = (l, \theta _1) + (l, \theta _2)$ 在角上剪/接。

有些文章中会采取 $\sum_{i=1}^n l_i \theta _i$，这足以完成证明，但会导致无法区分形如“边长为 π”的多面体。

### 张量积 {#tensor-product}
这里解释一下张量积 $\otimes$ 的严格定义。

两个向量空间 $V,W$ 的张量积 $V\otimes W$ 是一个向量空间，由形如

$$(v, w), v\in V, w\in W$$

的元素张成，且满足双线性关系。

$$(a_1v_1 + a_2v_2, w) = a_1(v_1, w) + a_2(v_2, w)$$
$$(v, a_1w_1 + a_2w_2) = a_1(v, w_1) + a_2(v, w_2)$$

前文中 $\otimes _\mathbb{Z}$ 是指这里 $a\in \mathbb{Z}$，在无歧义时可省略。且易证明 $\mathbb{R}\otimes _\mathbb{Z} \mathbb{R}/\pi\mathbb{Z} = \mathbb{R}\otimes _\mathbb{Q} \mathbb{R}/\pi\mathbb{Q}$。

张量积使得 $V\times W$ 上的双线性函数可以等价地看成 $V\otimes W$ 上的线性函数。

### 回答 {#answer}
棱长为 1 的正四面体的 Dehn 不变量为 $(6, 2\arctan\frac{\sqrt{2}}{2})$​，这不能化约成 0[^not-zero]。而任意长方体的 Dehn 不变量都为 0。

因此取体积与正四面体相等的长方体即为反例。

## 加强 {#enhancement}
Jean-Pierre Sydler 于 1965 年证明体积与 Dehn 不变量都相同的多面体一定可通过互相剪拼得到。[^paper-sydler]

[^hilbert23reprinted]: 一个英文重制文本见于 <https://people.tamu.edu/~rojas//hilbert23reprinted.pdf>。
[^paper-dehn]: <https://link.springer.com/article/10.1007/BF01448001>
[^not-zero]: 否则存在 $\frac{a}{b}\in\mathbb{Q}$ 使得 $\tan(\frac{a}{b}\pi)=\frac{\sqrt{2}}{2}$。一方面设 $y=e^{i\frac{a}{b}\pi}$ 则这等于 $-i\frac{y^2-1}{y^2+1}$，满足 $2(y^2-1)^2+(y^2+1)^2=0$。另一方面 $y$ 为单位根，极小多项式是分圆多项式，枚举知不成立。
[^paper-sydler]: <https://link.springer.com/article/10.1007/BF02564364>
