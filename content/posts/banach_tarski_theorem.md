+++
title = "Banach–Tarski 分球佯谬"
description = "Banach–Tarski 分球定理的详细证明。"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学"]
+++

之前设计文创的时候看过大致的证明，但是没有验证细节。

## 找到自由群
我们先在 $\mathrm{SO}(3)$ 中找到由两个元素生成的自由群。

以下证明参考了[proof writing - There is a free group F2 in SO(3)](https://math.stackexchange.com/questions/1492799/there-is-a-free-group-f-2-in-so3).

取元素 $a$ 为绕 $x$ 轴转 $\arccos(\frac{1}{3})$，取 $b$ 为绕 $z$ 轴转 $\arccos(\frac{1}{3})$，就有：

$$
a =
\left(\begin{array}{ccc}
    1 & 0 & 0 \\\\
    0 & \frac{1}{3} & -\frac{2\sqrt{2}}{3} \\\\
    0 & \frac{2\sqrt{2}}{3} & \frac{1}{3}
\end{array}\right) \quad
b = \left(\begin{array}{ccc}
    \frac{1}{3} & -\frac{2\sqrt{2}}{3} & 0 \\\\
    \frac{2\sqrt{2}}{3} & \frac{1}{3} & 0 \\\\
    0 & 0 & 1
\end{array}\right)
$$

然后分析 $3a$ 与 $3b$ 的整数部分系数，对 $\operatorname{mod} 3$ 余数归纳。

## 划分球面
将上述自由群作用在球面上。考虑那些无限的轨道，使用选择公理在每个轨道中选择一个代表元，记这个集合为 $M$.

将自由群的元素分成五类，令 $G(a)$ 代表 $a$ 开头的词，并令 $B=a^{-1}M\cup a^{-2}M\cup \dots$，可将球面分成：
* $A_{1}=S(a)M\cup M\cup B$
* $A_{2}=S(a^{-1})M\setminus B$
* $A_{3}=S(b)M$
* $A_{4}=S(b^{-1})M$

我们得到 $aA_{2}=A_{2}\cup A_{3}\cup A_{4}$ 及 $bA_{4}=A_{1}\cup A_{2}\cup A_{4}$.

## 细节处理
将球沿半径划分成一族球面，作步骤三。现在只剩下可数个点（球心及轨道有限的点），使用标准方法处理。
