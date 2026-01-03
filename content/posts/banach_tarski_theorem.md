+++
title = "Banach–Tarski 分球定理的证明细节"
description = "Banach–Tarski 分球定理的用到的想法和细节处理。"
date = 2026-01-01

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "代数", "群论"]
+++

之前设计文创的时候看过大致的证明，但是没有验证细节。

Banach–Tarski 分球佯谬说的是，可以把 $\mathbb{R}^3$ 中的单位球分成有限个（可以做到 5 个）点集的无交并，经过旋转、平移得到两份单位球。如果忽略只允许旋转、平移的条件的话，从纯集合论角度是合理的，但整个结论是反直觉的。

## 找到自由群
我们先在 $\mathrm{SO}(3)$ 中找到由两个元素生成的自由群。

---

以下证明参考了 [proof writing - There is a free group F2 in SO(3)](https://math.stackexchange.com/questions/1492799/there-is-a-free-group-f-2-in-so3).

取元素 $a$ 为绕 $x$ 轴转 $\arccos(\frac{1}{3})$，即：

$$
a = \begin{pmatrix}
1 & 0 & 0 \\\\
0 & \frac{1}{3} & -\frac{2\sqrt{2}}{3} \\\\
0 & \frac{2\sqrt{2}}{3} & \frac{1}{3}
\end{pmatrix}
$$

取 $b$ 为绕 $z$ 轴转 $\arccos(\frac{1}{3})$，即：

$$
b = \begin{pmatrix}
\frac{1}{3} & -\frac{2\sqrt{2}}{3} & 0 \\\\
\frac{2\sqrt{2}}{3} & \frac{1}{3} & 0 \\\\
0 & 0 & 1
\end{pmatrix}
$$

则 $3a, 3b$ 均属于：

$$
\begin{pmatrix}
\mathbb{Z} & \sqrt{2}\mathbb{Z} & \mathbb{Z} \\\\
\sqrt{2}\mathbb{Z} & \mathbb{Z} & \sqrt{2}\mathbb{Z} \\\\
\mathbb{Z} & \sqrt{2}\mathbb{Z} & \mathbb{Z}
\end{pmatrix}
$$

其在乘法下封闭。

考虑各个整数系数在 $\mod 3$ 下的等价类，记（都省略了 $\sqrt{2}$ 及等价类符号）：

$$
A = [3a] = \begin{pmatrix}
0 & 0 & 0 \\\\
0 & 1 & 1 \\\\
0 & -1 & 1
\end{pmatrix}, \quad
B = \begin{pmatrix}
0 & 0 & 0 \\\\
0 & 1 & 1 \\\\
0 & 1 & -1
\end{pmatrix}, \quad
C = \begin{pmatrix}
0 & 1 & -1 \\\\
0 & 1 & 1 \\\\
0 & 0 & 0
\end{pmatrix}, \quad
D = \begin{pmatrix}
0 & -1 & 1 \\\\
0 & 1 & 1 \\\\
0 & 0 & 0
\end{pmatrix}
$$

则有（左乘上）：

| $\times$ | $A$ | $B$ | $C$ | $D$ |
| :-: | :-: | :-: | :-: | :-: |
| $A = [3a]$ | $-A$ | $\mathbf{0}$ | $A$ | $A$ |
| $[3a^{-1}]$ | $\mathbf{0}$ | $-B$ | $B$ | $B$ |
| $[3b]$ | $C$ | $C$ | $-C$ | $\mathbf{0}$ |
| $[3b^{-1}]$ | $D$ | $D$ | $\mathbf{0}$ | $-D$ |

讨论知一个词是 $\mathbf{0}$ 只能是将 $[3a]$ 与 $[3a^{-1}]$ 对着消除掉。

---

另有一个使用 Ping-Pong Lemma 的证法，来自 [That trick where you embed the free group into a Lie group](https://sbseminar.wordpress.com/2007/09/17/that-trick-where-you-embed-the-free-group-into-a-lie-group/).

{% admonition(type="theorem", title="二元版 Ping-Pong Lemma") %}
$G$ 可由 $a, b$ 生成，有在 $X$ 上的作用。若存在 $X$ 的不交的非空子集 $A^+, B^+$ 及非空子集 $A^-, B^-$，使得 $a(X\setminus A^-) \subseteq A^+, b(X\setminus B^-) \subseteq B^+, a^{-1}(X\setminus A^+) \subseteq A^-, b^{-1}(X\setminus B^+) \subseteq B^-$，则 $G = \langle a, b \rangle$ 是自由的。
{% end %}

这是显然的。

为了使用 Ping-Pong Lemma, 考虑 $\mathrm{PGL}(2, \mathbb{R})$ 的情形。记 $\Delta$ 为上半平面，四个圆心在 x 轴上的半圆在 x 轴上截出 $(-3, 3), (-3, -1), (-1, 1), (1, 3)$，记内部为 $U, V_2, V_3, V_4$，令 $V_1$ 为 $\Delta \setminus U$ 的内部，考虑：

$$
a = \begin{pmatrix}
\sqrt{3} & 0 \\\\
0 & \frac{1}{\sqrt{3}}
\end{pmatrix}, \quad
b = \begin{pmatrix}
2 & -3 \\\\
-1 & 2
\end{pmatrix}
$$

则 $a(\Delta \setminus V_3) = V_1, b(\Delta \setminus V_4) = V_2, a(\Delta \setminus V_1) = V_3, b(\Delta \setminus V_2) = V_4$.

这可以被做成 $\mathrm{SO}(3, \mathbb{Q}_5)$ 上的，进而做成 $\mathrm{SO}(3)$ 上的。

---

另记一个有趣的事实，两个元素生成的自由群有子群为三个元素生成的自由群（取 $a^2, ab, b^2$ 为生成元即可）。

## 划分球面
将上述自由群作用在球面上，考虑轨道。

一个轨道是有限的，那么必然是因为对其中的一个点 $x$，有至少一个有限的非平凡词 $\psi$ 使 $\psi x = x$. 这样的点至多可数个，最后再处理。

对无限的轨道，使用选择公理在每个轨道中选择一个代表元，记这些代表元构成集合为 $M$.

将自由群 $\langle a, b \rangle$ 的元素分成五类，令 $S(a)$ 代表约化后 $a$ 开头的词，并令：

$$B = \bigcup_{n \geq 1} a^{-n} M$$

现在可将球面分成：
* $A_1 = S(a)M \cup M \cup B$
* $A_2 = S(a^{-1})M \setminus B$
* $A_3 = S(b)M$
* $A_4 = S(b^{-1})M$

我们得到 $aA_2 = A_2 \cup A_3 \cup A_4$ 及 $bA_4 = A_1 \cup A_2 \cup A_4$. 那么 $A_1, aA_2, A_3, bA_4$ 构成两份 $A_1, A_2, A_3, A_4$.

## 细节处理
将球沿半径划分成一族球面，作上述划分球面的步骤（实际上对于不同半径处的球面把对应的 $A_i$ 合在一起，所以总共只划分成有限个集合）。

那么现在剩下的未处理的点是球心和那些在有限轨道上的点。

考虑这个事实：

$$g^{-1}(\\{x, gx, g^2x, \cdots\\}) = \\{x, gx, g^2x, \cdots\\} \cup \\{g^{-1}x\\}$$

其中集合 $\\{x, gx, g^2x, \cdots\\}$ 可以取自已经成功处理的点集。多出的一个点可以平移到圆心，因此圆心总是可以被处理的。

设某个球面上那些轨道有限的点构成集合 $D$. 我们可以取出一个对称轴和一个角度，使得这个旋转 $g$ 在 $D$ 上没有不动点。现在仍然可以使用前述事实，对应的集合 $\bigcup_{n \geq 1} g^n (D)$ 是已经成功处理的。

这样一来，我们只划分了有限个集合（应当是 24 个）完成了复制。通过一些代数手段可以减少到 5 个。
