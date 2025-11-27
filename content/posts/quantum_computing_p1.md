+++
title = "【草稿】量子计算（一）"
description = "量子计算的一些系统学习。"
date = 2025-11-27

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "计算机", "算法"]
+++

前置知识
- 线性代数

参考的是 Scott Aaronson 的 [Introduction to Quantum Information Science](https://www.scottaaronson.com/qclec.pdf) 讲义。

## 基本原理
### 基本概念
我们讨论的量子状态是指有限维的 $\mathbb{C}^N$ 中的单位向量。

一个 qubit 是 $N=2$ 时的对应物，使用 Dirac 符号：

$$\binom{\alpha}{\beta} = \alpha\ |0\rangle + \beta\ |1\rangle = |\psi\rangle \tag{1.1}$$

上述右态矢称为 ket，我们还有左态矢称为 bra，并简记内积为 $\langle x | y \rangle$.

另外有约定：
- $|+\rangle = \frac{1}{\sqrt{2}} (|0\rangle + |1\rangle)$
- $|-\rangle = \frac{1}{\sqrt{2}} (|0\rangle - |1\rangle)$
- $|i\rangle = \frac{1}{\sqrt{2}} (|0\rangle + i|1\rangle)$

酉/幺正矩阵是把单位向量映到单位向量的矩阵，实值的为正交矩阵。

对单位复数 $c$，$|\psi\rangle$ 与 $c |\psi\rangle$ 是无法区分的，我们称其为 global phase，但我们能够区分 relative phase.

### 量子门与测量
我们把一些小的酉变换称为“门”，例如：

$$
\mathrm{NOT} = \begin{pmatrix}
0 & 1\\\\
1 & 0
\end{pmatrix} \tag{2.1}
$$

Hadamard 门可以将标准基 $\\{|0\rangle, |1\rangle\\}$ 变为 Hadamard 基 $\\{|+\rangle, |-\rangle\\}$.

$$
H = \frac{1}{\sqrt{2}} \begin{pmatrix}
1 & 1\\\\
1 & -1
\end{pmatrix} \tag{2.2}
$$

换基是有意义的行为，在基 $|V_0\rangle, \cdots , |V_{N-1}\rangle$ 下测量 $|\psi\rangle$ 得到 $|V_i\rangle$ 的概率是 $|\langle V_i | \psi \rangle|^2$.

测量行为是不可逆、依赖概率且不保证连续的。

电路记号如下：

![](/images/quantum/circuit_demo_1.png)

上图中左侧有两个 qubit 都被初始化成 $|0\rangle$，然后它们经过一个门 $U$，然后上面的 qubit 再经过一个 Hadamard 门，最终它们在标准基下被测量。

各种门及对应的符号如下：

![](/images/quantum/table_gates.png)

在上图 $\mathrm{CNOT}$ 门中，用实心点 $\bullet$ 表示控制的 qubit，用 $\oplus$ 表示被控制的 qubit；在 $\mathrm{CPHASE}$ 门中，会在被控制的 qubit 测量到 $|1\rangle$ 时对被控制的 qubit 使用 $Z$ 门。

---

对单个 qubit 来说，已经有一些有趣的性质了：
- The Quantum Zeno Effect 是指，我们每次在旋转了角 $k\cdot\epsilon$ 的标准基下测量，重复 $1/\epsilon$ 次，则每次都正确地坍缩最终到达 $|1\rangle$ 的概率是 $\approx 1 - \epsilon$，可以任意接近 $1$
- Watched Pot Effect 是指，如果一个在 $|0\rangle$ 的 qubit 正在向 $|1\rangle$ 旋转，每次旋转角 $\epsilon$，则若我们在每次旋转后观测它，经 $1/\epsilon$ 次，它到达 $|1\rangle$ 的概率只有 $\approx \epsilon$，从而在确定时间内增加观测频率可以让它任意大可能地固定在 $|0\rangle$

一个更具体的例子是 The Elitzur-Vaidman Bomb，假设我们有一个可能装了炸弹的箱子，但是打开箱子就会引爆它，我们如何在不引爆的前提下检测是否有炸弹呢？

如果打开箱子的行为可以变得更量子一些，我们就可以这样做：每次施加旋转，操作 $\pi/2\epsilon$ 次。

$$
R_\epsilon = \begin{pmatrix}
\cos \epsilon & -\sin \epsilon\\\\
\sin \epsilon & \cos \epsilon
\end{pmatrix} \tag{2.3}
$$

则我们可以得到结果，而只有 $\pi\epsilon/2$ 的结果引爆。

### 多 qubit 状态与纠缠
现在有一个硬币，我们如何判断它是公平的（$p=1/2$）还是非公平的（$p=1/2+\epsilon$），在标准情况下大约需要进行 $1/\epsilon^2$ 次投掷（这是因为有 Chebyshev 不等式 $P(|X-\mu|\geq k\sigma) \leq 1/k^2$），空间复杂度为 $O(\log (1/\epsilon^2))$.

量子方法是这样的：每次若抛出得到正面则进行旋转 $R_\epsilon$，反之旋转 $R_{-\epsilon}$，经过大约 $1/\epsilon^2$ 次投掷得到结果，全程只使用到一个 qubit.

这里还有一些细节问题：
1. 我们还需要执行正确的步数。实际上可以通过某种在每一步有特定概率停机的协议。
2. 如果 qubit 转了半圈（或者更多）怎么办？实际上还是可以正确判断的。

对两个 qubit，要通过观测区分它们，取的最有效的基是在同一个平面上，角平分线相同。
