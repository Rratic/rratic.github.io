+++
title = "量子计算（一）：基本原理与基本结论"
description = "量子计算的一些系统学习。从量子状态定义到量子密集编码、量子隐形传态、纠缠的量化。"
date = 2025-11-27
updated = 2025-12-10

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
我们讨论的量子**状态**是指有限维的 $\mathbb{C}^N$ 中的单位向量。

一个 **qubit** 是 $N=2$ 时的对应物，使用 Dirac 符号：

$$\binom{\alpha}{\beta} = \alpha\ |0\rangle + \beta\ |1\rangle = |\psi\rangle \tag{1.1.1}$$

上述右态矢称为 ket，我们还有左态矢称为 bra，并简记内积为 $\langle x | y \rangle$.

另外有约定：
- $|+\rangle = \frac{1}{\sqrt{2}} (|0\rangle + |1\rangle)$
- $|-\rangle = \frac{1}{\sqrt{2}} (|0\rangle - |1\rangle)$
- $|i\rangle = \frac{1}{\sqrt{2}} (|0\rangle + i|1\rangle)$

酉/幺正矩阵是把单位向量映到单位向量的矩阵，实值的为正交矩阵。

对单位复数 $c$，$|\psi\rangle$ 与 $c |\psi\rangle$ 是无法区分的，我们称其为 global phase，但我们能够区分 relative phase.

### 量子门与观测
我们把一些小的酉变换称为“门”，例如：

$$
\mathrm{NOT} = \begin{pmatrix}
0 & 1\\\\
1 & 0
\end{pmatrix} \tag{1.2.1}
$$

Hadamard 门可以将**标准基** $\\{|0\rangle, |1\rangle\\}$ 变为 **Hadamard 基** $\\{|+\rangle, |-\rangle\\}$.

$$
H = \frac{1}{\sqrt{2}} \begin{pmatrix}
1 & 1\\\\
1 & -1
\end{pmatrix} \tag{1.2.2}
$$

换基是有意义的行为，在基 $|V_0\rangle, \cdots , |V_{N-1}\rangle$ 下**观测** $|\psi\rangle$ 得到 $|V_i\rangle$ 的概率是 $|\langle V_i | \psi \rangle|^2$.

观测行为是不可逆、依赖概率且不保证连续的。

电路记号如下：

![Circuit Demo 1](/images/quantum/circuit_demo_1.png)

上图中左侧有两个 qubit 都被初始化成 $|0\rangle$，然后它们经过一个门 $U$，然后上面的 qubit 再经过一个 Hadamard 门，最终它们在标准基下被观测。

各种门及对应的符号如下：

![Table of Gates](/images/quantum/table_gates.png)

在上图 $\mathrm{CNOT}$ 门中，用实心点 $\bullet$ 表示控制的 qubit，用 $\oplus$ 表示被控制的 qubit；在 $\mathrm{CPHASE}$ 门中，会在被控制的 qubit 观测到 $|1\rangle$ 时对被控制的 qubit 使用 $Z$ 门。

---

对单个 qubit 来说，已经有一些有趣的性质了：
- The Quantum Zeno Effect 是指，我们每次在旋转了角 $k\cdot\epsilon$ 的标准基下观测，重复 $1/\epsilon$ 次，则每次都正确地坍缩最终到达 $|1\rangle$ 的概率是 $\approx 1 - \epsilon$，可以任意接近 $1$
- Watched Pot Effect 是指，如果一个在 $|0\rangle$ 的 qubit 正在向 $|1\rangle$ 旋转，每次旋转角 $\epsilon$，则若我们在每次旋转后观测它，经 $1/\epsilon$ 次，它到达 $|1\rangle$ 的概率只有 $\approx \epsilon$，从而在确定时间内增加观测频率可以让它任意大可能地固定在 $|0\rangle$

一个更具体的例子是 Elitzur-Vaidman Bomb，假设我们有一个可能装了炸弹的箱子，但是打开箱子就会引爆它，我们如何在不引爆的前提下检测是否有炸弹呢？

如果打开箱子的行为可以变得更量子一些，我们就可以这样做：每次施加旋转，操作 $\pi/2\epsilon$ 次。

$$
R_\epsilon = \begin{pmatrix}
\cos \epsilon & -\sin \epsilon\\\\
\sin \epsilon & \cos \epsilon
\end{pmatrix} \tag{1.2.3}
$$

则我们可以得到结果，而只有 $\pi\epsilon/2$ 的结果引爆。

---

现在有一个硬币，想要判断它是公平的（$p=1/2$）还是非公平的（$p=1/2+\epsilon$），在标准情况下大约需要进行 $1/\epsilon^2$ 次投掷（这是因为有 Chebyshev 不等式 $P(|X-\mu|\geq k\sigma) \leq 1/k^2$），空间复杂度为 $O(\log (1/\epsilon^2))$.

量子方法是这样的：每次若抛出得到正面则进行旋转 $R_\epsilon$，反之旋转 $R_{-\epsilon}$，经过大约 $1/\epsilon^2$ 次投掷得到结果，全程只使用到一个 qubit.

这里还有一些细节问题：
1. 我们还需要执行正确的步数。实际上可以通过某种在每一步有特定概率停机的协议。
2. 如果 qubit 转了半圈（或者更多）怎么办？实际上还是可以正确判断的。

### 多 qubit 状态与纠缠
对两个 qubit，要通过观测区分它们，取的最有效的基是在同一个平面上，角平分线相同。

两个 qubit 的一般状态是 $|\psi\rangle = \alpha\ |00\rangle + \beta\ |01\rangle + \gamma\ |10\rangle + \delta\ |11\rangle$.

那么第一个 qubit 观测结果为 $|0\rangle$ 的概率是 $|\alpha|^2 + |\beta|^2$.

在此基础上，整个状态现在是：

$$|0\rangle \otimes \frac{\alpha\ |0\rangle + \beta\ |1\rangle}{\sqrt{|\alpha|^2 + |\beta|^2}} \tag{1.3.1}$$

部分测量的一般效果就是概率平摊。

$\mathrm{CNOT}$ 门是一个典型的作用于多个 qubit 且 qubit 相互影响的门。

另一类各 qubit 独立的门则可写用张量积写出，如：

$$
I \otimes \mathrm{NOT} = \begin{pmatrix}
0 & 1 & 0 & 0\\\\
1 & 0 & 0 & 0\\\\
0 & 0 & 0 & 1\\\\
0 & 0 & 1 & 0
\end{pmatrix} \tag{1.3.2}
$$

另有：
$$
(\mathrm{CNOT}) (H \otimes I) \begin{pmatrix} 1 \\\\ 0 \\\\ 0 \\\\ 0 \end{pmatrix} =
\begin{pmatrix} \frac{1}{\sqrt{2}} \\\\ 0 \\\\ 0 \\\\ \frac{1}{\sqrt{2}} \end{pmatrix} \tag{1.3.3}
$$

这一结果称为 **Singlet/Bell Pair/EPR Pair**，它的一个有趣性质是观测第一个 qubit 将会使第二个 qubit 坍缩。称这种状态为**纠缠**。

---

纠缠行为将带来一个有趣的现象，即疑似存在信息的超光速传播。

例如说，现在有两个纠缠的粒子，状态为 Bell pair，一个人 Alice 把第一个粒子留着地球上，另一个人 Bob 把第二个粒子带到月球上。此时，若 Alice 在标准基下观测，就能立即知道 Bob 的观测结果。对此的解释是，Alice 并不能控制观测的结果。

1935 年一篇 Einstein，Podolsky 与 Rosen 的论文中给出了一个更有迷惑性的实验：如果 Alice 在 Hadamard 基下观测，则 Bob 的粒子状态会坍缩到 Hadamard 基之一而不是标准基之一！对此的解释是，Bob 需要自己进行观测，而结果的概率是一样的。

## 混合状态
### 密度矩阵
我们可以把一个混合状态表示成一个分布 $\\{(p_i, |\psi_i\rangle)\\}$，而为了有唯一性，进一步表示成**密度矩阵**：

$$\rho = \sum_i p_i |\psi_i\rangle \langle\psi_i| \tag{2.1}$$

其中外积定义成一个矩阵元是左边对应项乘以右边对应项的复共轭，从而 $\rho$ 将是一个 Hermitian 矩阵。它还满足迹 $1$ 和半正定，其秩则给出了需要混合的最小纯状态数量。

这样一来，我们知道 $\\{(1/2, |0\rangle), (1/2, |1\rangle)\\} \cong \\{(1/2, |+\rangle), (1/2, |-\rangle)\\}$.

回到之前纠缠行为带来的现象，称最大混合状态是指形如 $k\cdot I$ 的密度矩阵形式，这与基无关，从而得到 No-Communication Theorem.

作用酉变换 $U$ 将意味着把 $\rho$ 变为 $U\rho U^\dagger$.

### Bloch 球面
表示一个 qubit 的 **Bloch 球面**如图，正交的向量在其上表示成相反的。在球面上的是纯状态，球内的是混合状态。
![Bloch Sphere](/images/quantum/bloch_sphere.png)

图中有：

$$|\psi\rangle = \cos \frac{\theta}{2} |0\rangle + e^{i\varphi} \sin \frac{\theta}{2} |1\rangle \tag{2.2}$$

此时，$X, Y, Z, H$ 门分别是绕着 $|+\rangle, |i\rangle, |0\rangle$ 与 $(|0\rangle+|+\rangle)/(2\cos \pi/8)$ 对应轴旋转 $\pi$.

易见任何混合状态都可以写成两个纯状态的复合。

## 量子信息
### 不可复制定理
**不可复制定理**说，不存在一个门 $U$ 将 $|\psi\rangle, |0\rangle$ 变作 $|\psi\rangle, |\psi\rangle$，这可以通过写出表达式说明不是线性的；$\mathrm{CNOT}$ 门可以复制 $|0\rangle, |1\rangle$，经典信息本来就可以复制。

换一个角度来说明这件事情：若 $U(|\psi\rangle \otimes |0\rangle) = (|\psi\rangle, |\psi\rangle)$，考虑酉变换的保内积性，只能 $|\psi\rangle$ 为 $|0\rangle$ 或 $|1\rangle$.

### Quantum Money
依据这个定理，Wiesner 在 1969 年提出了 Quantum Money 的设想，其运行如下：
1. 一个钞票的构成包括：
	- 一串经典比特 $s \in \\{0, 1\\}^m$
	- 一个量子状态 $\psi_{f(s)}$，其中的 qubit 不纠缠，且是 $|0\rangle, |1\rangle, |+\rangle, |-\rangle$ 之一
2. 银行存储了所有的 $f(s)$ 结果
3. 在验钞时，银行查看数据库，然后对每个 qubit 这样观察：若它是 $|0\rangle$ 或 $|1\rangle$，则用标准基观测；反之用 Hadamard 基观测

Molina, Vidick 与 Watrous 在 2012 年证明了，任何尝试把单个钞票变成两个钞票的造假成功率只有 $(3/4)^n$，如果不考虑保存量子状态的技术问题，这看起来很有用。

---

现在我们假设银行会在验钞正确时退回钞票，而在验钞错误时触发警报。对此，有一种类似 Elitzur-Vaidman Bomb 的攻击方法：假设我们想要知道 $|\psi_i\rangle$ 这个 qubit 是什么，就这样做：
1. 将一个 qubit $|c\rangle$ 初始化为 $|0\rangle$
2. 重复 $\pi/2\epsilon$ 次：
	- 对 $|c\rangle$ 施加旋转 $R_\epsilon$
	- 对 $|c\rangle|\psi_i\rangle$ 施加 $\mathrm{CNOT}$
	- 把整个钞票送到银行检验

为此，可以改为让银行在验钞正确时改为退回一个新的同等面额的钞票。

---

现在 Quantum Money 的设想还有一个缺点：每次验钞都必须通过银行解决。为此，我们来考虑 Public-Key Quantum Money 的设想。与密码学的公私钥想法类似，任何人都可以用公钥验钞（我们假定攻击者的算力有某个上限而无法枚举），但只能用私钥来生成/复制钞票。

### Quantum Key Distribution
考虑一个典型的密码学场景：Alice 与 Bob 有一个共有的密钥（随机二进制串），则他们可以用 One-Time Pad 来交流：Alice 发出的消息是明文与密钥按位异或的结果，Bob 收到消息后再按位异或一遍。在经典世界中，Claude Shannon 证明了如果窃听者的算力没有上限，则只有在密钥至少有明文那么长时才能保证安全。而使用 **Quantum Key Distribution** 就可不对算力上限作假设。[^satellite-based-qkd]

现在来介绍 **BB84 协议**：
1. Alice 随机生成两个字符串 $x, y \in \\{0, 1\\}^n$
2. Alice 生成一个量子状态，其中每个 qubit 由 $y, x$ 的对应位分别决定使用标准基还是 Hadamard 基、是哪一个
3. Alice 把该量子状态 $|\psi\rangle$ 发给 Bob
4. Bob 随机生成一个字符串 $y' \in \\{0, 1\\}^n$
5. Bob 用 $y'$ 决定在哪组基下观测 $|\psi\rangle$ 并记录
6. Alice 与 Bob 分享他们观测时使用的基，然后把使用相同基的位拼起来作为现在的密钥

### 量子密集编码
Holevo 定理指出，一个 qubit 最多能传递一个经典 bit 的信息。

**量子密集编码 Superdense Coding** 是一种在预先纠缠的情形下使用一个 qubit 传递两个经典 bit 的协议。现在我们假设 Alice 预先给 Bob 了一个 Bell pair $(|00\rangle + |11\rangle) / \sqrt{2}$，那么她可以考虑四种作用 $I \otimes I$，$X \otimes I$，$Z \otimes I$ 与 $(Z \otimes I)(X \otimes I)$ 并择其一将结果发给 Bob，然后 Bob 可以使用 $\mathrm{CNOT}$ 与 $H$ 解码。

所以现在我们有：

$$\text{1 qubit + 1 ebit} \geq \text{2 bits} \tag{3.1.1}$$

但是把 $2$ 改成更大的整数是做不到的。

### 量子隐形传态
**量子隐形传态 Quantum Teleportation** 所做的是：

$$\text{1 ebit + 2 bits} \geq \text{1 qubit} \tag{3.1.2}$$

电路表示如下：

![Circuit Teleportation](/images/quantum/circuit_teleportation.png)

经过 $\mathrm{CNOT}$ 和 $H$ 给出：

$$(\alpha |0\rangle + \beta |1\rangle) \otimes \frac{|00\rangle + |11\rangle}{\sqrt{2}} \longrightarrow \frac{1}{2}(\alpha|000\rangle + \alpha|100\rangle + \alpha|011\rangle + \alpha|111\rangle + \beta|010\rangle - \beta|110\rangle + \beta|001\rangle - \beta|101\rangle)$$

经观测将坍缩到：
| 00  | 01  | 10  | 11  |
| :-: | :-: | :-: | :-: |
| $\alpha\|0\rangle + \beta\|1\rangle$ | $\alpha\|1\rangle + \beta\|0\rangle$ | $\alpha\|0\rangle - \beta\|1\rangle$ | $\alpha\|1\rangle - \beta\|0\rangle$ |

### 更多的纠缠
我们知道纠缠不一定需要通过直接接触发生。使用 teleportation 我们还可以完成纠缠的交换 **Entanglement Swapping**.

例如，两个人 Alice 与 Alice' 拥有纠缠的 qubit，则她们使用之前的方法将 qubit 传递给 Bob 与 Bob' 会导致纠缠被传递到 Alice 与 Alice' 间。

---

Bell pair 有一个 3-qubit 的版本 **GHZ 状态** $(|000\rangle + |111\rangle) / \sqrt{2}$，它具有一种类似三叶结的结构：只有把三个 qubit 放在一起才会体现出纠缠。

**Monogamy of Entanglement** 指出，如果一个 qubit 与另一个 qubit 形成了最大混合状态，则它不能和其它一个 qubit 形成最大混合状态。

### 纠缠的量化
我们来考虑二分态（两个子系统组成的复合状态，可以是混合的），形如：

$$\sum_{ij} \alpha_{ij} |s_i\rangle |t_j\rangle \tag{3.2.1}$$

两人可以各取一组正交基，将它表示为 Schmidt 型（可以通过奇异值分解实现）：

$$\sum_i \lambda_i |v_i\rangle |w_i\rangle \tag{3.2.2}$$

---

我们回忆对经典的概率分布，其 Shannon 熵为：

$$H(P) = \sum_{i=0}^{n-1} p_i \log_2 \frac{1}{p_i} \tag{3.2.3}$$

对量子态来说，可以推广为 **von Neumann 熵**：

$$S(\rho) = \sum_{i=0}^{n-1} \gamma_i \log_2 \frac{1}{\gamma_i} \tag{3.2.4}$$

实际上我们有：

$$S(\rho) = \min_U H(\operatorname{diag} U\rho U^\dagger) \tag{3.2.5}$$

---

为计算两个子系统的纠缠程度，我们定义**纠缠熵**：

$$E(|\psi\rangle) = S(\rho^A) = S(\rho^B) \tag{3.2.6}$$

可以算得 Bell pair 的纠缠熵为 $1$.

2003 年 Leonid Gurvits 证明了：根据密度矩阵判断它是否代表一个纠缠的状态是 NP-hard 的。

---

[^satellite-based-qkd]: 可以在 <https://arxiv.org/abs/1707.00542> 看到一个我国于 2017 年所作的基于卫星的 1200km QKD 实验论文。
