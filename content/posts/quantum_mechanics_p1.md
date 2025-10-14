+++
title = "【草稿】《初识量子力学》笔记"
date = 2025-10-05
updated = 2025-10-13

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "物理", "量子力学"]
+++

前置知识
- 线性代数（矩阵运算即可）
- 一般的经典力学基础

参考阅读
- [《初识量子力学》](https://chaoli.club/index.php/10485)

## 矩阵革命
人们发现原子光谱中谱线角频率总是可以被一组正整数表示成 $\omega_{mn}$，且满足里兹组合规则

$$\omega_{mn} = \omega_{ml} + \omega_{ln} \tag{1.1}$$

经典物理不仅解释不了原子光谱的分立性，而且无法解释原子的稳定性（按理说电子会不断辐射发出电磁辐射而失去能量）。

后来人们整理出氢原子的光谱的公式，即巴尔末-里德堡公式：

$$\omega_{mn} = 2\pi Rc \left(\frac{1}{m^2}-\frac{1}{n^2}\right) \tag{1.2}$$

其中 R 是 Rydberg 常数。

对于氢原子光谱，玻尔提出了三条假设
* **定态**假设：核外电子绕核运动只能取某些特定的轨道，此时不辐射电磁波损失能量，有恒定能量 $E_n$
* 跃迁假设：电子从第 n 个定态跃迁到第 m 个定态，辐射出频率 $\omega_{mn}$ 的电磁波
* 轨道角动量量子化假设：第 n 个定态上电子的角动量 $J = rp = n\hbar$

令精细结构常数 $\alpha = \frac{e^2}{4\pi\epsilon_0\hbar c}\approx \frac{1}{137}$

可推导

$$
\left\\{\begin{matrix}
m_e\frac{v^2}{r} = \frac{e^2}{4\pi\epsilon_0 r^2} \\\\
rm_ev = n\hbar
\end{matrix}\right.
$$

有

$$E_n = -\frac{1}{2n^2}m_e\alpha^2 c^2 \tag{1.3}$$

尽管玻尔的理论从结构上不令人满意，定态和定态跃迁成为了量子力学基本性的观念。

海森堡后来指出，电子轨道不可以观察，更恰当的看法是，定态是原子的一种确定可能性，定态跃迁也是随机的。

电动力学推导出，一个加速运动的电子辐射电磁波的功率为

$$P = \frac{e^2}{3\pi\epsilon_0 c^3}|\ddot{x}|^2 \tag{1.4}$$

其中 $x$ 是轨道。

通过普朗克对黑体辐射公式的推导，海森堡知道，原子定态跃迁发出电磁辐射的过程可以看成简谐振动，写为

$$x_{mn} = x_{0_{mn}} e^{-i\omega_{mn}t} \tag{1.5}$$

其中 $x_{0_{mn}}$ 与时间无关。

为了遍历所有的正整数 $(m, n)$，规定 $\omega_{mn}=-\omega_{nm}, \omega_{nn}=0$，我们有包含所有 $x_{mn}$ 的无穷大表格（尽管海森堡当时还不知道矩阵）。

{% admonition(type="info", title="Hermitian 矩阵") %}
我们称 Hermitian 共轭（有时被音译为厄米/埃尔米特共轭）是指 $A^\dagger = \left(A^T\right)^*$，

其中 $A^T$ 是矩阵转置，$A^*$ 是（按分量）复共轭。

一个矩阵是 Hermitian 矩阵，若 $A = A^\dagger$
{% end %}

易验证上述表格构成 Hermitian 矩阵。

我们对时间求导，再乘以质量，得到一个关于动量的矩阵，它也是 Hermitian 的。

在物理中会遇到这样的问题：有些物理量，例如能量，需要将两个这样的无穷大表格乘在一起。

式 (1.1) 对中间的 $l$ 态没有任何限制，海森堡选择将它们都加起来，这就对应矩阵的乘法。

量子力学中还会用到对易子运算：

{% admonition(type="info", title="对易子") %}
对易子定义为 $[A, B] = AB-BA$

它满足双线性、反对称性和 Jacobi 恒等式，故构成李代数。

如果 $A, B$ 都是 Hermitian 矩阵，则 $i[A, B]$ 也是 Hermitian 矩阵。
{% end %}

现在考虑哈密顿量对应的矩阵。我们知道它不能依赖于时间，因此它是对角矩阵。

$$
H = \begin{bmatrix}
E_1 & 0   & 0 & \cdots \\\\
0   & E_2 & 0 & \cdots \\\\
0   & 0 & E_3 & \cdots \\\\
\vdots & \vdots & \vdots & \ddots
\end{bmatrix} \tag{1.6}
$$

其中 $\omega_{mn}=(E_n-E_m)/\hbar$

对任意物理量 $A$ 的对应矩阵，只要满足 (1.5) 条件，对时间求导，可以得到海森堡运动方程：

$$i\hbar \frac{\mathrm{d}A}{\mathrm{d}t} = [A, H] \tag{1.6}$$

如果一个物理量满足 $[A, H] = 0$，则就有 $\frac{\mathrm{d}A}{\mathrm{d}t}=0$，它是守恒量。

可以证明，若 $A, B$ 均满足 (1.6) 方程，则 $\lambda A, A+B, AB$ 也满足该方程，进而 $[A, B]$ 也满足该方程。

现在考虑经典系统的哈密顿量：

$$H = \frac{P^2}{2m_e} + V(X) \tag{1.7}$$

其中 $V(X)$ 使用 $V(x)$ 展开成幂级数的形式（为什么是合理的？）

有 $[X, X^n] = X[X, X^{n-1}] + X[X, X^{n-1}]X = \cdots = 0$，故 $[X, V(X)] = 0$

如果我们要求量子系统与相应的经典系统相对应，则能得出 $[X, P] = i\hbar$

---

最后，让我们换一个写法，记

$$A_{mn} = \langle m|\hat{A}|n\rangle \tag{1.8}$$

那么就有 $\langle m|\hat{A}^\dagger|n\rangle = \langle m|\hat{A}|n\rangle^*$ 及 $\langle m|n \rangle = \langle m|\mathbf{1}|n \rangle =\delta_{mn}$.

不严格地说，可以把左态矢 $\langle m|$ 看作行向量，把右态矢 $|n \rangle$ 看作列向量。

## 量子力学基本原理
我们现在讨论一般的可能性集（而非氢原子电子的定态）。

{{ todo() }}
