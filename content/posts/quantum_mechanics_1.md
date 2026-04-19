+++
title = "《初识量子力学》笔记（上）：海森堡绘景到薛定谔绘景"
description = "《初识量子力学》是一个理顺量子力学理论的尝试。"
date = 2026-03-06

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "物理", "量子物理"]
+++

前置知识
- 线性代数
- 一般的经典力学基础，参考[《初识经典力学》笔记](@/posts/classical_mechanics_1.md)

参考的是[《初识量子力学》](https://chaoli.club/index.php/10485)前三章。

## 氢原子光谱
### 矩阵引入与运动方程
人们发现原子光谱中谱线角频率总是可以被一组正整数表示成 $\omega_{mn}$，且满足里兹组合规则：

$$\omega_{mn} = \omega_{ml} + \omega_{ln} \tag{1.1}$$

经典物理不仅解释不了原子光谱的分立性，而且无法解释原子的稳定性（按理说电子会不断辐射发出电磁辐射而失去能量）。

后来人们整理出氢原子的光谱的公式，即巴尔末-里德堡公式：

$$\omega_{mn} = 2\pi Rc \left(\frac{1}{m^2}-\frac{1}{n^2}\right) \tag{1.2}$$

其中 $R$ 是 Rydberg 常数，约 $1.097 \times 10^7\ \text{m}^{-1}$.

对于氢原子光谱，玻尔提出了三条假设：
* **定态**假设：核外电子绕核运动只能取某些特定的轨道，此时不辐射电磁波损失能量，有恒定能量 $E_n$
* 跃迁假设：电子从第 $n$ 个定态跃迁到第 $m$ 个定态，辐射出频率 $\omega_{mn}$ 的电磁波
* 轨道角动量量子化假设：第 $n$ 个定态上电子的角动量 $J = rp = n\hbar$

令精细结构常数 $\alpha = \frac{e^2}{4\pi\epsilon_0\hbar c}\approx \frac{1}{137}$，并使用条件 $m_e\frac{v^2}{r} = \frac{e^2}{4\pi\epsilon_0 r^2}$ 及 $rm_ev = n\hbar$ 可推导得：

$$E_n = -\frac{1}{2n^2}m_e\alpha^2 c^2 \tag{1.3}$$

尽管玻尔的理论从结构上不令人满意，定态和定态跃迁成为了量子力学基本性的观念。

海森堡后来指出，电子轨道不可以观察，更恰当的看法是，定态是原子的一种确定可能性，定态跃迁也是随机的。

电动力学推导出，一个加速运动的电子辐射电磁波的功率为：

$$P = \frac{e^2}{3\pi\epsilon_0 c^3}|\ddot{x}|^2 \tag{1.4}$$

其中 $x$ 是轨道。

通过普朗克对黑体辐射公式的推导，海森堡知道，原子定态跃迁发出电磁辐射的过程可以看成简谐振动，写为：

$$x_{mn} = x_{0_{mn}} e^{-i\omega_{mn}t} \tag{1.5}$$

其中 $x_{0_{mn}}$ 与时间无关。

为了遍历所有的正整数 $(m, n)$，规定 $\omega_{mn}=-\omega_{nm}, \omega_{nn}=0$，我们有包含所有 $x_{mn}$ 的无穷大表格（尽管海森堡当时还不知道矩阵）。

{% admonition(type="definition", title="Hermitian 矩阵") %}
我们称 Hermitian 共轭（有时被音译为厄米/埃尔米特共轭）是指 $A^\dagger = \left(A^\top\right)^\ast$，其中 $A^\top$ 是矩阵转置，$A^\ast$ 是（按分量）复共轭。

一个矩阵是 Hermitian 矩阵，若 $A = A^\dagger$.
{% end %}

易验证上述表格构成 Hermitian 矩阵。

我们对时间求导，再乘以质量，得到一个关于动量的矩阵，它也是 Hermitian 的。

在物理中会遇到这样的问题：有些物理量，例如能量，需要将两个这样的无穷大表格乘在一起。

式 (1.1) 对中间的 $l$ 态没有任何限制，海森堡选择将它们都加起来，这就对应矩阵的乘法。

量子力学中还会用到对易子运算：

{% admonition(type="definition", title="对易子") %}
对易子定义为 $[A, B] = AB-BA$. 它满足双线性、反对称性和 Jacobi 恒等式，故构成李代数。

如果 $A, B$ 都是 Hermitian 矩阵，则 $i[A, B]$ 也是 Hermitian 矩阵。
{% end %}

现在考虑哈密顿量对应的矩阵。我们知道它不能依赖于时间，因此它是对角矩阵。

$$
H = \begin{bmatrix}
E_1 & 0   & 0 & \cdots \cr
0   & E_2 & 0 & \cdots \cr
0   & 0 & E_3 & \cdots \cr
\vdots & \vdots & \vdots & \ddots
\end{bmatrix} \tag{1.6}
$$

其中 $\omega_{mn}=(E_n-E_m)/\hbar$.

对任意物理量 $A$ 的对应矩阵，只要满足 (1.5) 条件，对时间求导，可以得到海森堡运动方程：

$$i\hbar \frac{\mathrm{d}A}{\mathrm{d}t} = [A, H] \tag{1.7}$$

如果一个物理量满足 $[A, H] = 0$，则就有 $\frac{\mathrm{d}A}{\mathrm{d}t}=0$，它是守恒量。

可以证明，若 $A, B$ 均满足 (1.7) 方程，则 $\lambda A, A+B, AB$ 也满足该方程，进而 $[A, B]$ 也满足该方程。

### 对易关系
现在考虑经典系统的哈密顿量：

$$H = \frac{P^2}{2m_e} + V(X) \tag{1.8}$$

其中 $V(X)$ 使用 $V(x)$ 展开成幂级数的形式，就有 $[X, X^n] = X[X, X^{n-1}] + X[X, X^{n-1}]X = \cdots = 0$，故 $[X, V(X)] = 0$.

{% admonition(type="note", title="为什么是合理的？") %}
我同学的回答是，物理就是这样的。
{% end %}

如果我们要求量子系统与相应的经典系统相对应，即将哈密顿正则方程改造为：

$$\frac{\mathrm{d}X}{\mathrm{d}t}=\frac{P}{m_e},\quad \frac{\mathrm{d}P}{\mathrm{d}t} = -V'(X) \tag{1.9}$$

并且满足 (1.7) 方程，如果我们再假定一些良好的性质，则能得出 $[X, P] = i\hbar$，称为量子力学基本对易关系。

在多自由度情形，由于运动的不同自由度相互独立，我们有：

$$[X_a, P_b] = i\hbar\delta_{ab}, [X_a, X_b] = 0, [P_a, P_b] = 0 \tag{1.10}$$

称为海森堡代数。

在原书中，作者还对一维谐振子作了具体的计算，此文中略过。

### 新记号
最后，让我们换一个写法，记：

$$A_{mn} = \braket{m | \hat{A} | n} \tag{1.11}$$

那么就有 $\braket{m | \hat{A}^\dagger | n} = \braket{m | \hat{A} | n}^\ast$ 及 $\braket{m | n} = \braket{m | \mathbf{1} | n} = \delta_{mn}$.

不严格地说，可以把左态矢 $\bra{m}$ 看作行向量，把右态矢 $\ket{n}$ 看作列向量。

## Stern-Gerlach 实验
### 量子力学基本原理
我们现在讨论一般的可能性集（而非氢原子电子的定态）。

让我们考虑 Feynman 改编的 Stern-Gerlach 实验。让一束非极化原子（每个原子总角动量取向完全随机）沿水平方向（记作 $y$ 方向）通过一个非均匀指向 $z$ 方向，且梯度也沿 $z$ 方向的磁场，它将分裂为分立的三束。

对此的解释是：原子角动量的 $z$ 分量 $J_z$ 有三种量子化的可能，不妨记作 $+\hbar, 0, -\hbar$. 我们称它们构成一个可确定区分可能性完备集，其中完备意为它们占据全部可能性。

考虑带挡板的装置 $S$ 将 $0$ 与 $-\hbar$ 都挡住，成为只允许 $+\hbar$ 可能通过的过滤器。那么从这个装置通出的原子继续通出一个相同装置的概率是百分之百。可以把这个结果记作：

$$\braket{+S | +S} = \braket{+S | \mathbf{1} | +S} = 1 \tag{2.1}$$

另外的挡板情况也同理，即：

$$\braket{iS | jS} = \delta_{ij} \tag{2.2}$$

现在让原子通过 $+S$ 过滤器后再通过一个 $S$ 旋转了适当角度的版本，记作 $T$. 考虑不同的挡板装法 $+T, 0T, -T$, 实验发现 $\braket{+T | +S}, \braket{0T | +S}, \braket{-T | +S}$ 均不为零。进一步地，实验归纳出 Born 定则：

$$P(iS \to jT) = |\braket{jT | iS}|^2 \tag{2.3}$$

一个问题是，如果我们让一束原子先通过 $+S$, 再通过 $0T$, 是否就可以同时确定射出粒子的 $S$ 可能性与 $T$ 可能性了呢？答案是否定的。尽管 $T$ 可能性一定是 $0T$, 原子再通过 $+S, 0S, -S$ 后通出的可能性均非零。也就是说，一旦跃迁到 $0T$ 可能性，原来 $+S$ 可能性的信息就丢失了。

不仅如此，我们还无法区分原子是处于三种 $S$ 可能性之一还是三种 $T$ 可能性之一。为了区分处于哪种可能性，我们需要让原子通过某个过滤器，但是由此判断出的状态可能是由另一种跃迁过来的。

{% admonition(type="note", title="想法") %}
或许这个性质就是对应到[量子信息](@/posts/quantum_information_1.md)中的在特定的基下观测？
{% end %}

推广到一般情形，一个量子系统的两个不同的可确定区分可能性完备集是指标集 $\mathcal{I}, \mathcal{J}$ 满足存在 $(m) \in \mathcal{I}, (n') \in \mathcal{J}$ 使得 $\braket{n' | m} \neq 0, 1$.

---

我们继续这个实验，在过滤器 $S$, 无挡板的装置 $T$ 之后添加一个另外转过某个角度的过滤器 $R$. 实验发现中间的 $T$ 装置对结果没有影响，也即：

$$\braket{jR | iS} = \sum_k \braket{jR | kT} \braket{kT | iS} \tag{2.4}$$

我们记左侧 $\psi = \braket{jR | iS}$, 右侧三项分别为 $\phi_+, \phi_0, \phi_-$, 将发现：

$$P(iS \to jR) = |\phi_+ + \phi_0 + \phi_-|^2 > |\phi_+|^2 + |\phi_0|^2 + |\phi_-|^2 \tag{2.5}$$

这是量子物理不同于经典物理的地方。并不能说：要么经过中间可能性 $+T$, 要么经过 $0T$, 要么经过 $-T$. 我们并没有对中间可能性进行观测。

如果我们在装置 $T$ 中对于分立的每一束都添加探测器，使得每通过一个原子有且仅有一盏灯会亮，情况就变得不同。此时确实满足 $P(iS \to jR) = |\phi_+|^2 + |\phi_0|^2 + |\phi_-|^2$.

{% admonition(type="note", title="性质推导") %}
考虑式子 $1 = \braket{i | i} = \sum_{j' \in \mathcal{J}} \braket{i | j'} \braket{j' | i}$ 及 $1 = \sum_{j' \in \mathcal{J}} |\braket{j' | i}|^2 = \sum_{j' \in \mathcal{J}} \braket{j' | i}^\ast \braket{j' | i}$. 它们同时成立的最简单条件是：

$$\braket{i | j'} = \braket{j' | i}^\ast$$
{% end %}

从式 (2.4) 的形式易见和矩阵乘法是一致的。我们可以让 $T$ 是任意的装置 $A$, 记作 $\braket{jR | \hat{A} | iS}$, 就有：

$$\braket{jR | \hat{A}\hat{B} | iS} = \sum_k \braket{jR | \hat{A} | k} \braket{k | \hat{B} | iS} \tag{2.6}$$

对一个可确定区分可能性完备集，人们常常把 $\braket{j | \hat{A} | i}$ 排成：

$$
\begin{bmatrix}
\braket{1 | \hat{A} | 1} & \braket{1 | \hat{A} | 2} & \cdots \cr
\braket{2 | \hat{A} | 1} & \braket{2 | \hat{A} | 2} & \cdots \cr
\vdots & \vdots & \ddots
\end{bmatrix} \tag{2.7}
$$

这时候称可能性完备集 $\mathcal{L}$ 为 $\mathcal{L}$ 表象，上述矩阵为物理量 $A$ 在 $\mathcal{L}$ 表象中的矩阵表示。

能量表象存在一定的特殊性：哈密顿量在其中的表示矩阵为对角矩阵，即：

$$\braket{E_m | \hat{H} | E_n} = \delta_{mn}E_n \tag{2.8}$$

推广的海森堡运动方程是：

$$\braket{j' | i\hbar \frac{\mathrm{d}\hat{A}}{\mathrm{d}t} | i} = \braket{j' | [\hat{A}, \hat{H}] | i} \tag{2.9}$$

### 态和算符
考虑进一步简化的 Stern-Gerlach 实验，假设原子只分成两束（角动量由），在磁场指向 $z$ 轴时两种可能记作 $S = \set{\uparrow, \downarrow}$, 指向 $x$ 轴时两种可能记作 $T = \set{\rightarrow, \leftarrow}$. 实验测得 $|\braket{\rightarrow | \uparrow}|^2 = |\braket{\leftarrow | \uparrow}|^2 = |\braket{\rightarrow | \downarrow}|^2 = |\braket{\leftarrow | \downarrow}|^2 = \frac{1}{2}$. 反之亦然。

不妨选取为：

$$
\begin{bmatrix}
\braket{\uparrow | \rightarrow} & \braket{\uparrow | \leftarrow} \cr
\braket{\downarrow | \rightarrow} & \braket{\downarrow | \leftarrow}
\end{bmatrix}
= \frac{1}{\sqrt{2}}
\begin{bmatrix}
1 & 1 \cr
1 & -1
\end{bmatrix}
\tag{2.10}
$$

我们会得到 $\braket{\cdots | \uparrow} = \frac{1}{\sqrt{2}} \braket{\cdots | \rightarrow} + \frac{1}{\sqrt{2}} \braket{\cdots | \leftarrow}$. 一个自然的想法是写成 $\ket{\uparrow} = \frac{1}{\sqrt{2}} \ket{\rightarrow} + \frac{1}{\sqrt{2}} \ket{\leftarrow}$. 这可以解读成引入了某种**叠加原理**。

这种可以线性叠加的东西就是矢量，称为**态矢量**。构成的矢量空间称为希尔伯特空间（因为支持内积），一般来说可以是任意维的，甚至无穷维的。一个量子态对应的是希尔伯特空间中的一条复直线。

作为可确定区分性的要求，有：

$$\braket{\psi | \psi} = 1 \tag{2.11}$$

称为归一化条件。

容易推导出一些结论，如对一个初态可能性 $\ket{\psi} = \sum_i a_i \ket{i}$ 其中 $a_i = \braket{i | \psi}$, 若将它作为一个末态可能性则有 $\bra{\psi} = \sum_i \braket{\psi | i} \bra{i} = \sum_i a_i^\ast \bra{i}$. 从 $\psi$ 到 $\phi $ 的跃迁幅是一种乘法，即内积 $\braket{\phi | \psi}$.

如果两个态矢量满足 $\braket{\phi | \psi} = 0$ 则称它们**正交**。可确定区分可能性完备集 $\mathcal{L}$ 需要满足的就是 $\braket{i | j} = \delta_{ij}$ **正交归一性**。

---

考察初态可能性 $\psi$ 经物理量 $A$ 作用的结果，由于 $\braket{\cdots | A | \psi}$ 可以等效地由 $\braket{\cdots | \phi}$ 给出，可以写方程 $\ket{A | \psi} = \ket{\phi}$. 所以我们说量子力学中每一个物理量对应一个**线性算符**。

式子 $\ket{u}\bra{v}$ 可以视作一个算符（其运算规则是自然的）。

### 绘景
我们回顾海森堡运动方程是 (1.7) 式，量子力学基本对易关系是 $[X, P] = i\hbar$. 运动方程有一个形式上的通解：

$$A(t) = e^{iHt/\hbar} A_0 e^{-iHt/\hbar} \tag{2.12}$$

其中算符放在指数上是指：

$$e^B = 1 + \frac{1}{1!}B + \frac{1}{2!}B^2 + \frac{1}{3!}B^3 + \cdots \tag{2.13}$$

读者可自行验证 $e^Be^{-B} = 1$ 及 $i\hbar \frac{\mathrm{d}A}{\mathrm{d}t} = [A, H]$.

到现在为止我们都认为量子态不随时间演化（随时间演化的是代表物理量的算符），这种观点称为海森堡绘景；一种等价的观点是物理量算符不随时间演化，而量子态随时间演化，称为薛定谔绘景。从海森堡绘景过渡到薛定谔绘景可通过：

$$\braket{\phi | A(t) | \psi} = \braket{\phi | e^{iHt/\hbar} A_0 e^{-iHt/\hbar} | \psi} = \braket{\phi(t) | A_0 | \psi(t)} \tag{2.14}$$

其中 $\ket{\psi(t)} = e^{-iHt/\hbar} \ket{\psi}, \ket{\phi(t)} = e^{-iHt/\hbar} \ket{\phi}$. 注意 $(e^{-iHt/\hbar})^\dagger = e^{iHt/\hbar}$. 对此式对时间求导得到薛定谔方程：

$$i\hbar \frac{\mathrm{d}}{\mathrm{d}t} \ket{\psi(t)} = H \ket{\psi(t)} \tag{2.15}$$

在薛定谔绘景中，量子力学基本对易关系是 $[X_0, P_0] = i\hbar$.

---

此处省略一些与[量子信息笔记](@/posts/quantum_information_1.md)中重复的内容。

---

如果一个态矢量 $\ket{u_n}$ 满足 $A \ket{u_n} = \lambda_n \ket{u_n}$ 则称它是算符 $A$ 的本征值，这个方程是算符 $A$ 的本征方程。

哈密顿方程的本征方程叫做定态薛定谔方程，之后会看到其本征态就是能量有确定值的定态，因此定态薛定谔方程可以写成：

$$H \ket{E_n} = E_n \ket{E_n} \tag{2.16}$$

易见厄米算符的本征值必为实数。又 $\lambda_j \braket{i | j} = \lambda_i^\ast \braket{i | j}$ 推出 $\lambda_i \neq \lambda_j$ 时两个本征态正交。而 $\lambda_i = \lambda_j$ 时，称两个本征态**简并**。我们可以考虑所有本征态张成的子空间，然后从中选取正交的基。最终可以使得所有的本征态两两正交。如果加上归一化条件，就有：

$$\braket{i | j} = \delta_{ij} \tag{2.17}$$

对一个厄米算符 $A$, 相应的可能性完备集是可确定区分的，这些可能性称为 $A$ 的**本征可能性**。

可以证明两个物理量 $A, B$ 的值可以同时确定，当且仅当两个算符可对易。
