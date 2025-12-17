+++
title = "《初识经典力学》笔记：哈密顿量"
description = "一般形式的哈密顿量和常见定律的导出。"
date = 2025-10-04
updated = 2025-11-26

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "物理", "经典力学"]
+++

前置知识
- 数学分析（多元微分即可）

参考的是[《初识经典力学》](https://chaoli.club/index.php/10248)，这是一个创新性的，跳过拉格朗日力学直接从哈密顿力学开始的经典力学讲义。

类同于物理的一贯特色，我们总是假设事物有*足够好*的性质。

## 状态与演化
我们假定系统默认指封闭系统，观察者对系统的观察和测量不会干扰系统本身。

称一个经典力学系统所有可能的状态构成的空间为**状态空间/相空间**，记作 $\Gamma$，其中一个状态（空间中一个点）记作 $X$，本文中记我们讨论的系统在 $t$ 时刻的状态为 $X_t$，这个符号也表示整个函数 $X_t: \mathrm{Time}\to \Gamma$，按照上下文判断含义。

经典力学系统满足决定论（时间上严格的因果关系），即形式上我们认为存在 $f$ 把一个状态映到一个切矢，使得：

$$\frac{\mathrm{d}X_t}{\mathrm{d}t} = f(X_t) \tag{1.1}$$

我们不能只用位置这一个变量来刻画粒子的状态，在现代物理中，额外引入动量（请忘记它和所谓速度 $\dot{x_i}$ 的关系）。

为了量化位置 $x_i$ 和动量 $p_i$，我们选取一个参照物并构建坐标系，合称**参考系**，记作 $K$.

在普通的 $\mathbb{E}^3$ 空间中，我们希望建立的是笛卡尔坐标系，此时 $\dim \Gamma = 2n = 6N$，其中有 $N$ 个粒子，称 $n=3N$ 为其自由度数目。

## 原理
我们希望选取适当的参考系，使得物理在时间、空间上是等价的。**伽利略相对性原理**提出，所有惯性系在物理上都等价。

我们希望有熟知的不变量。**能量守恒定律**指出，封闭系统随时间演化过程中，有一个名为能量的物理量保持不变。可以参考 Feynman 关于“积木”的论述。

经典力学系统涉及的只有动能和势能。

经典力学系统的总能量是系统状态的函数，通常记作 $H(X)$，称作哈密顿函数/**哈密顿量**。

我们知道

$$\frac{\mathrm{d}H(X_t)}{\mathrm{d}t} = 0 \tag{2.1}$$

这是很强的条件。

不同惯性系中测到的系统总能量可能并不相同，只不过演化规律一样。

能量具有以下性质
* 可加性：我们先取定标度消除 $H'=cH$ 的不确定性，再规定所有粒子都相距很远且每个粒子都静止不动时能量为 0（在狭义相对论下会有所不同）。若两部分之间没有相互作用，我们可以写 $H(X)=H_A(X_A)+H_B(X_B)$
* 有下界：我们希望如此；不过可以有上界

## 对称性
哈密顿量需要满足以下对称性
* 时间、空间平移对称性
* 空间旋转对称性
* 时间、空间反演对称性（时间反演是指 $t\mapsto -t$）

其中时间平移对称性意味着哈密顿量并不显式依赖于 $t$，空间平移对称性意味着 $\sum_{i=1}^N\frac{\partial H}{\partial x_i} = 0$，而空间旋转、反演对称性意味着 $H$ 将形如 $x_i\cdot x_j, x_i\cdot p_j, p_i\cdot p_j$ 的组合。

因此，对单个粒子，存在 $H=T(p^2)$，称 $T$ 为该粒子的动能。

多粒子系统的哈密顿量有一般形式（如果我们假定相互作用与动量无关，有关的情况并非不存在）

$$H = \sum_{i=1}^N T_i(p_i^2) + V(x_1, x_2 \cdots x_N) \tag{3.1}$$

为了确定 $V$ 的形式，需要考虑另一种对称性，即伽利略相对性原理，这将涉及伽利略变换（狭义相对论下是 Lorentz 变换）。

现在我们猜测演化定律，即 (1.1) 中 $f$ 的形式。从单自由度情形开始，考虑：

$$0 = \frac{\mathrm{d}H(X_t)}{\mathrm{d}t} = \sum_{\text{particles}} \left[\frac{\partial H}{\partial x}\frac{\mathrm{d}x}{\mathrm{d}t} + \frac{\partial H}{\partial p}\frac{\mathrm{d}p}{\mathrm{d}t}\right] \tag{3.2}$$

我们合理地猜测有下式：

$$\left(\frac{\mathrm{d}x}{\mathrm{d}t}, \frac{\mathrm{d}p}{\mathrm{d}t}\right) = k(x, p)\left(\frac{\partial H}{\partial p}, -\frac{\partial H}{\partial x}\right) \tag{3.3}$$

通过重新标度时间我们可以消去 $k$，然后我们自然地将它推广成 $n$ 自由度的**哈密顿正则方程**：

$$\frac{\mathrm{d}x^\mu}{\mathrm{d}t} = \frac{\partial H}{\partial p_\mu},\ \frac{\mathrm{d}p_\mu}{\mathrm{d}t} = -\frac{\partial H}{\partial x^\mu} \tag{3.4}$$

这里位置的分量用上标表示，是一种物理的传统记法。

这个过程有很多猜测成分，但幸运的是迄今为止的实验都验证了哈密顿正则方程的成立。

我们把相空间*看作一个空间*，$X_t$ 是以时间为参数的曲线。由微分方程解的唯一性，过一点恰有一条曲线，因此不同曲线不相交。

称这一性质为**满足决定论**。

有一个一般的 Noether 定理说，经典力学系统任何一个连续对称性都必然对应着一条守恒定律。

我们暂时不去讨论它，而是仅限于一些例子。
- 令总动量 $P = \sum_{i=1}^N p_i$，由空间平移不变性及 (1) 知 $\frac{\mathrm{d}P}{\mathrm{d}t} = 0$
- 在 $\mathbb{E}^3$ 中，可以使用叉积来表达，然后我们可以取一阶小量。令总角动量 $L = \sum_{i=1}^N (x_i\times p_i)$，有 $\frac{\mathrm{d}L}{\mathrm{d}t} = 0$

## 导出
对单个粒子，$H=T(p^2)$，有

$$\frac{\mathrm{d}x}{\mathrm{d}t} = 2T'(p^2)p,\ \frac{\mathrm{d}p}{\mathrm{d}t} = 0 \tag{4.1}$$

从而速度 $v = \frac{\mathrm{d}x}{\mathrm{d}t}$ 为常矢量，得到惯性定律/牛顿第一定律。

物体运动的原因是式 (3.1) 中的 $V$ 项。我们称第 $i$ 个粒子受到的其它粒子的作用力为

$$F_i = -\frac{\partial V}{\partial x_i} \tag{4.2}$$

从而 $\frac{\mathrm{d}p_i}{\mathrm{d}t} = F_i$，这几乎是牛顿第二定律。

由空间平移对称性，$\sum_{i=1}^N F_i = 0$，若把系统分为两部分，就得到牛顿第三定律。

我们定义粒子相对原点的力矩为 $M_i = x_i \times F_i$，有 $\frac{\mathrm{d}L_i}{\mathrm{d}t} = M_i$

若我们假定两体间势能 $V$ 与粒子动量无关，可进一步说明两体相互作用力必沿着两粒子连线方向。

我们来看一些两体相互作用势：
- 核外电子绕原子核运动，存在自旋轨道耦合项，依赖于粒子自旋
- Lennard-Jones 势，即电中性原子/分子间势的模型
	$$V_{LJ} = 4\epsilon \left[\left(\frac{\delta}{r}\right)^{12} - \left(\frac{\delta}{r}\right)^{6}\right]$$
- 正-反两个夸克间的相互作用势
	$$V_{q\bar{q}}(r) = -\frac{\alpha_s}{r}+\sigma r$$
此处第一项是库伦势，第二项是禁闭势（距离越远相互作用势越大）

对非封闭系统，可以将其看成大的封闭系统的子系统。可以导出功能原理、动量定理。

此外，可以导出伽利略变换。

## 相空间路径
### 位力定理
如果力学系统的相互作用势能函数是各粒子位置矢量的齐次函数，那么变换 $x_i \to \alpha x_i$ 对应的相空间路径将具有力学相似性。

当力学系统作周期运动或有限运动时，对任意物理量 $A(t)$，定义时间平均值 $\bar{A}$，对周期为 $T$ 的周期运动，是指

$$\frac{1}{T}\int_0^T A(t)\mathrm{d}t \tag{5.1}$$

对有限运动，定义为上式对 $T$ 取极限。

考察 $\sigma(t) = \frac{\mathrm{d}}{\mathrm{d}t} \sum p_\mu x^\mu$，有 $\bar{\sigma} = 0$，从而推出**位力定理**：

$$\overline{\sum x^\mu \frac{\partial H}{\partial x^\mu}} = \overline{\sum p_\mu \frac{\partial H}{\partial p_\mu}} \tag{5.2}$$

如果可以按 (3.1) 式将 $H$ 分解为动能与势能之和 $T + V$，并且 $V$ 是齐次度为 $\lambda$ 的齐次函数，就有

$$\bar{T} = \frac{n}{n+2} E,\ \bar{V} = \frac{2}{n+2} E \tag{5.3}$$

这给出的一个结论是，在万有引力情形下，系统作有限运动必须系统总能量 $E$ 为负。

### 相流
作为相空间坐标，$x$ 与 $p$ 的地位是平等的，不妨记 $X^1=x, X^2=p$，则哈密顿正则方程可以写成：

$$\left(\frac{\mathrm{d}X^1}{\mathrm{d}t}, \frac{\mathrm{d}X^2}{\mathrm{d}t}\right) = \left(\frac{\partial H}{\partial X^2}(X), -\frac{\partial H}{\partial X^1}(X)\right) \tag{6.1}$$

我们定义相空间速度是：

$$V^\alpha = \frac{\mathrm{d}X^\alpha}{\mathrm{d}t} \tag{6.2}$$

现在整个相空间上有一个速度场 $V_\alpha(X)$，它是不依赖于时间的。人们把它看作一种稳定流动的相空间流体，称作**相流**。

刘维尔定理指出，这种流体是不可压缩的，即一个区域 $D_0$ 演化到 $D_t$ 后仍有：

$$\mathrm{Vol}(D_0) = \mathrm{Vol}(D_t) \tag{6.3}$$
