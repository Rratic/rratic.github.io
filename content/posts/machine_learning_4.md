+++
title = "机器学习（四）：神经网络"
draft = true

[extra]
math = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["计算机"]
+++

对应于[教师 Lecture Notes](https://leiwu0.github.io/courses/mi2ml/index.html) 的 Lecture 7, 8.

## 架构设计
### 全连接神经网络
1943 年在研究神经元时 McCulloch 与 Pitts 提出了神经元的数学模型，即 $f(\mathbf x) = \mathbf 1(\mathbf w \cdot \mathbf x + b > 0)$；1957 年 Rosenblatt 提出神经元学习参数的方法（当时还没有梯度下降）$w \leftarrow w + \eta (y - \hat y) x$，并制作了第一台神经网络的硬件 Mark I Perceptron，当时登上了纽约时报。

Minsky 与 Papert 的 *The Perceptrons* 一书指出，Perceptron 无法表达异或（因为是线性的），带来第一次“AI 寒冬”。

1965 年 Alexey Ivakhnenko 与 Valentin Lapa 开发了一个多层的版本 Multilayer Perceptron (MLP)，如在二维版中有一个隐藏层，中间经过一个非线性的激活函数。

全连接神经网络即相邻层之间全连接，读者可以在 [Tensorflow Playground](https://playground.tensorflow.org/) 试玩。通常记作：

$$
f(\mathbf x; \theta) = \mathcal A^{(L)} \circ \sigma \circ \cdots \circ \sigma \circ \mathcal A^{(1)}(\mathbf x) \\\\
A^{(l)}(\mathbf z) = W^l \mathbf z + b^l
$$

| 类型 | 激活函数名称 | 说明 |
| :---: | :--------: | :---: |
| Saturating | Sigmoid | $1 / (1 + e^{-x})$ |
| Saturating | Tanh | $(e^x - e^{-x}) / (e^x + e^{-x})$ |
| Non-saturating | ReLU | $\max(0, x)$ |
| Non-saturating | Leaky ReLU | $\max(ax, x)$，$a$ 是取定较小值 |
| Non-saturating | Parametric ReLU | $\max(ax, x)$，$a$ 可学习 |
| Non-saturating | SoftPlus | $\ln(1 + e^{-x})$ |
| Self-gated | GELU | $x\Phi(x)$ |
| Self-gated | SiLU | $x\sigma_{\text{sigmoid}}(\beta x)$ |

{% <theorem title="Universal Approximation Theorem"> %}
设 $\Omega$ 为 $\R^d$ 中紧集，设 $\sigma$ 在 $t \to +\infty$ 为 $1$，在 $t \to -\infty$ 为 $0$，对任意 $f \in C(\Omega)$ 及 $\varepsilon > 0$，存在两层神经网络 $f_m(\mathbf x; \theta) = \sum_{j=1}^m a_j \sigma(\mathbf b_j^\top \mathbf x + c_j)$ 使得：

$$\sup_{x \in \Omega} |f(\mathbf x) - f_m(\mathbf x)| \leq \varepsilon$$
{% </theorem> %}

Cybenko 在 1989 年证明了这个定理，展示了神经网络的通用性，使得神经网络重新回温。

### 卷积神经网络
1969 年 Kunihiko Fukushima 提出了第一个深层 ReLU 卷积神经网络，但其 filter 不可学习；1989 年 Yann LeCun 提出可学习的卷积 filter，后来其识别手写数字的 7 层神经网络 LeNet-5 被银行采用。

2012 年 AlexNet 在 ImageNet 挑战中获胜，激发了深度学习的时代；2015 年的 ResNet 使得数百层的卷积神经网络训练能够实现。

卷积神经网络利用了数据的空间结构（如图像、音频等）。其中使用的是卷积：

$$\mathcal A^{(l)}(\mathbf z) = \mathbf z \ast w^l + b^l$$

以二维卷积为例，对 $x \in \R^{d \times d}$ 与 $w \in \R^{k \times k}$，有 $x \ast w \in \R^{(n-k+1) \times (n-k+1)}$，其中：

$$(x \ast w) _{s, t} = \sum _{i, j=1}^k x _{s+i, t+j} w _{i, j}$$

通常使用较小的 $k$，如 3、5，通过叠加很多层来把握全局的结构。以下 $k = 3$ 的例子可以把握轮廓：

$$w = \begin{pmatrix} -1 & -1 & -1 \cr -1 & 8 & -1 \cr -1 & -1 & -1 \end{pmatrix}$$

实际中可能进行：Padding：为了让输出与输入的维数相同，周围填充 $0$；Pooling / down sampling：减少采样。

LeNet-5 采用的数据集 MNIST 是一族 $28 \times 28$ 的灰度图。其使用 $5 \times 5$ 的 filter，先由卷积得到 $6$ 个 $28 \times 28$，然后 pooling 得到 $6$ 个 $14 \times 14$；再由卷积得到 $16$ 个 $10 \times 10$，然后 pooling 得到 $16$ 个 $5 \times 5$，此后进入分类器。

AlexNet 成功系统性组合了一系列技术：加深加宽网络结构、使用 GPU 加速（Jürgen Schmidhuber 的团队在 2011 做了类似的事，但当时在一个小数据集上训练，效果不明显，影响力不大）、采用 ReLU、引入 Dropout 机制、使用 Max Pooling 替代 Average Pooling 等。

ResNets 引入了残差连接。

### 循环神经网络
循环神经网络希望捕捉远距离的依赖性。状态空间模型（State-Space Models, SSMs）认为直接建模 $H_t(x_1, \dots, x_t)$ 不合理，而应该用某种隐变量 $S_t \in \R^m$ 来总结过去的相关信息，状态更新写为 $S_t = G_{\theta}(x_t, S_{t-1})$，输出写为 $y_t = O_{\theta}(S_t)$. 具体来说，vanilla RNN 取：

$$
h_t = \tanh (W_{hh} h_{t-1} + W_{hx} x_t + b_h) \\\\
y_t = W_{yh} h_t + b_y
$$

状态空间模型有一个普遍的问题：状态空间只有固定维，写入新的信息必然也会损失旧的信息；同时在梯度传播（gradient propagation）上 $\partial h_t / \partial h_{t-k} \approx (W_{hh}')^k$，梯度容易消失或爆炸。

Long Short-Term Memory (LSTM) 提出，将记忆分成两种，像 $h$ 一样由矩阵乘法得到的是短期记忆。

Gate update 是（分别称 forget gate、input gate、output gate）：

$$\begin{pmatrix} f_t \\\\ i_t \\\\ o_t \end{pmatrix} = \mathrm{sigmoid} \begin{pmatrix} W_f x_t + U_f h_{t-1} + b_f \\\\ W_i x_t + U_i h_{t-1} + b_i \\\\ W_o x_t + U_o h_{t-1} + b_o \end{pmatrix}$$

Memory update 是（$\odot$ 是 Hadamard 积）：

$$
c_t = f_t \odot c_{t-1} + i_t \odot \tanh (W_c x_t + U_c h_{t-1} + b_c) \\\\
h_t = o_t \odot c_t
$$

gating 和 memory 的概念深刻影响了之后的模型。

然而即使有了 gating，状态空间模型在直观上仍然对长程依赖性有困难。为了解决有限状态的问题，产生了 Encoder-Decoder 架构。

Geometric Deep Learning 考虑对称不变的神经网络。对群 $G$，称 $f: \mathcal X^d \to \R$ 是 $G$-不变的，如果 $f(\sigma x) = f(x)$；称 $F: \mathcal X^d \to \mathcal X^d$ 是 $G$-等变的，如果 $F(\sigma x) = \sigma F(x)$. 考虑点云（空间中的点集），那么分类函数就应当有置换不变性。

## 训练算法
### 计算梯度
[前向传播、反向传播](@/posts/neural_networks_1.md)略去。对深度 $L$、宽度 $m$、批次大小 $B$ 的网络，计算成本是 $O(Bm^2L)$，通过并行化可以降低对 $B, m$ 的依赖。
