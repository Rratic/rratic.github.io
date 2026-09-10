+++
title = "【草稿】机器学习数学导引"
draft = true

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["计算机"]
+++

## 导论
### 数学理解
我们来从数学角度理解机器学习中的监督学习。我们希望知道一个目标函数：

$$f^\ast: X \to Y$$

我们通过数据和先验知识（如目标函数光滑）去找函数 $\hat f$ 使得：

$$\hat f \approx f^\ast$$

考虑训练集 $S = \set{(x_i, y_i)}$，定义：

$$\underbrace{y_i} _{\text{label}} = \underbrace{f(x_i)} _{\text{target function}} + \underbrace{\varepsilon_i} _{\text{noise}}$$

---

典型的 $y_i$ 有两种：实值，此时称模型为回归（regression）；类型，此时称模型为分类（classification）。

{% admonition(type="note", title="根据需要去建模") %}
考虑预测同学的成绩，虽然可以表达成实值，但我们并不关心小数点两位那么精确的值，同时也不认为我们能做到如此精确的估计。考虑将成绩分为 $1, \dots, 10$ 十类，实际中建模成分类任务可能会比直接做回归效果更好。
{% end %}

考虑学习任务的难度可能来自哪里。这一方面会受到 linear vs nonlinear 的影响；另一方面受到维数的影响。一维数据可以肉眼看出来，但图像识别（早期重要的体现机器学习价值的例子）的输入可能是 $224 \times 224 \times 3$ 维的，无法用传统统计方法处理。由此看维数是重要的区分传统方法和机器学习的依据。


### 方法论
Step 1 是选择一个带参数模型族，如：

$$f_\theta: X \to Y \qquad (\theta \in \R^m)$$

其可能是：
- $b + w^\top x \qquad (\theta = (b, w))$
- $\sum b_j \varphi_j(x)$
- 神经网络

---

Step 2.0 是建立一个优化问题。定义**经验风险**（empirical risk）：

$$\hat R(\theta) = \frac 1 n \sum l(f(x_i), y_i)$$

$$\min_\theta \hat R(\theta)$$

我们通常还会加上正则化项：

$$J(\theta) = \hat R(\theta) + \lambda \underbrace{R(f_\theta)}_{\text{penalty}}$$

$$\min_\theta J(\theta)$$

Step 2.1 是设计优化器。如梯度下降迭代：

$$\theta_{k+1} = \theta_k - \eta \nabla J(\theta_k)$$

在公司中，Step 1 会对应一个架构/模型组，Step 2 对应一个优化器组。这两个组有时合并，因为取决于算力，架构设计时需要让收敛更快。

---

对公司来说还有 Step 3，对应评测部门。记（用 $\leftarrow$ 表示赋值，用 $\hat ~$ 表示从数据学到的）：

$$\hat f \leftarrow \argmin_{f \in \mathcal H_m} \hat R(f)$$

我们定义**期望损失**（expected risk）：

$$R(f) = \mathbb E_{x \sim \rho}[l(f(x), f^\ast(x))]$$

这里我们假设存在一个客观存在的分布 $\rho$，并有 $x_i \stackrel {\text {iid}} \sim \rho$.

取测试集 $S_{\text{test}}$ 满足 $\tilde x_j \stackrel {\text {iid}} \sim \rho$，定义 test error 是：

$$\hat R_{\text{test}}(f) = \frac 1 {n_{\text{test}}} \sum l(f(\tilde x_j, \tilde y_j))$$

这就是 Monte Carlo 近似。若记 $z_i = l(f(\tilde x_i), \tilde y_i)$，并令 $\mu = \mathbb E[z]$，则独立同分布样本的平均值满足：

$$
\begin{align*}
&\mathbb E(R(f) - \hat R(f))^2 \cr
=& \mathbb E\left(\frac 1 N \sum (z_j - \mu)\right)^2 \cr
=& \frac 1 {N^2} \sum_{i \neq j} \mathbb E[(z_i - \mu)(z_j - \mu)] + \frac 1 {N^2} \sum \mathbb E[(z_i - \mu)^2] \cr
=& \frac 1 {N^2} \sum_{i \neq j} \mathbb E[z_i - \mu] \mathbb E[z_j - \mu] + \frac 1 {N^2} \sum \mathbb E[(z_i - \mu)^2] \cr
=& \frac 1 {N^2} \sum \mathbb E[(z_i - \mu)^2] \cr
=& \frac {\mathrm{Var}(z)} N
\end{align*}
$$

---

在整个过程中，Step 1 与 2 门槛最高，但核心是数据，清洗数据带来的效果可能比优化架构有用很多。

### 误差分解
记 $\mathcal H_m$ 内最优：

$$f_H^\ast \leftarrow \argmin_{f \in \mathcal H_m} R(f)$$

则可以作分解：

$$R(\hat f) - R(f^\ast) = \underbrace{R(\hat f) - R(f_H^\ast)} _{\text{estimation error}} + \underbrace{R(f_H^\ast) - R(f^\ast)} _{\text{approximation error}}$$

这里估计误差来自于有限训练数据和优化过程，记作 $E(n, m)$；近似误差来自空间 $\mathcal H_m$ 的表达能力，记作 $A(m)$；总误差记为 $\mathcal E(n, m)$.

增加数据主要有助于降低估计误差，而扩大模型的表达能力主要有助于降低近似误差。机器学习系统需要在这两者之间取得平衡。
