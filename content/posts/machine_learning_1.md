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

{% <note title="根据需要去建模"> %}
考虑预测同学的成绩，虽然可以表达成实值，但我们并不关心小数点两位那么精确的值，同时也不认为我们能做到如此精确的估计。考虑将成绩分为 $1, \dots, 10$ 十类，实际中建模成分类任务可能会比直接做回归效果更好。
{% </note> %}

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

$$J(\theta) = \hat R(\theta) + \lambda \underbrace{P(f_\theta)}_{\text{penalty}}$$

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
记假设空间 $\mathcal H_m = \set{f_\theta | \theta \in \R^m}$ 内最优：

$$f_H^\ast \leftarrow \argmin_{f \in \mathcal H_m} R(f)$$

则可以作分解（$R(f^\ast) = 0$）：

$$R(\hat f) - R(f^\ast) = \underbrace{R(\hat f) - R(f_H^\ast)} _{\text{estimation error}} + \underbrace{R(f_H^\ast) - R(f^\ast)} _{\text{approximation error}}$$

这里估计误差来自于有限训练数据和优化过程，记作 $E_H(n, m)$，一般来说会随 $m$ 增大（表达能力增强后有更大的风险得到复杂而非真实的函数）；近似误差来自空间 $\mathcal H_m$ 的表达能力，记作 $A_H(m)$；总误差记为 $\mathcal E(n, m)$.

增加数据主要有助于降低估计误差，而扩大模型的表达能力主要有助于降低近似误差。朴素的看法认为误差会随 capacity $m$ 呈 U 型曲线，因此正则化作用是惩罚复杂性。现代深度学习会更复杂，如在 capacity 足够高时会出现二次下降。

### 成本
我们希望在 $\min_{m, n} R_H(m, n)$ 的同时让成本 $C_H(m, n) \leq B$.

不妨考察：

$$C_H(m, n) = n \cdot m \cdot \alpha_H$$

$$R_H(m, n) = \frac 1 {m^\alpha} + \frac 1 {n^\beta} \tag{deepmind scaling law}$$

$$\begin{cases} \min \frac 1 {m^\alpha} + \frac 1 {n^\beta} \cr m \cdot n \leq c \end{cases} \implies \begin{cases} m \sim C^{\frac \beta {\alpha+\beta}} \cr n \sim C^{\frac \alpha {\alpha+\beta}} \end{cases}$$

现代来看，我们可以设计 $R$，从而给出对应 scaling law 结果，可以预测增加若干预算能有多少优化。由此我们离开了古法的“炼丹”情景。

### 泛化
我们希望从有限的数据推广到无限的数据。考察：

$$
\begin{align*}
&\hat f(x) - f^\ast(x) \cr
=& \hat f(x) - \hat f(x_k) + \hat f(x_k) - f^\ast(x_k) + f^\ast(x_k) - f^\ast(x) \cr
\leq & \mathrm{Lip}(f) |x - x_k| + \mathrm{Lip}(f^\ast) |x - x_k| + \delta_k
\end{align*}
$$

假设数据在 $[0, 1]$ 上均匀，有 $\min_j |x - x_j| \sim \frac 1 n$；对 $X = [0, 1]^d$ 则（考察网格情况及使用 sphere packing argument）：

$$\min_j |x - x_j| \sim n^{-\frac 1 d} = Q_n$$

对 $s$ 阶可微的函数：

$$|\hat f(x) - f^\ast(x)| \sim n^{-\frac s d} = \varepsilon$$

此现象（维数灾难）等价于，高维单位球面上的向量几乎都是正交的。

假若希望 $\varepsilon \leq 0.1$，对 $d = 150000$ 就需要 $10^{150000}$ 的数据，这是不可接受的。因此，我们会利用一些数据的结构。

如果假设空间是分片常值函数（在实际中确有，如 K-nearest neighbour (KNN)），想要去拟合线性函数，则读者可验证也会造成维数灾难。

## 线性方法
### 模型
线性方法比较 robust，同时完全可解释、可计算，因此学习时不能当黑箱来看。

线性方法的假设空间即：

$$\mathcal H = \set{x \mapsto \beta^\top x + \beta_0 | \beta \in \R^d, \beta_0 \in \R}$$

有时我们会作简化，将它看成：

$$\underbrace{\begin{pmatrix}\beta^\top & \beta_0\end{pmatrix}}_\beta \underbrace{\begin{pmatrix}x \cr 1\end{pmatrix}}_x$$

$$\mathcal H = \set{x \mapsto \beta^\top x | \beta \in \R^d}$$

考察以下经典的 risk，注意使用 square loss 并不是天然的：这会带来解析解，在噪声 $\varepsilon_i \sim N(0, \sigma^2)$ 时适用，但在 $P(|\varepsilon_i| \geq R) \propto R^{-\beta}$ 这样尾部较大的情形可能不适用。

$$\hat R(\beta) = \frac 1 {2n} \lVert x\beta - y \rVert^2$$

这里 $x$ 是 $n \times d$ 的。容易解出：

$$
\begin{align*}
    &\min_\beta \frac 1 {2n} \lVert x\beta - y \rVert^2 \cr
    \iff& \frac 1 n x^\top (x\beta - y) = 0 \cr
    \iff& x^\top x\beta = x^\top y \cr
    \iff& \hat \beta_{\text{OLS}} = (x^\top x)^{-1} x^\top y
\end{align*}
$$

但实际中几乎不会直接用这个解。一方面需要 $x^\top x$ 可逆，这需要样本数大于维数。另一方面，这个解对标签噪声（label noise）和异常值敏感，因此需要改为：

$$\min_\beta \hat R(\beta) + \underbrace\lambda_{\text{hyper parameter}} r(\beta)$$

所谓“调参”，就是调整这里的 hyper parameter，这没有什么一般的方法，但可以参考一些原则：
- $n$ 变大，左边更接近真实，应当 $\lambda$ 变小
    - 对大模型来说假设有一个 scaling law $\lambda \propto n^{-r}$，先在小模型上拟合然后推广
- 准备一个 validation set，设 $\beta(\lambda)$ 是一个 $\lambda$ 对应的 $\argmax$，
    $$\lambda \leftarrow \argmin \frac 1 {|n_{\text{val}}|} \sum |x\beta(\lambda) - y|^2$$

### Ridge Regression
岭回归是指使用 L2 正则化：
$$\min_\beta \frac 1 n \lVert x\beta - y \rVert^2 + \lambda \lVert \beta \rVert^2$$

$$\hat \beta_\lambda = \left(\frac 1 n x^\top x + \lambda I_n\right)^{-1} \frac 1 n x^\top y$$

假设 $\varepsilon_i \sim N(0, \sigma^2)$，并令 $\Sigma = \frac 1 n x^\top x$ 及 $\Delta = \beta^\ast - \hat \beta_\lambda$，则：

$$\hat \beta_\lambda = (\Sigma + \lambda)^{-1} \Sigma \beta^\ast + (\Sigma + \lambda)^{-1} \frac 1 n x^\top \varepsilon$$

$$\Delta = \lambda (\Sigma + \lambda)^{-1} \beta^\ast - (\Sigma + \lambda)^{-1} \frac 1 n x^\top \varepsilon$$


$$
\begin{align*}
&\mathbb E \lVert \Delta \rVert^2 \cr
=& \lambda^2 \beta^\ast (\Sigma + \lambda)^{-2} \beta^\ast + \frac 1 {n^2} \mathbb E[\varepsilon^\top x (\Sigma + \lambda)^{-2} x^\top \varepsilon] \cr
=& \lambda^2 \beta^\ast (\Sigma + \lambda)^{-2} \beta^\ast + \frac{\sigma^2}{n} \mathrm{tr}[(\Sigma + \lambda)^{-2} \Sigma]
\end{align*}
$$

作特征值分解：

$$
\Sigma = \sum_{j=1}^d \lambda_j u_j u_j^\top \\\\
\beta^\ast = \sum_{j=1}^d \beta_j^\ast u_j
$$

$$\mathbb E \lVert \Delta \rVert^2 = \lambda^2 \sum_{j=1}^d \frac{|\beta_j^\ast|^2}{\lambda_j + \lambda} + \frac{\sigma^2}{n} \sum_{j=1}^d \frac{\lambda_j}{(\lambda_j + \lambda)^2}$$

假设各向同性 $\lambda_i = 1$，则算得：

$$\lambda_{\text{opt}} \propto \frac 1 n$$

另外可分析得绝大部分误差来自于 $\lambda_j \ll \lambda$ 的方向的噪声。这样看正则化可以降低该部分误差。

### LASSO
思考上面在使用 $\lambda \lVert \beta \rVert_2^q$ 时为什么选取 $q = 2$？一方面是为了更好算，另一方面是为了让量纲一致。

考虑 L0 正则化（相当于 sparsity）：

$$\lVert\beta\rVert_0 \coloneqq \\#\set{\beta_j \neq 0 | j \in [d]}$$

最终会有 $\lVert\beta\rVert_0 \ll d$，这使得结果有较好的可解释性。但 L0 正则化不连续且非凸，无法梯度下降，因此考虑用一般的 $p$ norm，由于 $p = 1$ 时函数才凸，考虑 L1 正则化。

一般用 L1 正则化的时候不会平方。原因不明，一方面可能因为实际中会把数据归一化，另一方面 L0 正则化是没有量纲的，我们本质上考虑的是 L0 而不是 $\lambda \lVert \beta \rVert_2^2$.

可解释是指对于人类可解释，如 $\beta = (1, 0, 0, 0)$ 误差 $0.02$，$\beta = (0.6, 0.15, 0.15, 0.1)$ 误差 $0.01$，则人类心智偏向于前者。也许对 AI 来说并不如此。

考察一下为什么会让 $\beta$ 稀疏：

$$S_\lambda(y) = \min_\beta \frac 1 2 (\beta - y)^2 + \lambda |\beta|$$

$$
S_\lambda(y) = \begin{cases}
    y + \lambda & y < -\lambda \cr
    y - \lambda & y > \lambda \cr
    0 & \text{otherwise}
\end{cases}
$$

因此，较小的参数会直接变为零。相反地，

$$\hat \beta_{\text{ridge}} = \frac{y}{\lambda + 1}$$

可参考如下几何直观。可见只需要让单位圆是尖的。实际中有时把 $p = 1$ 换成 $0.5, 0.6$ 会更好用。

![几何直观](/images/misc/2026_09_24.png)

### 压缩感知
Compressive sensing 是说，数据在某个 $d$ 维空间中，但我们的数据量 $n \ll d$. 考察：

$$y = x\beta^\ast, \quad x \in \R^{n \times d}$$

并要求 $\beta^\ast$ 是稀疏的。当 $\lVert\beta^\ast\rVert_0 = k$ 时称 $\beta^\ast$ 为 $k$-稀疏的。为了能够唯一地恢复信号，我们不妨要求：

$$\lVert x\beta - y\rVert = \lVert x(\beta - \beta^\ast)\rVert = \lVert\beta - \beta^\ast\rVert$$

这个在稀疏向量上保距的性质称为 Restricted Isometry Property (RIP). 我们有以下结论：

{% <theorem title="Candes-Tao (2006)"> %}
设 $X \in \R^{n\times p}, \beta_0 \in \R^p$，若 $X$ 满足 $2k$ 阶限制等距性质，即存在 $\delta_{2k} \in (0, 1)$，使得对所有 $2k$-稀疏向量 $\beta$ 有：

$$(1 - \delta_{2k}) \lVert\beta\rVert_2^2 \leq \lVert X\beta\rVert_2^2 \leq (1 + \delta_{2k}) \lVert\beta\rVert_2^2$$

且 $\delta_{2k} < \sqrt 2 - 1$；给定无噪声观测 $y = X\beta_0$，令 $\beta_1$ 为 $\ell_1$ 最小化问题的解：

$$\beta_1 = \argmin_{\beta \in \R^p}\lVert\beta\rVert_1 \quad \text{s.t.} \quad X\beta = y$$

则有下式成立，其中 $T_k$ 是 top-$k$ 稀疏近似，即保留绝对值最大的 $k$ 个分量，其余分量置零：

$$\lVert\beta_1 - \beta_0\rVert_2 \leq C_k \\, \lVert\beta_0 - T_k(\beta_0)\rVert_2,$$
{% </theorem> %}

### 核方法
将线性方法推广到非线性时一个方法是引入基函数：

$$f_\theta(x) = \theta^\top \varphi(x)$$

做机器学习的人进一步把它看成：

$$f_\theta(x) = \braket{\theta, \varphi(x)}$$

这里 $\varphi$ 是一个 feature map，将数据打到某个 Hilbert 空间 $\mathcal H$，

$$\varphi: X \to \underbrace{\mathcal H}_{\text{feature space}}$$

总体图景如下：

$$X \xrightarrow{\varphi} \mathcal H \xrightarrow{\text{linear}} \R$$

“特征”的直观在于，我们看到人的时候并不是看对应的像素点，而是考虑纹理、局部形状；手之类的特征。

在特征空间 $m \gg 1$ 时，朴素地按下式计算有很高的成本：

$$\min_\theta \frac 1 n \sum_{i=1}^n (\braket{\varphi(x_i), \theta} - y_i)^2 + \lambda \lVert\theta\rVert^2$$

{% <theorem title="Representer Theorem"> %}
最优解一定如下在 $\varphi(x_i)$ 张成空间中：

$$V_n = \operatorname{span}\set{\varphi(x_1), \dots, \varphi(x_n)}$$
{% </theorem> %}

考虑正交分解 $\theta = \theta^\parallel + \theta^\perp$，

$$f_\theta(x_i) = \braket{\theta, \varphi(x_i)} = \braket{\theta^\parallel, \varphi(x_i)} = f_{\theta^\parallel}(x_i)$$

因此我们可以只考虑：

$$\hat\theta = \sum_{i=1}^n \alpha_i \varphi(x_i)$$

我们定义以下核函数，并令矩阵 $K_{ij} = k(x_i, x_j)$，

$$k(x_i, x_j) \coloneqq \braket{\varphi(x_i), \varphi(x_j)}$$

因此只需要考虑以下问题（Kernel Ridge Regression, KRR）：

$$\min_{\alpha \in \R^n} \frac 1 n \lVert K\alpha -y \rVert^2 + \lambda \alpha^\top K\alpha$$

常见的核函数有：

| 名称 | 表达式 |
| :-: | :-: |
| linear | $x^\top x'$ |
| poly | $(x^\top x' + c)^p$ |
| Gaussian | $\exp(- \lVert x-x'\rVert^2 / 2\sigma^2)$ |
| Laplace | $\exp(- \lVert x-x'\rVert_2 / \sigma)$ |

核函数的判别法是：对称、半正定。以以下 Gaussian kernel 为例：

$$k(x, x') = e^{-\frac{(x-x')^2} 2}$$

$$\varphi(x) = e^{-\frac{x^2}{2}} \left(1, x, \frac{x^2}{\sqrt 2}, \frac{x^3}{\sqrt {3!}}, \dots\right)$$
