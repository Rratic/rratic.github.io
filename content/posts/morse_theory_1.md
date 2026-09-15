+++
title = "【草稿】Morse 理论（一）"
date = 2026-09-15

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

此系列为讨论班的复习笔记，这一部分参考 Milnor 的 *Morse Theory* 第一部分。

<!-- more -->

Morse 理论的想法可参考如下例子：

![导引](/images/geometry/morse_theory_intro.jpeg)

## 定义
考虑光滑流形。对 $f: M \to \R$，称 $p$ 是其**临界点**，如果其诱导的 $f_\ast: TM_p  \to T\R_{f(p)}$ 为零，也即对局部坐标：

$$\frac{\partial f}{\partial x^i} = 0, \quad \forall i$$

用 $M^a$ 表示 $\set{x \in M | f(x) \leq a}$，由隐函数定理，在 $a$ 不是临界点时，$M^a$ 是光滑带边流形，$f^{-1}(a)$ 是光滑子流形。

称临界点 $p$ 是**非退化**的，如果以下矩阵非奇异：

$$\left(\frac{\partial^2 f}{\partial x^i \partial x^j}(p)\right)$$

我们定义一个 $TM_p$ 上的双线性形式 $f_{\ast\ast}$，设 $v, w$ 拓展到向量场 $\tilde v, \tilde w$，令：

$$f_{\ast\ast} \coloneqq \tilde v_p(\tilde w(f))$$

对临界点 $p$，有 $[\tilde v, \tilde w]_p(f) = \mathrm df_p([\tilde v, \tilde w]_p) = 0$，故 $f$ 对称；又，由表达式形式知与 $\tilde v, \tilde w$ 选取无关。

称 $f$ 在 $p$ 处的指数（index）是 $f_{\ast\ast}|_p$ 的负惯性指数。

{% <theorem title="Morse 引理"> %}
设 $p$ 是 $f$ 的非退化临界点，则存在 $p$ 邻域 $U$ 及其上坐标系 $(y^1, \dots, y^n), y^i(p) = 0$，使得：

$$f = f(p) - (y^1)^2 - \cdots - (y^\lambda)^2 + (y^{\lambda+1})^2 + \cdots + (y^n)^2$$
{% </theorem> %}

在此种形式下，由 $p$ 处的 Hessian 矩阵可得 $\lambda$ 即 $f$ 在 $p$ 处的指数。

在一个邻域内取坐标卡 $(x_1, \dots, x_n)$，令：

$$g_j(x_1, \dots, x_n) = \int_0^1 \frac{\partial f}{\partial x_j} f(tx_1, \dots, tx_n) \mathrm dt$$

$$h_{ij}(x_1, \dots, x_n) = \int_0^1 \frac{\partial g_j}{\partial x_i} g(tx_1, \dots, tx_n) \mathrm dt$$

可以取 $\overline{h_{ij}} = \frac 1 2 (h_{ij} + h_{ji})$ 使之对称，则：

$$(\bar h_{ij}) = \left(\frac 1 2 \frac{\partial^2 f}{\partial x^i \partial x^j}(0)\right)$$

现在进行合同变换即可。其推论是，非退化的临界点是孤立的。

## 同伦型
{% <theorem> %}
对 $f: M \to \R$，若 $f^{-1}[a, b]$ 紧且不含临界点，则 $M^a$ 是 $M^b$ 的形变收缩。
{% </theorem> %}

选取 $M$ 是一个 Riemann 度量，记切向量内积 $\braket{X, Y}$，梯度 $\mathrm{grad}$ 满足 $\braket{X, \operatorname{grad} f} = X(f)$，此向量场恰在 $f$ 的临界点消没。

令光滑 $\rho: M \to \R$ 在 $f^{-1}[a, b]$ 是 $1 / \braket{\operatorname{grad} f, \operatorname{grad} f}$，在一个紧邻域外消没，令向量场：

$$X_q = \rho(q) (\operatorname{grad} f)_q$$

这会生成一个单参数群[^flow] $\varphi_t: M \to M$，生成是指满足：

$$X_q(f) = \lim_{h \to 0} \frac{f(\varphi_h(q)) - f(q)}{h}$$

从而有：

$$\frac{\mathrm df(\varphi_p(q))}{\mathrm dt} = \left\langle\frac{\mathrm d\varphi_p(q)}{\mathrm dt}, \operatorname{grad} f\right\rangle = \braket{X, \operatorname{grad} f} = 1$$

故 $f(\varphi_t(q)) = f(q) + t$，知 $\varphi_{b-a}$ 给出 $M^a \to M^b$ 微分同胚。进一步定义 $r_t: M^b \to M^b$，

$$r_t(q) = \begin{cases} q & \text{if } f(q) \leq a \cr \varphi_{t(a-f(q))}(q) & \text{if } a \leq f(q) \leq b \end{cases}$$

则 $r$ 给出所求形变收缩。

{% <theorem> %}
对 $f: M \to \R$，$p$ 是指数 $\lambda$ 的非退化临界点，设 $f(p) = c$，对某个 $\varepsilon$ 有 $f^{-1}[c-\varepsilon, c+\varepsilon]$ 紧且不含 $p$ 外临界点，则对充分小的 $\varepsilon$，$M^{c+\varepsilon}$ 的同伦型是 $M^{c-\varepsilon}$ 附加上一个 $\lambda$-胞腔。
{% </theorem> %}

证明思路如下：

![证明思路](/images/geometry/morse_theory_attach.png)

取 $p$ 处坐标系使得：

$$f = c - (u^1)^2 - \cdots - (u^\lambda)^2 + (u^{\lambda+1})^2 + \cdots + (u^n)^2$$

取充分小的 $\varepsilon$，并让 $e^\lambda$ 是：

$$\set{p \in U | (u^1)^2 + \cdots + (u^\lambda)^2 \leq \varepsilon,\\, u^{\lambda+1} = \cdots = u^n = 0}$$

现在就有 $e^\lambda \cap M^{c-\varepsilon} = \partial e^\lambda$，只需 $M^{c-\varepsilon} \cup e^\lambda$ 是 $M^{c+\varepsilon}$ 的形变收缩。

取 $\mu \in C^\infty(\R)$ 满足 $\mu(0) > \varepsilon$，在 $r \geq 2\varepsilon$ 时 $\mu(r) = 0$，且 $-1 < \mu'(r) < 0$，记 $\xi = (u^1)^2 + \cdots + (u^\lambda)^2$ 及 $\eta = (u^{\lambda+1})^2 + \cdots + (u^n)^2$，令：

$$F(q) = \underbrace{c - \xi(q) + \eta(q)}_{f(q)} - \mu(\xi(q) + 2\eta(q))$$

- $F^{-1}(-\infty, c+\varepsilon] = M^{c+\varepsilon}$
- $F$ 与 $f$ 有相同的临界值
- $F^{-1}(-\infty, c-\varepsilon]$ 是 $M^{c+\varepsilon}$ 的形变收缩
- $M^{c-\varepsilon} \cup e^\lambda$ 是 $F^{-1}(-\infty, c-\varepsilon] = M^{c-\varepsilon} \cup H$ 的形变收缩

实际上可以稍微修改一下证明，说明 $M^{c-\varepsilon} \cup e^\lambda$ 是 $M^c$ 的形变收缩，且 $M^c$ 是 $M^{c+\varepsilon}$ 的形变收缩。

{% <theorem> %}
对 $f$ 是 $M$ 上可微函数，无退化临界点，且所有 $M^a$ 紧，则 $M$ 有一个 CW-复形的同伦型，其中每个指数 $\lambda$ 临界点对应一个 $\lambda$ 维胞腔。
{% </theorem> %}

有限情形容易，无限情形可以用 Whitehead [Whi49][^Whi49] 的定理一。

## 例子
{% <theorem title="(Reeb)"> %}
$M$ 是紧流形，$f$ 是 $M$ 上恰有两个临界点（且均非退化）的可微函数，则 $M$ 与一个球面同胚。
{% </theorem> %}

两个临界点必为最小值与最大值点，不妨是 $f(p) = 0, f(q) = 1$，取充分小的 $\varepsilon$ 则 $f^{-1}[0, \varepsilon]$ 与 $f^{-1}[1-\varepsilon, 1]$ 是闭 $n$-胞腔（Morse 引理给出 $f = f(p) + (y^1)^2 + \cdots + (y^n)^2$）。

又 $M^\varepsilon \cong M^{1-\varepsilon}$，故 $M \cong \mathbb S^n$.

{% <example title="7 维怪球"> %}
考虑底空间 $\mathbb S^4$，看成 $\R^4 \cup_{\R^4 \setminus \set{0}} \R^4$，分别在两个 $\R^4$ 上取平凡 $\mathbb S^3$-丛，

拼接方式（$u \in \R^4 \setminus \set{0}, v \in \mathbb S^3$ 均看作四元数）以：

$$(u, v) \leftrightarrow (u', v') = \left(\frac u {\lVert u \rVert^2}, \frac{u^h v u^j}{\lVert u \rVert}\right)$$

对整数 $h, j$ 满足 $h + j = 1$，得到的 $M$ 同胚于 $\mathbb S^7$.
{% </example> %}

注：但并不微分同胚，这是其重要历史意义。

记 $u'' = u' \cdot (v')^{-1} = u^h v^{-1} u^{-h} / \lVert u \rVert$，令：

$$f(x) = \frac{\Re(u'')}{(1 + \lVert u'' \rVert^2)^{\frac 1 2}}$$

计算知其临界点只有 $(0, 1)$ 与 $(0, -1)$.

{% <example title="复射影空间"> %}
$$\Complex\mathbf P^n \simeq e^0 \cup e^2 \cup \cdots \cup e^{2n}$$
{% </example> %}

记空间的元素形如 $(z_0 : z_1 : \dots : z_n)$，其中 $\sum |z_j|^2 = 1$；

$$f(z_0 : z_1 : \dots : z_n) = \sum c_j |z_j|^2, \quad c_i \neq c_j$$

考虑 $U_0$ 是 $z_0 \neq 0$ 对应集合，在其中令：

$$|z_0| \frac{z_j}{z_0} = x_j + \mathrm i y_j$$

那么有：

$$f = c_0 + \sum_{j=1}^n (c_j - c_0) (x_j^2 + y_j^2)$$

其临界点只有 $(1 : 0 : \cdots : 0)$，对应指数是两倍的 $c_j < c_0$ 个数。同理讨论 $U_k$ 是 $z_k \neq 0$ 对应集合即可。

{% <example title="Grassmannian 流形"> %}
令 $G_k(\R^n)$ 是所有 $k$ 维线性子空间，考察其胞腔分解（称为 Schubert 分解）。
{% </example> %}

对 $k$ 维线性子空间，记 $P_V: \R^n \to V$ 是投影映射；$A$ 自伴且有特征值 $\lambda_1 < \cdots < \lambda_n$，

$$
\begin{aligned}
f: G_k(\R^n) & \to \R \cr
    V & \mapsto \mathrm{tr}(AP_V)
\end{aligned}
$$

{{ <todo /> }}

---

[^flow]: 对光滑向量场 $X$，其流是一族映射 $\varphi_t: M \to M$，满足 $\varphi_0(q) = q$ 及 $\frac{\mathrm d}{\mathrm dt} \varphi_t(q) = X(\varphi_t(q))$.

    由 ODE，其局部存在唯一且光滑；在 $X$ 有紧支集时是全局的。流有定义的地方具备群性质；在流是全局的时 $\varphi_t$ 是微分同胚。

[^Whi49]: J. H. C. Whitehead. Combinatorial Homotopy. I. *Bull. Amer. Math. Soc.*, 55:213–245, 1949.
