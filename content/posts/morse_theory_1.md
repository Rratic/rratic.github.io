+++
title = "Morse 理论（一）：非退化光滑函数"
date = 2026-09-26

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "拓扑学"]
+++

此系列为讨论班的复习笔记，这一部分参考 Milnor 的 *Morse Theory* 第一部分，这其中 Chapter 7 涉及一些复几何与代数几何，故不包含。

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

现在就有 $e^\lambda \cap M^{c-\varepsilon} = \dot e^\lambda$，只需 $M^{c-\varepsilon} \cup e^\lambda$ 是 $M^{c+\varepsilon}$ 的形变收缩。

取 $\mu \in C^\infty(\R)$ 满足 $\mu(0) > \varepsilon$，在 $r \geq 2\varepsilon$ 时 $\mu(r) = 0$，且 $-1 < \mu'(r) < 0$，记 $\xi = (u^1)^2 + \cdots + (u^\lambda)^2$ 及 $\eta = (u^{\lambda+1})^2 + \cdots + (u^n)^2$，令：

$$F(q) = \underbrace{c - \xi(q) + \eta(q)}_{f(q)} - \mu(\xi(q) + 2\eta(q))$$

- $F^{-1}(-\infty, c+\varepsilon] = M^{c+\varepsilon}$
- $F$ 与 $f$ 有相同的临界值
- $F^{-1}(-\infty, c-\varepsilon]$ 是 $M^{c+\varepsilon}$ 的形变收缩
- $M^{c-\varepsilon} \cup e^\lambda$ 是 $F^{-1}(-\infty, c-\varepsilon] = M^{c-\varepsilon} \cup H$ 的形变收缩

实际上可以稍微修改一下证明，说明 $M^{c-\varepsilon} \cup e^\lambda$ 是 $M^c$ 的形变收缩，且 $M^c$ 是 $M^{c+\varepsilon}$ 的形变收缩。

{% <theorem title="核心定理"> %}
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
令 $G_k(\R^n)$ 是所有 $k$ 维线性子空间。对 $L \in G_k(\R^n)$，选择 $V$ 是 $L$ 的补空间，所有与 $V$ 横截的 $k$ 维线性子空间 $G_k^V(\R^n)$ 构成 $L$ 的坐标邻域。

考察其胞腔分解（称为 Schubert 分解）。
{% </example> %}

对 $k$ 维线性子空间，记 $P_V: \R^n \to V$ 是投影映射；$A$ 自伴且有特征值 $\lambda_1 < \cdots < \lambda_n$，

$$
\begin{aligned}
f: G_k(\R^n) & \to \R \cr
    V & \mapsto \mathrm{tr}(AP_V)
\end{aligned}
$$

通过*虚张声势的线性代数*，可得到有 $\binom{n}{k}$ 个临界点，对应于 $\set{1, \dots, n}$ 的 $k$ 元子集。

## Morse 不等式
本节较为独立，是有 Whitehead 定理之前所使用的技术。

对 $S$ 是从特定空间打到整数的函数，我们称次可加性是指对 $X \supset Y \supset Z$ 有 $S(X, Z) \leq S(X, Y) + S(Y, Z)$ 的性质，取等时称为可加性。

对于域 $\mathbb F$，我们令：

$$R_\lambda(X, Y) = \text{rank over } \mathbb F \text{ of } \mathsf H_\lambda(X, Y; \mathbb F)$$

考虑如下正合列（对链复形短正合列用同调长正合列定理）：

$$\cdots \to \mathsf H_\lambda(Y, Z) \xrightarrow{i} \mathsf H_\lambda(X, Z) \xrightarrow{j} \mathsf H_\lambda(X, Y) \to \cdots$$

$$R_\lambda(X, Z) = \operatorname{rank} \ker j + \operatorname{rank} \operatorname{im} j = \operatorname{rank} \operatorname{im} i + \operatorname{rank} \operatorname{im} j \leq R_\lambda(X, Y) + R_\lambda(Y, Z)$$

用完整的正合列易说明欧拉示性数 $\chi(X, Y)$ 是可加的，其中：

$$\chi(X, Y) = \sum (-1)^\lambda R_\lambda(X, Y)$$

{% <theorem title="弱 Morse 不等式"> %}
$M$ 是紧流形，记 $C_\lambda$ 为指数 $\lambda$ 的临界点数量，则：

$$
\begin{align}
R_\lambda(M) &\leq C_\lambda \tag{1} \\\\
\sum (-1)^\lambda R_\lambda(M) &= \sum (-1)^\lambda C_\lambda \tag{2}
\end{align}
$$
{% </theorem> %}

取 $a_1 < \cdots < a_k$ 使得 $M^{a_i}$ 恰含 $i$ 个临界点，$M^{a_k} = M$，则用切除引理有：

$$
\begin{align*}
    &H_\ast(M^{a_i}, M^{a_{i-1}}) \cr
    =& H_\ast(M^{a_{i-1}} \cup e^{\lambda_i}, M^{a_{i-1}}) \cr
    =& H_\ast(e^{\lambda_i}, \dot e^{\lambda_i}) \cr
\end{align*}
$$

使用次可加性的性质即得 (1) 式，(2) 式同理。

$$R_\lambda(M) \leq \sum_{i=1}^n R_\lambda(M^{a_i}, M^{a_{i-1}}) = C_\lambda$$

{% <theorem title="强 Morse 不等式"> %}
$$R_\lambda(M) - R_{\lambda-1}(M) + \cdots \pm R_0(M) \leq C_\lambda(M) - C_{\lambda-1}(M) + \cdots \pm C_0(M)$$
{% </theorem> %}

证明略，也是用长正合列搞来搞去。其推论是，若 $C_{\lambda+1} = C_{\lambda-1} = 0$，则 $R_\lambda = C_\lambda$ 且 $R_{\lambda+1} = R_{\lambda-1} = 0$，对于 $\Complex\mathbf P^n$ 就有 $R_0 = R_2 = \cdots = R_{2n} = 1$.

## 嵌入的流形
我们回过头来讨论没有退化临界点的函数是否存在。考虑嵌入 $\R^n$ 的流形及，

$$
\begin{aligned}
L_p: M &\longrightarrow \R \cr
    q &\longmapsto \lVert p-q\rVert^2
\end{aligned}
$$

我们将说明这样的 $f$ 几乎总是满足条件的。

对 $k < n$ 维的 $M$ 嵌入 $\R^n$，我们考虑如下法丛，也是嵌入 $2n$ 维的 $n$ 维流形：

$$N = \set{(q, v) | q \in M, v \text{ perpendicular to } M \text{ at } q}$$

并令 $N \to \R^n$ 端点映射：

$$E(q, v) = q + v$$

{% <definition title="焦点"> %}
称 $e \in \R^n$ 是 $(M, q)$ 的重数-$\mu$ 的焦点，如果 $e = E(q, v)$，且：

$$\operatorname{null} J(E)|_{(q, v)} = \mu > 0$$
{% </definition> %}

由 [Sard 定理](@/posts/smooth_manifolds_3.md)知焦点集在 $M$ 中是零测的。

为了更好地理解焦点，我们考虑嵌入的流形上的“第二基本形式”。设 $M$ 的局部坐标系 $u^1, \dots, u^k$，嵌入映射决定了光滑函数 $\vec x = (x_1, \dots, x_k)$，回忆[“第一基本形式”](@/posts/geometry_2_midterm.md)是：

$$(g_{ij}) = \left(\frac{\partial \vec x}{\partial u_i} \cdot \frac{\partial \vec x}{\partial u_j}\right)$$

记 $\vec l_{ij}$ 是 $\partial^2 \vec x / \partial u^i \partial u^j$ 的法于 $M$ 的部分，取在 $q$ 法于 $M$ 的单位向量 $\vec v$，则下式称为 $M$ 在 $\vec q$ 处沿 $\vec v$ 方向的第二基本形式：

$$\left(\vec v \cdot \frac{\partial^2 \vec x}{\partial u^i \partial u^j}\right) = \left(\vec v \cdot \vec l_{ij}\right)$$

不妨设选取的坐标满足 $g_{ij}$ 在 $q$ 是单位矩阵，则第二基本形式的特征值 $\kappa_1, \dots, \kappa_k$ 称为 $M$ 在 $\vec q$ 处沿 $\vec v$ 方向的主曲率。对应的有定义的 $\kappa_i^{-1}$ 就称为主曲率半径。

{% <theorem title="焦点的分布"> %}
$(M, \vec q)$ 的 $\vec p$ 线向上的焦点恰是那些 $\vec q + \kappa_i^{-1} \vec v$.
{% </theorem> %}

取向量场 $\vec w_1, \dots, \vec w_{n-k}$，使向量都是单位向量、两两正交且正交于 $M$.

让 $N$ 坐标系 $(u^1, \dots, u^k, t^1, \dots, t^{n-k})$，使得 $E: N \to \R^n$ 将它打到：

$$\vec x(u^1, \dots, u^k) + \sum_{\alpha=1}^{n-k} t^\alpha \vec w_\alpha(u^1, \dots, u^k)$$

有 $E$ 的 Jacobi 矩阵形如：

$$
\begin{pmatrix}
    \frac{\partial \vec x}{\partial u_i} \cdot \frac{\partial \vec x}{\partial u_j} +
    \sum_\alpha t^\alpha \frac{\partial \vec w_\alpha}{\partial u^i} \cdot \frac{\partial \vec x}{\partial u^j} &
    \ast \cr 0 & I
\end{pmatrix}
$$

由于我们有：

$$0 = \frac{\partial}{\partial u^i} \left(\vec w_\alpha \cdot \frac{\partial \vec x}{\partial u_j}\right) = \frac{\partial \vec w_\alpha}{\partial u^i} \cdot \frac{\partial \vec x}{\partial u^j} + \vec w_\alpha \cdot \frac{\partial^2 \vec x}{\partial u_i \partial u_j}$$

故，左上块就是：

$$\left(g_{ij} - \sum_\alpha t^\alpha \vec w_\alpha \cdot \vec l_{ij}\right)$$

从而焦点的条件中的 $\operatorname{null} J(E)| _{(q, v)}$ 就是 $\operatorname{null} (g _{ij} - t \vec v \cdot \vec l _{ij})$，完成证明。

{% <theorem title="焦点的用意"> %}
$\vec q$ 是 $L_{\vec p}$ 的退化临界点当且仅当 $\vec p$ 是 $(M, \vec p)$ 的焦点，且其作为临界点的零化度等于作为焦点的重数。
{% </theorem> %}

$$\frac{\partial f}{\partial u^i} = 2\frac{\partial \vec x}{\partial u^i} \cdot (\vec x - \vec p)$$

故有临界点 $\vec q$，如果 $\vec q - \vec p$ 在 $\vec p$ 处与 $M$ 垂直。对 $\vec p = \vec x + t\vec v$ 即有：

$$\frac{\partial^2 f}{\partial u^i \partial u^j} = 2(g _{ij} - t \vec v \cdot \vec l _{ij})$$

将这个结论与焦点的零测性结合，即有几乎所有 $L_p$ 满足条件。存在性与同伦型相关核心定理立即得到推论：

{% <theorem> %}
光滑流形有某个 CW-复形的同伦型。
{% </theorem> %}

{% <theorem title="推论"> %}
任意有界光滑函数 $f: M \to \R$ 可以被某个没有退化临界点的光滑函数 $g$ 一致逼近。进一步可以在紧集 $K$ 上让 $g$ 的第 $i$ 个导数一致逼近 $f$ 的对应导数。
{% </theorem> %}

取 $h: M \to \R^n$ 把 $M$ 嵌入成有界子集，并让 $h_1 = f$，取大数 $c$ 及 $p = (-c, 0, \dots, 0) + \varepsilon$，让：

$$g(x) = \frac{L_p(x) - c^2}{2c}$$

则可计算知：

$$g(x) - f(x) = \sum_{i=1}^n \frac{h_i(x)^2}{2c} - \sum_{i=1}^n \frac{\varepsilon_i h_i(x)}{c} + \sum_{i=1}^n \frac{\varepsilon_i^2}{2c} - \varepsilon_1$$

{% <theorem title="Lp 的指数定理"> %}
$L_p$ 在某个非退化临界点 $q$ 的指数等于 $(M, q)$ 从 $q$ 到 $p$ 的线段上的焦点数（计重数）。
{% </theorem> %}

再一次使用：

$$\frac{\partial^2 f}{\partial u^i \partial u^j} = 2(g _{ij} - t \vec v \cdot \vec l _{ij})$$

---

[^flow]: 对光滑向量场 $X$，其流是一族映射 $\varphi_t: M \to M$，满足 $\varphi_0(q) = q$ 及 $\frac{\mathrm d}{\mathrm dt} \varphi_t(q) = X(\varphi_t(q))$.

    由 ODE，其局部存在唯一且光滑；在 $X$ 有紧支集时是全局的。流有定义的地方具备群性质；在流是全局的时 $\varphi_t$ 是微分同胚。

[^Whi49]: J. H. C. Whitehead. Combinatorial Homotopy. I. *Bull. Amer. Math. Soc.*, 55:213–245, 1949.
