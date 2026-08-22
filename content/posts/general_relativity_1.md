+++
title = "广义相对论"
draft = true

[extra]
toc = true
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["物理"]
+++

参考书是 Ward, *General Relativity*，采取符号 $(-, +, +, +)$.

## 引入
在广义相对论之前的朴素观念中，我们认为时空（spacetime）有这样的附加结构：对时空中的事件 $p, q$，互斥的三种可能性之一必然发生：（一）某个观测者/物质体能够从事件 $q$ 去往事件 $p$，此时称 $q$ 在 $p$ 的过去；（二）对偶的情形，称 $q$ 在 $p$ 的未来；（三）观测者/物质体不能同时处在 $p$ 与 $q$，此时假定这样的事件构成一个三维集合，定义与 $p$ “同时发生”。

在狭义相对论中仍然区分上述三种互斥的可能性，不同的是（三）类中构成的不再是“一个三维集合”，而是光锥（过去光锥及未来光锥）边界上的点及光锥之外的点。一个观察者能够定义其视角下的“同时发生”，但这依赖于观察者的状态（反过来说，光锥不依赖于观察者），因而没有“绝对同时发生”的概念。

朴素观念及狭义相对论中有惯性（inertial）的概念，用以指代不受外力的物质体所经历的运动。一个惯性观察者这样标记时空中的事件：先建立一个带有笛卡尔坐标系的刚性标架，然后在每个点放一个时钟并校准（如，对一个指定时钟和观察者自己的时钟，让中点处的某观察者对称地发信号，让读数一致）。若观察者 $O$ 标记事件 $p: (t, x, y, z)$，观察者 $O'$ 沿 $x$-方向以速度 $v$ 运动，在 $(0, 0, 0, 0)$ 经过 $O$，则狭义相对论中 $O'$ 标记事件 $p$ 为：

$$
\begin{align*}
	t' &= (t - vx/c^2) / (1 - v^2/c^2)^{1/2} \tag{1.1.1} \cr
	x' &= (x - vt) / (1 - v^2/c^2)^{1/2} \tag{1.1.2} \cr
	y' &= y \tag{1.1.3} \cr
	z' &= z \tag{1.1.4}
\end{align*}
$$

在狭义相对论中，时间间隔与空间间隔都会随观察者变化，独立于观察者的是：

$$I = -(\Delta t)^2 + \frac 1 {c^2} [(\Delta x)^2 + (\Delta y)^2 + (\Delta z)^2] \tag{1.2}$$

实际上 Poincaré 群 $\R^{1, 3} \rtimes \mathrm{SO}(1, 3)$ 的变换恰好是保持 $I$ 的那些线性变换。回忆 [Erlangen 纲领](@/posts/geometry_1_final.md)，我们让 $I$ 是时空的一个度量。

狭义相对论已经与 Maxwell 的理论相容了，然而出于两种原因 Einstein 选择发展一个新的关于时空与重力的理论：（一）所有物体都受到重力，由于运动独立于物体本身，可以考虑把重力场的性质归因于时空本身的结构（我们将会看到自由落体的物体总是走时空度量的测地线）；（二）Mach 及一些其它哲学家、学者不满足于时空的结构一成不变，希望物质能够影响时空。

{% admonition(type="question", title="Car and garage paradox") %}
考虑某车与车库等长，车库管理员在车尾进入的同时瞬间关闭前后车库门。车库管理员言，“车发生了尺缩，因此关门时车库足够容纳车。”司机言，“车库发生了尺缩，因此关门时车库不够大。”两人的陈述是否正确？为了方便，假设车可以穿透车库门。
{% end %}

注意到问题可能出在“同时”。设静止长度 $1$，车速 $v$，记车尾 $C$ 对应车库门 $A$，车头 $D$ 对应车库门 $B$. 先考虑车库管理员视角，以下用小写字母指代对应的事件，我们设：

$$
\begin{align*}
	a, c &: (0, 0, 0, 0) \cr
	b &: (0, 1, 0, 0) \cr
	d &: (0, x_d, 0, 0)
\end{align*}
$$

考虑司机视角（设司机在车尾），使用 $(1.1)$ 式，有 $a', c'$ 不变，其余：

$$
\begin{align*}
	b' &: \left(- \frac v {c^2 \sqrt{1 - v^2/c^2}}, \frac 1 {\sqrt{1 - v^2/c^2}}, 0, 0\right) \cr
	d' &: \left(- \frac{vx_d}{c^2 \sqrt{1 - v^2/c^2}}, \frac {x_d} {\sqrt{1 - v^2/c^2}} = 1, 0, 0\right)
\end{align*}
$$

故 $x_d = \sqrt{1 - v^2/c^2}$，即尺缩效应的结果，车库管理员所说正确。而对司机来说 $a', b'$ 不是同时的，说法不正确。

## 记号定义
与[之前](@/posts/differential_forms.md)相同但换一种看法，我们说 $T$ 是 $(k, l)$ 型张量，如果它是多重线性的：

$$T: \underbrace{V^\ast \times \dots \times V^\ast}_k \times \underbrace{V \times \dots \times V}_l \to \R$$

记 $(k, l)$ 型张量全体是 $\mathscr T(k, l)$，基 $v_i$ 对应对偶基 $v^{i\ast}$.

我们引入关于第 $i$ 个（对偶向量）槽位和第 $j$ 个（向量）槽位**缩并** $C: \mathscr T(k, l) \to \mathscr T(k-1, l-1)$ 是指：

$$CT = \sum_{\sigma=1}^n T(\dots, v^{\sigma\ast}, \dots; \dots, v_\sigma, \dots)$$

其中前者和后者对应安插在第 $i$ 个、第 $j$ 个槽位。

我们接着将 $(k, l)$ 型张量 $T$ 与 $(k', l')$ 型张量 $T'$ 的**外积** $(k+k', l+l')$ 型张量 $T \otimes T'$ 定义为：

$$(T \otimes T')(v^{1\ast}, \dots, v^{k+k'\ast}; w_1, \dots, w_{l+l'}) = T(v^{1\ast}, \dots, v^{k'\ast}; w_1, \dots, w_l) T(v^{k+1\ast}, \dots, v^{k+k'\ast}; w_{l+1}, \dots, w_{l+l'})$$

我们可以把 $(k, l)$ 型张量 $T$ 写成和：

$$T = \sum _{\mu _1, \dots, \nu _l = 1}^n T^{\mu _1 \dots \mu _k}{} _{\nu _1 \dots \nu _l} v _{\mu _1} \otimes \dots \otimes v^{\nu _l\ast}$$

其中的 $T^{\mu _1 \dots \mu _k}{} _{\nu _1 \dots \nu _l}$ 称为分量。

在此种观点下我们有：

$$(CT)^{\mu _1 \dots \mu _{k-1}} _{\nu _1 \dots \nu _{l-1}} = \sum _{\sigma = 1}^n T^{\mu _1 \dots \sigma \dots \mu _{k-1}} _{\nu _1 \dots \sigma \dots \nu _{l-1}} \tag{2.1}$$

$$(T \otimes T')^{\mu _1 \dots \mu _{k+k'}} _{\nu _1 \dots \nu _{l+l'}} = T^{\mu _1 \dots \mu _k} _{\nu _1 \dots \nu _l} T'^{\mu _{k+1} \dots \mu _{k+k'}} _{\nu _{l+1} \dots \nu _{l+l'}} \tag{2.2}$$

我们考虑对称、非退化（$g(v, w) = 0,\\, \forall v \implies w = 0$）的 $(0, 2)$ 型张量，这是度量的允许不正定的版本。记：

$$g = \sum_{\mu, \nu} g_{\mu \nu} \mathrm dx^\mu \otimes \mathrm dx^\nu \tag{2.3}$$

$g$ 有时也记作 $\mathrm ds^2$，体现“无穷小距离”的含义。

由于前述分量是基底相关的，Penrose 引入了一种不依赖基底的**抽象指标记号**：用 $T^{a_1 \dots a_k}{} _{b_1 \dots b_l}$ 表示 $(k, l)$ 型张量，用相同的字母表示同一个槽位。如 $T^{abc}{} _{be}$ 指代 $T^{abc}{} _{de}$ 关于第 $2, 1$ 个槽位缩并得到的 $(2, 1)$ 型张量。为了区分，我们在分量中用希腊字母，在抽象指标记号中用拉丁字母。

对于 $g_{ab}$，它会给出一个 $V_p$ 与 $V_p^\ast$ 的同构，故不妨将 $\nu^a$ 对应的对偶向量 $g_{ab}\nu^b$ 就记作 $\nu^a$；将 $g_{ab}$ 的逆记作 $g^{ab}$，从而有 $g^{ab} g_{bc} = \delta^a_c$.

我们用指标的升降来表达度量/度量的逆的作用。如，对 $T^{abc}{} _{de}$ 是 $(3, 2)$ 型张量，读者可验证：

$$T^{a}{} _b{}^{cde} = g _{bf} g^{dh} g^{ej} T^{afc}{} _{hj}$$

读者也可见 $T_{ab} = T_{ba}$ 等价于说 $T$ 是对称的。我们定义对称与反对称部分：

$$
\begin{align*}
	T_{(a_1, \dots, a_l)} &= \frac 1 {l!} \sum_\pi T_{a_{\pi(1)}, \dots, a_{\pi(l)}} \tag{2.4.1} \cr
	T_{[a_1, \dots, a_l]} &= \frac 1 {l!} \sum_\pi \delta_\pi T_{a_{\pi(1)}, \dots, a_{\pi(l)}} \tag{2.4.2}
\end{align*}
$$

这可以扩展到：

$$T^{(ab)c}{} _{[de]} = \frac 1 4 [T^{abc}{} _{de} + T^{bac}{} _{de} - T^{abc}{} _{ed} - T^{bac}{} _{ed}]$$

## 曲率
我们考虑[联络](@/posts/geometry_3_extra.md)的平行移动看法。
