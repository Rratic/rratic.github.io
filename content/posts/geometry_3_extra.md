+++
title = "一点点几何学随机学习"
date = 2026-08-17

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["数学", "几何学"]
+++

在暑假初的时候阅读丘赛几何与拓扑的考纲，发现自己什么都不会，打算好好学习重新做人（然而并没有）。当时参考了知乎上一篇[学习建议](https://zhuanlan.zhihu.com/p/40333692)。本文为一些碎片学习的整合，部分内容被重新整理到其它文章中。

<!-- more -->

## 层
首先，对拓扑空间 $X$，其上的一个 $\mathcal C$-值**预层** $\mathcal F$ 是一个从 $X$ 的开集格（按包含偏序）到 $\mathcal C$ 的反变函子。考虑集合值预层，一个开集 $U$ 对应的对象 $\mathcal F(U)$ 称为 $U$ 的截面；$U \subseteq V$ 对应的态射 $\mathrm{res}_{U, V}$ 称为 $U$ 到 $V$ 的限制映射。

在此之上，一个预层 $\mathcal F$ 称为**层**（sheaf），如果对 $X$ 的任意开集 $U$ 及 $U$ 的开覆盖 $\set{U_i}$，满足局部确定性：

$$(\forall i: \mathrm{res}_ {U, U _i}(s) = \mathrm{res} _{U, U _i}(t)) \implies s = t$$

及粘合性：如果对一族截面 $s_i \in \mathcal F(U_i)$ 它们在重叠部分一致（即下式），则存在截面 $s \in \mathcal F(U)$ 使得对每个 $i$ 都有 $\mathrm{res}_{U, U_i}(s) = s_i$.

$$\mathrm{res} _{U _i, U _i \cap U _j}(s _i) = \mathrm{res} _{U _j, U _i \cap U _j}(s _j)$$

由局部确定性这还是唯一的。实际上，这两个条件也可以视作下图是等化子：

$$\mathcal F(U) \to \prod_i \mathcal F(U_i) \rightrightarrows \prod_{i, j} \mathcal F(U_i \cap U_j)$$

典型的例子是连续函数层：

$$\mathcal C^0(U) = \set{f: U \to \R | f \text{ continuous}}$$

类似地有光滑函数层、全纯函数层、常值层（截面是局部常值函数）。

---

点 $x \in X$ 处的**茎**（stalk）定义为如下余极限（即所有邻域截面的芽（germ）组成的对象）：

$$\mathcal F_x = \operatorname*{colim}_{U \ni x} \mathcal{F}(U)$$

## 配边理论
两个 $n$ 维闭流形 $M, N$ 称为**配边**的，如果存在一个 $n + 1$ 维紧流形 $W$，使得：

$$\partial W \cong M \sqcup N$$

一个典型的例子是裤子状曲面将一个圆与两个圆配边，反例是 $\R\mathrm{P}^2$ 与 $\emptyset$ 不配边。

如果考虑的是定向流形，则需要 $W$ 是定向的，关系式改为 $\partial W \cong M \sqcup (-N)$.

设 $M^m$ 是闭光滑流形，并选取光滑嵌入 $i: M \hookrightarrow \R^{m+k}$. 记 $i$ 的秩 $k$ 法丛为 $\nu_i$，一个法丛标架是向量丛同构：

$$\varphi: \nu_i \cong M \times \R^k$$

将嵌入加入额外的平凡法方向，会把 $\varphi$ 替换为 $\varphi \oplus \mathrm{id}_{\R}$. 如果两个法丛标架在分别加入有限个平凡方向后可以通过一族法丛标架相连，就称它们稳定等价；不同高维嵌入给出的稳定法丛也按这种方式识别。这样的等价类称为 $M$ 的稳定法丛标架，会诱导流形的定向。两个带稳定标架的闭 $m$ 维流形 $M, N$ 称为带标架配边的，如果存在 $m + 1$ 维紧流形 $W$ 及其稳定法丛标架，使得：

$$\partial W \cong M_0 \sqcup (-M_1)$$

并且 $W$ 的标架按照外法向优先的边界约定，在两个边界分支上分别限制为给定标架；负号表示反转诱导定向及相应的边界标架。带标架配边类关于不交并构成 Abel 群，记为 $\Omega_m^{\mathrm{fr}}$；单位元由空流形表示，逆元可由反转标架中的一个法向量得到。

球面的第 $m$ 个稳定同伦群定义为悬挂映射构成的归纳系统的余极限：

$$\pi _m^{\mathrm S}(\mathbb S) = \operatorname*{colim} _{k \to \infty} \pi _{m+k}(\mathbb S^k)$$

Pontryagin–Thom 构造给出了如下结果：

$$\Omega_m^{\mathrm{fr}} \cong \pi_m^{\mathrm S}(\mathbb S)$$
