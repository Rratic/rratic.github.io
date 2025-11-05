+++
title = "几何学Ⅰ期中复习笔记"
description = "需要特别记忆的定义和结论。"
date = 2025-10-31
updated = 2025-11-05

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "几何", "Euclid 几何"]
+++

本文用于准备几何学I（实验班）2025秋的期中考试。

## 向量代数
需要记住 Lagrange 定理（远交近攻）：

$$(\alpha\times\beta)\times\gamma = (\alpha\cdot\gamma)\beta - (\beta\cdot\gamma)\alpha$$

可用这种方式现推：由 $(\alpha\times\beta)\times\gamma$ 在 $\alpha$ 与 $\beta$ 张成的平面里，设为 $\lambda\alpha+\mu\beta$，有 $(\lambda\alpha+\mu\beta)\cdot\gamma = 0$，即 $\lambda(\alpha\cdot\gamma)+\mu(\beta\cdot\gamma) = 0$.

另有公式：

$$(a\times b)\cdot(c\times d) = \begin{vmatrix} a\cdot c & b\cdot c\\\\ a\cdot d & b\cdot d\end{vmatrix}$$

可由前式推得。它的转置相等在仿射空间上没有较好的几何解释。

## 二次曲线
把方程的一般形式记作 $Ax^2+2Bxy+Cy^2+2Dx+2Ey+F=0$，也即：

$$
\begin{pmatrix}
x & y & 1
\end{pmatrix}
\begin{pmatrix}
A & B & D \\\\
B & C & E \\\\
D & E & F
\end{pmatrix}
\begin{pmatrix}
x \\\\ y \\\\ 1
\end{pmatrix}
= 0
$$

可以用 $I_2 = \det \begin{pmatrix}A & B\\\\ B & C\end{pmatrix}$ 来判断曲线的类型。在移轴、转轴变换下 $I_2$ 及 $I_1 = A + C$ 与 $I_3 = \det \begin{pmatrix}A & B & D\\\\ B & C & E\\\\ D & E & F\end{pmatrix}$ 不变。

---

有以下重要仿射特征：

{% admonition(type="info", title="中心") %}
一个点 $(x_0, y_0)$ 称为二次曲线的**中心**，如果 $Ax_0+By_0+D=0, Bx_0+Cy_0+E=0$.
{% end %}

中心是对称中心。

只有椭圆型、双曲型曲线有中心。

{% admonition(type="info", title="渐进方向") %}
一个向量 $(x_0, y_0)$ 对应的直线方向为二次曲线的**渐进方向**，如果 $Ax_0^2+2Bx_0y_0+Cy_0^2=0$.
{% end %}

我们定义**渐近线**是距离在无穷远处趋向于 $0$ 的直线[^asymptote]，那么渐近线如果存在，可由中心和渐进方向算得。

{% admonition(type="info", title="共轭直径") %}
一个方向对应的**共轭直径**是所有与它平行的弦的中点的连线。

方程为 $(Ax_0+By_0)x + (Bx_0+Cy_0)y + Dx_0 + Ey_0 = 0$.
{% end %}

两个直线方向共轭，如果 $\begin{pmatrix}x_0' & y_0'\end{pmatrix}\begin{pmatrix}A & B\\\\ B & C\end{pmatrix}\begin{pmatrix}x_0 \\\\ y_0\end{pmatrix}$，也就是一个是另一个的共轭直径的直线方向。

---

另有一个重要的度量特征**对称轴**，非退化椭圆与双曲线都有两条对称轴，可以通过它们的方向共轭且垂直（与共轭方向垂直的方向称为**主方向**）算得。

## 二次曲面
几乎就是抄书。

直线除标准方程、一般方程外，还可用一般方程表示（即两平面的交）：

$$
\left\\{\begin{matrix}
A_1x+B_1y+C_1z+D_1=0 \\\\
A_2x+B_2y+C_2z+D_2=0
\end{matrix}\right.
$$

该表示方法给出直线与平面平行或在其上的条件是：

$$
\begin{pmatrix}
A_1 & B_1 & C_1 \\\\
A_2 & B_2 & C_2 \\\\
A & B & C
\end{pmatrix}
= 0
$$

异面直线的距离可转化为点到平面的距离。

---

一个**旋转面**由曲线 $\Gamma$ 绕轴 $l$ 得到，则称 $\Gamma$ 为**母线**，直线 $l$ 为**轴线**。

若一条曲线与一柱面/锥面的每条直母线相交，则称该曲线为**准线**。

二次曲面分类：通过移轴、转轴，非空二次曲面可化为以下 $14$ 种可能之一：
* 椭球型
	1. 椭球面 $\frac{x^2}{a^2}+\frac{y^2}{b^2}+\frac{z^2}{c^2} = 1$
	2. 单点 $\frac{x^2}{a^2}+\frac{y^2}{b^2}+\frac{z^2}{c^2} = 0$
* 双曲型
	1. 单叶双曲面 $\frac{x^2}{a^2}+\frac{y^2}{b^2}-\frac{z^2}{c^2} = 1$
	2. 双叶双曲面 $\frac{x^2}{a^2}+\frac{y^2}{b^2}-\frac{z^2}{c^2} = -1$
	3. 椭圆锥面 $\frac{x^2}{a^2}+\frac{y^2}{b^2}-\frac{z^2}{c^2} = 0$
* 椭球抛物型
	1. 椭圆抛物面 $\frac{x^2}{a^2}+\frac{y^2}{b^2} = 2z$
	2. 椭圆柱面 $\frac{x^2}{a^2}+\frac{y^2}{b^2} = 1$
	3. 单直线 $\frac{x^2}{a^2}+\frac{y^2}{b^2} = 0$
* 双曲抛物型
	1. 双曲抛物面 $\frac{x^2}{a^2}-\frac{y^2}{b^2} = 2z$
	2. 双曲柱面 $\frac{x^2}{a^2}+\frac{y^2}{b^2} = 1$
	3. 两个相交平面 $\frac{x^2}{a^2}+\frac{y^2}{b^2} = 0$
* 抛物柱型
	1. 抛物柱面 $x^2=2py$
	2. 单平面 $x^2=a^2$
	3. 两平行平面 $x^2=0$

在非平凡的五种二次曲面（椭球面，单叶双曲面，双叶双曲面，椭圆抛物面，双曲抛物面）中有直纹性的是：
1. 双曲抛物面（马鞍面），满足同族的直母线彼此异面，平行于同一平面。
2. 单叶双曲面，满足同族的直母线彼此异面，异族的两条直母线一定共面。

## 变换
**仿射变换**是指保直线的双射，有 $\mathrm{Aff}(\mathbb{E}^n) = \mathrm{Transl}(\mathbb{E}^n) \rtimes \mathrm{GL}(n, \mathbb{R})$.

对**保距变换**，有 $\mathrm{Isom}(\mathbb{E}^n) = \mathrm{Transl}(\mathbb{E}^n) \rtimes \mathrm{O}(n, \mathbb{R})$.

平面上的一些变换定义如下：
- **反射**：关于一条直线对称
- **正压缩**：在垂直于一个直线的方向压缩
- **滑反射**：关于一条直线对称再沿它的方向平移
- **错切**：关于一条直线平行移动，移动距离与点到直线距离成正比
- **斜压缩**：沿着一个向量方向移动，使点到指定直线距离按比例放缩

[^asymptote]: 没有在教材中找到定义，此定义来自 Wikipedia.
