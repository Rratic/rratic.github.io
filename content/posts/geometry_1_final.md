+++
title = "几何学Ⅰ期末复习笔记"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "几何", "Euclid 几何"]
+++

下半学期比较好地体现了 Erlangen 纲领，即：

> 几何学是研究图形/对象在变换群作用下不变性质的学问。[^coinci]

主要包括射影几何 $(\mathbb{R}\mathrm{P}^2, \mathrm{PGL}(3, \mathbb{R}))$、反演几何 $(\hat{\mathbb{C}}, M_2(\hat{\mathbb{C}}))$ 与双曲几何 $(\mathbb{H}^2, \mathrm{Isom}(\mathbb{H}^2))$. 额外讲了少量的拓扑与痕量的流形。

## 射影几何
对于线向模型，射影平面上的点是普通点和无穷远点（对应一个线向），线是普通线（原来的线加上无穷远点）及无穷远线（无穷远点集合）。线把模型中点是 $\mathbb{R}^3$ 的一维线性子空间，可以用齐次坐标 $[(x, y, z)^T]$ 表示；线是二维线性子空间，在一个给定的内积下的正交补给出一个双射对应，这导出对偶原理。

射影平面上相异两线总交于唯一一点，相异两点确定唯一一线，射影变换保点线关联关系。

{% admonition(type="theorem", title="保交比") %}
交比 $(A, B; C, D) = \frac{(A, B; C)}{A, B; D} = \frac{AC/CB}{AD/DB}$ 在射影变换下不变。
{% end %}

首先，如果两条直线去截一个由 $O$ 引出的线束，则两个点组的交比相同。这是因为 $(\vec{a_1}, \vec{a_2}; \vec{a_3}, \vec{a_4}) = \frac{s_2t_1}{s_1t_2}$，其中 $\vec{a_3} = s_1\vec{a_1} + t_1\vec{a_2}$，$\vec{a_4} = s_2\vec{a_1} + t_2\vec{a_2}$.

设 $AB$ 与 $CD$ 交于点 $P$，$AD$ 与 $BC$ 交于点 $Q$. 考虑将 $PQ$ 映成无穷远线的射影变换，此时 $ABCD$ 构成平行四边形。

{% admonition(type="theorem", title="射影变换生成") %}
射影平面的所有射影变换由（1）仿射射影变换（由仿射变换唯一地延拓）（2）两个中心投影复合生成。
{% end %}

{% admonition(type="definition", title="射影圆锥曲线") %}
射影圆锥曲线是线把模型上由齐次方程 $Ax^2 + 2Bxy + Cy^2 + 2Dxz + 2Eyz + Fz^2 = 0$ 决定的。
{% end %}

{% admonition(type="theorem", title="唯一决定") %}
一般位置 5 点决定唯一的圆锥曲线。
{% end %}

## 反演几何
Steiner 圆族定理

## 双曲几何

---

[^coinci]: 发现 `symmetric` 一词包含 `metric`，不知道是不是巧合。
