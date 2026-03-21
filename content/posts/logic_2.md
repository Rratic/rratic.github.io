+++
title = "【逻辑学】经典命题逻辑及"
draft = true

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
+++

这一节讨论的是经典命题逻辑。不同于它的均

<!-- more -->

---

命题逻辑的形式语言使用符号包括：
- 命题联结词：合取 $\wedge$, 析取 $\vee$, 蕴含 $\to$ [^implies], 否定 $\neg$
- 可数个命题字母
- 技术符号：括号

我们定义**命题公式** `propositional formula` 是通过有穷次使用如下规则得到的：
- 命题字母是公式
- 如果符号串 $\varphi$ 与 $\psi$ 是公式，则 $\neg \varphi$, $(\varphi \wedge \psi)$, $(\varphi \vee \psi)$, $(\varphi \to \psi)$ 是公式

有时希望证明所有公式都具有性质 $P$, 可以使用结构归纳法：证明每个命题字母具有性质 $P$ 且如果 $\varphi$ 与 $\psi$ 具有性质 $P$ 则 $(\cdots)$ 均具有性质 $P$.

在经典命题逻辑中，命题有真与假两种属性，在语义上有假设：
- **二值原理** `Bi-valence Principle`: 命题有且仅有两个真值中的一个
- **组合原理** `Composition Principle`: 命
题的真值由组成它的命题的真值以及这些命题的组合方式唯一确定

真值表从略，略记一些术语：
- 一般的析取（满足 $T \vee T = T$）称为相容析取 `inclusive disjunction`, 反之称为不相容析取 `exclusive disjunction`
- 形如 $P \to Q$ 的陈述句称为条件句，其中 $P$ 称为前件 `antecedent`, $Q$ 称为后件 `consequent`
- 一般的蕴含（满足前件为假时命题为真，称为空洞为真 `vacuously true`）称为实质蕴涵 `material implication`

一个**赋值** `valuation` 是一个从公式组成的集合到集合 $\\{T, F\\}$ 的函数，使得各个真值表成立。

令 $\Gamma$ 是公式组成的集合，$\varphi$ 与 $\psi$ 是公式，$V$ 是一个赋值，定义：
- 若 $V(\varphi) = T$ 则称 $\varphi$ 在 $V$ 上为真，或 $V$ 满足 $\varphi$, 进而定义 $V$ 满足 $\Gamma$
- 若对任意赋值 $V$, 一旦 $V$ 满足 $\Gamma$ 就有 $V$ 满足 $\varphi$, 则称 $\varphi$ 是 $\Gamma$ 的一个语义后承，记为 $\Gamma \models \varphi$, 易见这和上一节中定义是类似的
- 若 $\emptyset \models \varphi$, 则称 $\varphi$ 是一个命题有效式/重言式 `propositional validity/tautology`
- 若 $\\{\varphi\\} \models \psi$ 且 $\\{\psi\\} \models \varphi$, 则称 $\varphi$ 与 $\psi$ 逻辑等价 `logical equivalent`, 记为 $\varphi \equiv \psi$

{% admonition(type="theorem", title="紧致性定理") %}
对公式集 $\Gamma$, 若对其任一有穷子集，存在一个赋值满足它，则存在赋值满足 $\Gamma$.
{% end %}

由于命题字母可数个，可以使用归纳法。对命题 $p_0$, 考虑所有赋值构成的集合，分为满足 $V(p_0) = T$ 的与满足 $V(p_0) = F$ 的两部分，必有一部分满足：对任一有穷子集，存在一个赋值满足它。取该对 $p_0$ 的赋值，之后同理。

如果命题字母可以是不可数个，则需要选择公理。

---

[^implies]: 在其它领域中一般使用 $\implies$, 但在逻辑学中通常使用 $\to$.
