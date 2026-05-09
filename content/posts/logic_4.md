+++
title = "【逻辑学】一阶逻辑"
date = 2026-05-09

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
+++

命题逻辑对推理的分析无法满足我们的需求，我们还需要对“所有”这样的词语分析。本文不涉及元定理的证明。

<!-- more -->

## 形式语言
在数学中，**名字**（name）有三类：
- **变元**（variable）使用字母表示，在日常生活中不存在完全对应的语言现象，类似的**指示词**（indexical）“这块黑板”
- **专名**（proper name）是完全确定的指称，如“0”、“苏格拉底”
- **限定摹状词**（definite description）如“$x^2$”、“那块黑板上的裂痕”

**谓词**（predicate）是有空位的表达式，如“……大于……”。

在数学中，主要关心的**量词**（quantifier）是最极端的（由于变元、等词的辅助，“唯一一个”之类的量词不是必需的）：
- **全称量词**（universal quantifier）：对于任意一个 $x$
- **存在量词**（existential quantifier）：存在一个 $x$

对于一阶形式语言，会有以下符号：
- 个体变元：各种字母等
- 等词符号：$\dot{=}$
- 命题联结词符号：$\neg, \wedge, \vee, \to$
- 量词符号：$\forall, \exists$
- 技术符号：$()$ 与 $,$

另外还会有一些非逻辑符号：常元符号、函数符号与关系符号。

{% admonition(type="definition", title="项（term）") %}
对一个一阶逻辑形式语言 $\mathcal{L}$, 有穷次使用以下规则得到的符号串是 $\mathcal{L}$-项：
- 个体变元是 $\mathcal{L}$-项
- 常元符号是 $\mathcal{L}$-项
- 如果 $F$ 是 $\mathcal{L}$ 中的 $n$ 元函数符号，而 $t_1, \cdots, t_n$ 是 $n$ 个 $\mathcal{L}$-项，则 $F(t_1, \cdots, t_n)$ 是 $\mathcal{L}$-项
{% end %}

{% admonition(type="definition", title="原子公式（atomic formula）") %}
- 如果 $R$ 是 $\mathcal{L}$ 中的 $n$ 元关系符号，而 $t_1, \cdots, t_n$ 是 $n$ 个 $\mathcal{L}$-项，则 $R(t_1, \cdots, t_n)$ 是原子 $\mathcal{L}$-公式
- 如果 $s, t$ 是 $\mathcal{L}$-项，则 $s \dot{=} t$ 是原子 $\mathcal{L}$-公式
{% end %}

{% admonition(type="definition", title="公式（formula）") %}
- 原子 $\mathcal{L}$-公式是 $\mathcal{L}$-公式
- 如果 $\varphi$ 和 $\psi$ 是 $\mathcal{L}$-公式，那么 $\neg \varphi, (\varphi \wedge \psi), (\varphi \vee \psi), (\varphi \to \psi)$ 是 $\mathcal{L}$-公式
- 如果 $\varphi$ 是 $\mathcal{L}$-公式且 $x$ 是个体变元，则 $\forall x \varphi$ 和 $\exists x \varphi$ 是 $\mathcal{L}$-公式
{% end %}

对 $\mathcal{L}$-公式 $\varphi$，若一部分 $\forall x \psi$, 称 $\forall x \psi$ 是量词 $\forall x$ 在这次出现的辖域（scope）。如果个体变元 $x$ 某次出现在量词的辖域中，则称其为约束出现，否则称为自由出现。至少一次自由出现的称为**自由变元**（free variable），否则称为约束变元（bounded variable）。

一个 $\mathcal{L}$-公式没有自由变元，则称之为 $\mathcal{L}$-语句（sentence）。

## 形式语义
一个 $\mathcal{L}$-结构 $\mathfrak{A}$ 包含：一个涉及所有个体的非空集合，称为**论域**（domain），并把常元符号、函数符号、关系符号解释成 $\mathfrak{A}$ 上的元素、函数、关系。

$\mathfrak{A}$ 上的一个指派（assignment）是一个从个体变元集合到论域的函数，这直观上说的是语境。

仿照之前的文章，剩下的语义定义部分是自然的，读者可自行思考。语义后承、有效式、逻辑等价定义同理。

{% admonition(type="theorem", title="合同引理") %}
$\mathfrak{A}$ 与 $\mathfrak{B}$ 是论域相同的两个 $\mathcal{L}$-结构，分别有指派 $\nu$ 与 $\mu$ 使得对 $\varphi$ 中出现的常元符号、函数符号、关系符号对应相等，自由变元满足 $\nu(x) = \mu(x)$, 则：

$$\mathfrak{A}, \nu \models \varphi \iff \mathfrak{B}, \mu \models \varphi$$
{% end %}

## 证明系统
### 自然演绎系统
除[命题逻辑](@/posts/logic_2.md)自然演绎系统的规则外，还有六条规则。

用记号 $\varphi[t/x]$ 表示将 $x$ 在 $\varphi$ 中每一次自由出现替换为 $t$ 所得的公式。

---

对 $\mathcal{L}$-公式 $\varphi$ 与 $\mathcal{L}$-项 $t$ 与个体变元 $x$, 称 $t$ 对于 $\varphi$ 中的 $x$ 代入自由，如果对 $t$ 中出现的每一个个体变元 $y$, $x$ 在 $\varphi$ 中的每一次自由出现都不在量词 $\forall y$ 或 $\exists y$ 的辖域内。

规则 $(\forall \text{E})$ 是说，如果 $t$ 对于 $\varphi$ 中的 $x$ 代入自由，且 $\begin{matrix} D \cr \forall x \varphi \end{matrix}$ 是 $\mathcal{L}$-推演，则：

$$
\frac{
	\begin{matrix} D \cr \forall x \varphi \end{matrix}
}{\varphi[t/x]} (\forall \text{E})
$$

相应地就有：

$$
\frac{
	\begin{matrix} D \cr \varphi[t/x] \end{matrix}
}{\exists x \varphi} (\exists \text{I})
$$

---

基于等词的直观有：

$$
\frac{~}{t \dot{=} t} (\dot{=} \text{I})
$$

$$
\frac{
	(s \dot{=} t) \quad
	\varphi[s/x]
}{\varphi[t/x]} (\dot{=} \text{E})
$$

实际上可以用它们证明对称性与传递性。

---

$z$ 是 $\mathcal{L}$ 的一个个体变元，$\varphi$ 是一个 $\mathcal{L}$-公式，如果 $z$ 不是前提且不在 $\varphi$ 中出现，则：

$$
\frac{
	\begin{matrix} D \cr \varphi[z/x] \end{matrix}
}{\forall x \varphi} (\forall \text{I})
$$

$z$ 是 $\mathcal{L}$ 的一个个体变元，$\varphi$ 是一个 $\mathcal{L}$-公式，如果 $z$ 不在额外的前提中出现，也不在 $\varphi$ 与 $\psi$ 中出现，则：

$$
\frac{
	\begin{matrix} ~ \cr D_1 \cr \exists x \varphi \end{matrix} \quad
	\begin{matrix} [\varphi[z/x]] \cr D_2 \cr \psi \end{matrix}
}{\psi} (\exists \text{E})
$$

---

语形后承、可证、一致的定义仿照之前文章。

### 希尔伯特式证明系统
希尔伯特式证明系统包含分离规则及以下公理（模式）（不涉及 $\exists$ 因为 $\exists x$ 与 $\neg \forall x \neg$ 一样）：

命题逻辑三个公理模式的一阶版本：

$$\forall x_1 \cdots \forall x_n (\psi \to (\varphi \to \psi))$$

$$\forall x_1 \cdots \forall x_n ((\varphi \to (\psi \to \chi)) \to ((\varphi \to \psi) \to (\varphi \to \chi)))$$

$$\forall x_1 \cdots \forall x_n ((\neg \psi \to \neg \varphi) \to (\varphi \to \psi))$$

以及（出现情况说明省略）：

$$\forall x_1 \cdots \forall x_n (t \dot{=} t)$$

$$\forall x_1 \cdots \forall x_n ((s \dot{=} t) \to (\varphi[s/x] \dot{=} \varphi[t/x]))$$

$$\forall x_1 \cdots \forall x_n (\forall x \varphi(x, x_1, \cdots, x_n) \to \varphi(x, x_1, \cdots, x_n)[t/x])$$

$$\forall x_1 \cdots \forall x_n (\forall x (\varphi(x, x_1, \cdots, x_n) \to \psi(x, x_1, \cdots, x_n)) \to (\forall x \varphi(x, x_1, \cdots, x_n) \to \forall x \psi(x, x_1, \cdots, x_n)))$$

$$\forall x_1 \cdots \forall x_n (\varphi(x, x_1, \cdots, x_n) \to \forall x \varphi(x, x_1, \cdots, x_n))$$
