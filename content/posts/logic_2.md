+++
title = "【逻辑学】经典命题逻辑及其强可靠性、强完全性"
date = 2026-03-25

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
+++

这一节主要讨论的是经典命题逻辑（不同于它的均称为“非经典”），重点在于其强完全性的证明。

<!-- more -->

## 形式语言
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

## 形式系统
现在考察**命题逻辑自然演绎系统** `Natrual Deduction System of Propositional Logic`. 自然演绎系统的核心是**推演** `derivation`. 我们使用记号：

$$D \\\\ \varphi$$

表示一个结论为 $\varphi$ 的推演，用 $h(D)$ 表示 $D$ 的高度。

### 规则
最简单的规则是规则 $(\text{A})$: 如果 $\varphi$ 是一个公式，那么写 $\varphi$ 或者：

$$\frac\varphi\varphi (\text{A})$$

其高度是 $0$.

---

规则 $(\wedge \text{I})$ 是关于 $\wedge$ 的规则（这里 $I$ 表示引入规则），如下：如果 $\varphi, \psi$ 是公式，且 $\begin{matrix} D_1 \\\\ \varphi \end{matrix}$ 和 $\begin{matrix} D_2 \\\\ \psi \end{matrix}$ 都是推演，则有推演：

$$
\frac{
	\begin{matrix} D_1 \\\\ \varphi \end{matrix} \quad
	\begin{matrix} D_2 \\\\ \psi \end{matrix}
}{(\varphi \wedge \psi)} (\wedge \text{I})
$$

---

规则 $(\wedge \text{E})$ 类似（这里 $E$ 表示消去规则）：

$$
\frac{
	\begin{matrix} D \\\\ (\varphi \wedge \psi) \end{matrix}
}{\varphi} (\wedge \text{E}) \quad
\frac{
	\begin{matrix} D \\\\ (\varphi \wedge \psi) \end{matrix}
}{\psi} (\wedge \text{E})
$$

读者可自行写出规则 $(\vee \text{I}), (\to \text{E})$.

---

规则 $(\to \text{I})$ 涉及到假设。

我们用方括号表示假设，用 $\begin{matrix} [\varphi] \\\\ D \\\\ \psi \end{matrix}$ 表示将 $\begin{matrix} D \\\\ \psi \end{matrix}$ 中所有由规则 $(\text{A})$ 引入的 $\varphi$ 换成 $[\varphi]$ 的结果。

规则 $(\to \text{I})$ 及另外三条规则（$(\neg \text{I})$, $(\text{RAA})$, $(\vee \text{E})$）可以把“借来”的假设“还”回去。借的假设一定要还。

$$
\frac{
	\begin{matrix} [\varphi] \\\\ D \\\\ \psi \end{matrix}
}{(\varphi \to \psi)} (\to \text{I})
$$

在实际使用时，我们加上上标 $[\varphi]^1$, 然后用 $(\to \text{I}), 1$ 标记还了哪个假设。

有时我们可以不用额外通过规则 $(\text{A})$ 引入，此时不需要把它做成假设：

$$
\frac{q}{(p \to q)} (\to \text{I})
$$

---

规则 $(\neg \text{I})$ 如下：

$$
\frac{
	\begin{matrix} [\varphi] \\\\ D \\\\ \psi \end{matrix} \quad
	\begin{matrix} [\varphi] \\\\ D' \\\\ \neg\psi \end{matrix}
}{\neg \varphi} (\neg \text{I})
$$

---

规则 $(\text{RAA})$ 即是反证法，如下：

$$
\frac{
	\begin{matrix} [\neg \varphi] \\\\ D \\\\ \psi \end{matrix} \quad
	\begin{matrix} [\neg \varphi] \\\\ D' \\\\ \neg\psi \end{matrix}
}{\varphi} (\neg \text{I})
$$

直觉主义逻辑自然演绎系统中没有这一条规则。关于直觉主义逻辑何以有用可参考我之前写的[类型论笔记](@/posts/type_theory_1.md)。

---

最后是规则 $(\vee \text{E})$:

$$
\frac{
	\begin{matrix} ~ \\\\ D_1 \\\\ (\varphi \vee \psi) \end{matrix} \quad
	\begin{matrix} [\varphi] \\\\ D_2 \\\\ \chi \end{matrix} \quad
	\begin{matrix} [\varphi] \\\\ D_3 \\\\ \chi \end{matrix}
}{\varphi} (\vee \text{E})
$$

### 语形后承
仿照上一节中的定义，我们记 $\varphi$ 是 $\Gamma$ 在自然演绎系统中的语形后承 `syntactic consequence (in natural deduction)` 为 $\Gamma \vdash^\text{ND} \varphi$.

如果 $\emptyset \vdash^\text{ND} \varphi$ 则称 $\varphi$ 是可证的 `provable (in natural deduction)`.

称 $\Gamma$ 是（在自然演绎系统下）一致的 `consistent`, 如果不存在一个公式 $\psi$ 使得 $\Gamma \vdash^\text{ND} \varphi$ 且 $\Gamma \vdash^\text{ND} \neg\varphi$.

### 可靠性与完全性
为了方便起见，只考虑 $\neg$ 和 $\to$ 两个命题联结词符号，因为具有真值函数完全性。

{% admonition(type="theorem", title="强可靠性定理") %}
对任意公式集 $\Gamma$ 和公式 $\varphi$, 如果 $\Gamma \vdash^\text{ND} \varphi$ 则 $\Gamma \models \varphi$.
{% end %}

对 $h(D)$ 归纳即可。

{% admonition(type="theorem", title="弱完全性定理") %}
对任意公式 $\varphi$, 如果 $\emptyset \models \varphi$ 则 $\emptyset \vdash^\text{ND} \varphi$.
{% end %}

我们先考虑一个例子。通过真值表我们知道 $\emptyset \models (p \to (q \to p))$. 首先希望证明 $\\{\neg p, \neg q\\} \vdash^\text{ND} (p \to (q \to p))$ 及另外三个情况也成立，然后用规则 $(\neg \text{I}), (\text{RAA})$ 拼成一个前提集 $\emptyset$ 的推演。

对任意赋值 $V$, 定义：

$$
\varphi^V = \begin{cases}
\varphi & \text{if } V(\varphi) = T \\\\
\neg \varphi & \text{if } V(\varphi) = F
\end{cases}
$$

我们对 $n$ 归纳证明：如果 $p_0, \cdots, p_n$ 包含了 $\varphi$ 中出现的所有命题字母，则对任意赋值 $V$ 有 $\\{p_0^V, \cdots, p_n^V\\} \vdash^\text{ND} \varphi^V$. 这里强行讨论即可。

然后证明：对任意公式集 $\Gamma$ 和公式 $\varphi$ 和 $\psi$, 如果 $\Gamma \cup \\{\varphi\\} \vdash^\text{ND} \psi$ 且 $\Gamma \cup \\{\neg \varphi\\} \vdash^\text{ND} \psi$, 则 $\Gamma\vdash^\text{ND} \psi$.

{% admonition(type="theorem", title="强完全性定理") %}
对任意公式集 $\Gamma$ 和公式 $\varphi$, 如果 $\Gamma \models \varphi$ 则 $\Gamma \vdash^\text{ND} \varphi$.
{% end %}

由定义，不存在赋值满足 $\Gamma \cup \\{\neg \varphi\\}$. 由紧致性定理知存在 $\psi_1, \cdots, \psi_n$ 使得不存在赋值满足 $\\{\psi_1, \cdots, \psi_n, \neg \varphi\\}$. 因此 $\emptyset \models (\psi_1 \to (\cdots (\psi_n \to \varphi) \cdots))$. 使用弱完全性定理再使用 $\to \text{E}$ 即可。

### 再证强完全性定理
对于大部分逻辑来说，弱完全性定理并没有像经典命题逻辑这样的简单的、构造性的证明。一般的方法是基于极大一致集。

称一个公式集 $\Gamma$ 是**极大一致集** `maximal consistent set`, 如果它一致，且对任意公式 $\psi$, 要么 $\psi \in \Gamma$ 要么 $\neg \psi \in \Gamma$.

{% admonition(type="theorem", title="真值引理") %}
令 $\Delta$ 是极大一致集，定义一个赋值：

$$
V^\Delta(\varphi) = \begin{cases}
	T & \text{if } \varphi \in \Delta \\\\
	F & \text{if } \varphi \notin \Delta
\end{cases}
$$

则 $V^\Delta \models \Delta$.
{% end %}

归纳即可。

{% admonition(type="theorem", title="Lindenbaum 引理") %}
每一个一致集都是某个极大一致集的子集。
{% end %}

将公式用自然数编号，依次考虑是否加入。

## 希尔伯特式证明系统
希尔伯特式证明系统是经典命题逻辑的另一个证明系统。

公理包括：
1. $(\psi \to (\varphi \to \psi))$
2. $((\varphi \to (\psi \to \chi)) \to ((\varphi \to \psi) \to (\varphi \to \chi)))$
3. $((\neg \psi \to \neg \varphi) \to (\varphi \to \psi))$

另有分离规则 `Modus Ponens`: 可以从 $\varphi$ 和 $(\varphi \to \psi)$ 推出 $\psi$.

读者可在 [P. 1 of Theorem List - Metamath Proof Explorer](https://us.metamath.org/mpeuni/mmtheorems1.html) 看到使用它们推导出一些基本结论的过程。

为了证明公理三独立于公理一、二和分离规则，我们考虑赋值 $f$ 使得对任意公式 $\varphi$ 有 $f(\neg \varphi) = F$, 且对任意公式 $\varphi, \psi$ 有 $f((\varphi \to \psi)) = T$ 当且仅当 $f(\varphi) = F$ 或 $f(\psi) = T$. 这称为否定恒假赋值。

---

[^implies]: 在其它领域中一般使用 $\implies$, 但在逻辑学中通常使用 $\to$.
