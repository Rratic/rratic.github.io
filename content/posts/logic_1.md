+++
title = "【逻辑学】简单的形式语言及可靠性、完全性证明"
date = 2026-03-10

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "类型论"]
+++

选了哲学系开的Ⅲ类通识课“逻辑导论”。此文主要是做习题（之后大概也会如此），因此先快速掠过定义：

<!-- more -->

---

**形式语言** `formal language` 是指符合特定要求的符号串。令非空符号集 $S$, 考虑形式语言 $L_{\text{all}}^S$ 由所有形如 All $X$ are $Y$ 的公式组成。

一个**形式系统** `formal system` 由若干**推理规则**组成，其中没有前提的称为**公理**。考虑形式系统 $\mathrm{Sys}_{\text{all}}^S$ 包含：

推理规则 $\mathrm{Trans}$:

$$\frac{\text{All } X \text{ are } Y \quad \text{All } Y \text{ are } Z}{\text{All } X \text{ are } Z} \mathrm{Trans}$$

公理模式 $\mathrm{Id}$:

$$\frac{}{\text{All } X \text{ are } X} \mathrm{Id}$$

对一个前提集 $\Gamma$ （可能会长成 $\\{\text{All } A \text{ are } B\\, \text{All } B \text{ are } C\\}$），以 $\varphi$ 结尾的**推演** `deduction` 是指一个有穷公式序列：

$$\varphi_1, \cdots, \varphi_n = \varphi$$

其中每个 $\varphi_i$ 是公理或者属于 $\Gamma$ 或者可以由已有公式通过某规则得到。

此时记作 $\Gamma \vdash_{\mathrm{Sys}} \varphi$. 这样称 $\varphi$ 是 $\Gamma$ 的**语形后承** `syntactic consequence`. 前提集为空时简记为 $\vdash_{\mathrm{Sys}} \varphi$, 此时称推演为**证明**，证明出的公式叫内定理。

---

一个 $L_{\text{all}}^S$ 的**模型** $\mathcal{M}$ 是 $\langle O, I \rangle$, 其中 $O$ 称为对象域是非空集合，$I: S \to \mathcal{P}(O)$ 是解释函数，满足：

$$\mathcal{M} \models \text{All } X \text{ are } Y \iff I(X) \subseteq I(Y)$$

如果 $\mathcal{M} \models \varphi$ 则称 $\varphi$ 在 $\mathcal{M}$ 上真，或 $\mathcal{M}$ 满足 $\varphi$. 此处我们默认公式非真即假，定义符号 $\mathcal{M} \nvDash \varphi$.

如果对任意模型 $\mathcal{M}$ 只要 $M \models \Gamma$ 就有 $M \models \varphi$ 则称 $\varphi$ 是 $\Gamma$ 的**语义后承**，记作 $\Gamma \models \varphi$. 前提集为空时简记为 $\models \varphi$, 此时称 $\varphi$ 是有效的 `valid`.

---

现在证明 $\mathrm{Sys} _{\text{all}}^S$ 相对于 $L _{\text{all}}^S$ 的两个元性质（称为元定理 `metatheorem`）：

{% admonition(type="theorem", title="可靠性 soundness") %}
对任意 $\Gamma, \varphi$ 有：

$$\Gamma \vdash \varphi \implies \Gamma \models \varphi$$
{% end %}

回忆推演定义为 $\varphi_1, \cdots, \varphi_n = \varphi$, 对 $i$ 归纳证 $\varphi \models \varphi_i$ 即可。

{% admonition(type="theorem", title="完全性 completeness") %}
对任意 $\Gamma, \varphi$ 有：

$$\Gamma \models \varphi \implies \Gamma \vdash \varphi$$
{% end %}

考虑证其逆否 $\Gamma \nvdash \varphi \implies \Gamma \nvDash \varphi$. 为此构造一个典范模型 `canonical model` $\mathcal{M}^\Gamma$ 为：
- $O = S$
- $I(X) = \\{Y \in S | \Gamma \vdash \text{All } Y \text{ are } X\\}$

现在可以证明：

$$\mathcal{M}^\Gamma \models \text{All } A \text{ are } B \iff \Gamma \vdash \text{All } A \text{ are } B$$

其中 $\Rightarrow$ 是通过 $\mathrm{Id}$ 证明，$\Leftarrow$ 是通过 $\mathrm{Trans}$ 证明。

从而 $\Gamma \nvdash \varphi$ 时 $\mathcal{M}^\Gamma$ 是一个见证 $\Gamma \nvDash \varphi$ 的反模型。

---

考虑推理规则：

$$\frac{\text{Some } X \text{ is } Y}{\text{Some } Y \text{ is } X} \mathrm{Symm}$$

$$\frac{\text{Some } X \text{ is } Y}{\text{Some } X \text{ is } X} \mathrm{Ex}$$

其语义为：

$$\mathcal{M} \models \text{Some } X \text{ is } Y \iff I(X) \cap I(Y) \neq \emptyset$$

{% admonition(type="question", title="习题") %}
在 $\mathrm{Sys}_{\text{some}}^S$ 中分别用树形和线性推演证明 Some $X$ is $Y$ 可以推出 Some $Y$ is $Y$.
{% end %}

主要是记住格式：

$$
\frac{
	\frac{\text{Some } X \text{ is } Y}{\text{Some } Y \text{ is } X} \mathrm{Symm}
}{\text{Some } Y \text{ is } Y} \mathrm{Ex}
$$

$$
\begin{align*}
& \text{1. Some } X \text{ is } Y \quad (\Gamma) \\\\
& \text{2. Some } Y \text{ is } X \quad (\mathrm{Symm}, 1) \\\\
& \text{3. Some } Y \text{ is } Y \quad (\mathrm{Ex}, 2)
\end{align*}
$$

{% admonition(type="question", title="习题") %}
证明 $\mathrm{Sys}_{\text{some}}^S$ 相对于给定的语义可靠，即两个推理规则保真。
{% end %}

略。

{% admonition(type="question", title="习题") %}
证明 $\mathrm{Sys}_{\text{some}}^S$ 完全。
{% end %}

回顾在构造 $\mathrm{Sys}_{\text{all}}^S$ 的典范模型时是看成有向图，模型是考虑前驱集。

考虑把 $\mathrm{Sys}_{\text{some}}^S$ 看成无向图，模型是考虑连的边。典范模型 $\mathcal{M}^\Gamma$ 为：
- $O = \\{\\{A, B\\} \in \mathcal{P}(S) | \Gamma \vdash \text{Some } A \text{ is } B\\}$
- $I(X) = \\{\\{A, B\\} \in O | (X = A) \vee (X = B)\\}$

{% admonition(type="question", title="习题") %}
尝试为语言 $L_{\text{all, some}}^S$ 设计一个可靠的形式系统，然后思考：你设计的系统是否完全？如果暂时证明不出来，请至少说明你认为困难在哪里。
{% end %}

在各自规则基础上加入：

$$\frac{\text{Some } X \text{ is } Y \quad \text{All } Y \text{ are } Z}{\text{Some } X \text{ is } Z} \mathrm{Unnamed}$$

其完全性确实不容易证（甚至并且很不直观，我看着它都无法保证提供了足够的规则），先空着。

{{ todo() }}
