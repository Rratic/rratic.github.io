+++
title = "【逻辑学】简单的形式语言及可靠性、完全性证明"
date = 2026-03-10
updated = 2026-03-18

[extra]
math = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "数学", "基石", "逻辑学"]
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

如果对任意模型 $\mathcal{M}$ 只要 $M \models \Gamma$ 就有 $M \models \varphi$ 则称 $\varphi$ 是 $\Gamma$ 的**语义后承** `semantic consequence`, 记作 $\Gamma \models \varphi$. 前提集为空时简记为 $\models \varphi$, 此时称 $\varphi$ 是有效的 `valid`.

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
在 $\mathrm{Sys}_{\text{some}}^S$ 中分别用树形和线性推演证明 $\text{Some } X \text{ is } Y$ 可以推出 $\text{Some } Y \text{ is } Y$.
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

<div id="anchor"></div>

其完全性确实不容易证（甚至并且很不直观，我看着它都无法保证提供了足够的规则）。

实际上会发现：我们无法通过构造单个典范模型说明这一点（否则，如果 $X$ 不满足 $\text{Some } X \text{ is } X$, 则 $I(X) = \emptyset$, 从而 $\text{All } X \text{ is } Y$ 总是成立）。

这里回答一个问题：从语义上说，如果 $A$ 不满足 $\text{Some } A \text{ is } A$, 那么所有的 $\text{All } A \text{ is } B$ 总是成立，也就是说，孤零零两个点 $X, Y$ 之间没有任何关系（只有 $\text{All}$ 自己是自己）不是合法的最终图景，是否说明上述五条规则不是完全的？答案是否定的，回到定义会发现，我们定义的不是“最终图景”这样的东西。我们说在某个前提下可以推演出某个结果，而没有说“某个前提不成立”下可以推出哪个结果。语言中也没有 $\text{Not}$ 之类的句子来间接表达这一点。在孤零零两个点例子中，我们确实不能推出 $\text{Some } X \text{ is } Y$, 因为也有可能是另一种情况。而如果你想问：“我们知道这个事实存在，但是无法写成规则，会不会丢失信息？”，会发现最终自然的要求就是完全性。

考虑分情况讨论 $\varphi$ 是 $\text{All}$-句子和 $\text{Some}$-句子两种情形（来自助教的提示）。回顾定义，只需要分别构造模型 $\mathcal{M}$ 满足：

$$\mathcal{M} \models \Gamma \tag{1}$$

$$\mathcal{M} \models \varphi \iff \Gamma \vdash \varphi \tag{2}$$

对于 $\text{All}$-句子，考虑我们之前构造的 $\mathrm{Sys}_{\text{all}}^S$ 的典范模型 $\mathcal{M}'$, 现在在对象域中加入 $\star$, 并在所有 $I(X)$ 中添上 $\star$. 这会使得所有 $\text{Some}$-句子都成立，从而 (1) 成立；同时 (2) 成立。

对于 $\text{Some}$-句子，使用图论直观，考虑对象域为 $O = \\{w_{X, Y} | \Gamma \vdash \text{Some } X \text{ is } Y \\}$ 其中 $w_{X,Y} = w_{Y,X}$, 解释函数 $I(X) = \\{w_{Y, Z} \in O | \text{All } X \text{ are } Y\\}$. 读者可验证 (1) (2) 成立。注意其中 $\text{Ex}$ 与 $\text{Unnamed}$ 规则的混合使用。

---

特别地，我的同学 [@Kavod](https://space.bilibili.com/1861613068) 提出了对 $\varphi: \text{Some } X \text{ is } Y$ 构造反模型的方法（如果 $\varphi$ 不成立，有一个模型使 $I(X), I(Y)$ 无交）；采取图论视角：
1. 取 $\mathcal{M} \models \Gamma$
2. 令 $N(X) = \\{Z | \Gamma \vdash \text{All } Z \text{ are } X\\}$, 则 $N(X) \cap N(Y)$ 中的元素不能有 $\text{Some}$-边，语义上它们都是空集
3. 只考虑 $X \notin N(Y), Y \notin N(X)$ 的情形，并略去不看 $N(X) \cap N(Y)$
4. 我们将 $N(X) \setminus N(Y)$ 捏成 $X$, 将 $N(Y) \setminus N(X)$ 捏成 $Y$: 在新的图中，如果原本存在点 $Z \in N(X) \setminus N(Y)$ 满足 $\text{All } Z \text{ are } W$, 则加上边 $\text{All } X \text{ are } W$
5. 新的图有模型 $\mathcal{M}_0$, 另外 $\mathcal{M}$ 诱导了 $N(X) \setminus N(Y)$ 上的模型 $\mathcal{M}_X$ 和 $N(Y) \setminus N(X)$ 上的模型 $\mathcal{M}_Y$, 让它们使用不同的集合元素
6. 现在，对一个 $Z$, $I(Z)$ 初始取为 $\mathcal{M}_0$ 中的，如果 $Z \in N(X)$ 再并上 $\mathcal{M}_X$ 中的，如果 $Z \in N(Y)$ 再并上 $\mathcal{M}_Y$ 中的

这里需要解释为什么总是可以在一组前提集上构造模型，特别是对于 $\mathcal{M} _0$ 的构造，要求对 $X, Y$ 满足没有 $\text{All } Z$ 是它时 $I(X), I(Y)$ 无交。我的想法是，对于有限情形，略去不看 $\text{All}$-有向边成圈的情形，此时先赋值 $I(Z) = \emptyset$, 然后对所有 $\text{Some}$-无向边，填入新元 $\star _w$ 然后向上传递，可以在有限步停止。对于一般的可数情形，考虑把一些名字捏成相同的并 $\bigcup _{\text{finite } S} \\{\star _{w _S}\\}$, 按照某个点是否包含某个 $\star _w$ 看成赋值，使用紧致性定理的方法（大意是归纳，总有一半满足对所有有限子集存在赋值成立）。

以上过程省略了大量细节，暂时没有完全验证。
