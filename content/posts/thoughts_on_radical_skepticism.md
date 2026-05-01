+++
title = "关于怀疑论的若干衡量"
date = 2026-05-01

[extra]
math = true

[taxonomies]
categories = ["杂物"]
tags = ["哲学"]
+++

起因是我舍友这学期选了哲学导论，谈及此。这也许会是一篇需要长期更新、反复修缮的文章。讨论的核心是这个具有基础性的问题：*如何看待*怀疑论？

我个人认同强怀疑论，但似乎大部分人并不如此。

<!-- more -->

---

先从我对哲学的*图景*至今的印象开始（这不包括我认为应该放在数学的逻辑学部分外）。我观察到人们这样学哲学：学习历史上的各个定义（其中一些确实相当复杂）、观点、反驳，以及反驳的反驳。人们写哲学文章时，也常沿用这套路径。

总之，除了少数我认为确实称得上*真知灼见*的观点外，我看到的哲学，常常像是在一个混沌的 word vector[^word-vector] 空间里施展某种*巫术*。其中一部分当然有趣[^magic]，但我们似乎并未达成什么*真正*的成果[^progress]，因为连许多关键词汇都没有公认定义。对此常见的回应是：即便没有完美结论，讨论本身仍会让我们更谨慎，也更能理解问题的真实难度；不必过度追求精确，也可能抵达某种深刻。[^witt]

但我们也可以从根本处发问。比如 Problem of other minds（他心问题）：我们如何知道他人确实有与我们相似的心智，而不是只是恰好表现得“像是能理解”呢？也许人类的生理机制会把经验整合成一种*内在*倾向，使我们*相信*他人有心智；可我们真的能断言，哪怕极小概率的反例都不可能发生吗？

推而广之，就有怀疑论。我们来看 [Wikipedia 下 Philosophical skepticism](https://en.wikipedia.org/wiki/Philosophical_skepticism) 给出的定义：

> It differs from other forms of skepticism in that it even rejects very plausible knowledge claims that belong to basic common sense. Philosophical skeptics are often classified into two general categories: Those who deny all possibility of knowledge, and those who advocate for the suspension of judgment due to the inadequacy of evidence.

这里我选取强怀疑论的一个版本：

{% admonition(type="definition", title="Radical skepticism 强怀疑论") %}
> It is impossible to have certainty in knowledge.
{% end %}

这里“知识”暂按经典的“得到辩护的真信念”来理解；“确定”则指 indubitability / infallibility / inimitability / indefeasibility[^certainty] 的某种混合要求。

我*直觉*上认同这一点。

以下对一些反驳作出回应：

{% admonition(type="question", title="可能的反驳") %}
像 1 + 1 = 2 这种东西，是我们能够确定的知识，不是吗？
{% end %}

首先，当我们说 1 + 1 = 2 时，总是依托某种背景：比如把它放在含 Peano 公理的形式系统里，或者放在更大的集合论框架里，或者干脆把 1 + 1 = 2 自身当作公理。此时我们的意思是：在该系统内，按既定规则推演是正确的。

此正确性是*可以被接受*的，但是如果审慎地想，在我们认为 1 + 1 = 2 是确定的知识之前，我们先认同了“规则”与“推演”是如何运作的，但是为什么这样的运作是能够被接受的呢，我们似乎给不出理由（有时人们声称，可以找一个更大的模型，证明可靠性与完全性，但是那个更大的模型又为何可以被接受？以及我们为何接受这种范式？），而只是直觉地认为如此。

此外，我们如何确保整个过程中神经系统没有出错？即使我们对照他人结果，发现许多信任的人都认同“当 1 + 1 = 2 时，1 + 1 = 2”，这除了并不足以带来“确定性”外，也仍无法排除我们在对照环节本身出错。

{% admonition(type="question", title="Pragmatism") %}
考虑实用主义，在实践中，我们认同那些知识是*有用的*。
{% end %}

我承认这一点，但是现在我并不是在讨论实用性。

{% admonition(type="question", title="可能的反驳") %}
你的论述本身就承认了某些确定的知识的存在。例如说你至少预设了最基础的逻辑的存在。
{% end %}

我的论述只是自然语言，甚至只是一个字符串。它只是向读者传达想法的工具，只需完成这项功能；至于它依赖的“逻辑”等预设是否构成确定知识，并不是这段文字必须先行解决的问题。

{% admonition(type="question", title="可能的反驳") %}
那强怀疑论本身不是你能确定的知识吗？
{% end %}

我只是声称“我*直觉*上认同这一点”。

---

如果文章到这里就结束，确实意义不大，也容易被看成无理取闹。于是我去看了 Philosophy StackExchange 上的问题 [How to avoid radical skepticism?](https://philosophy.stackexchange.com/questions/134884/how-to-avoid-radical-skepticism) 及其回答。

先来看看这个回答：

> Everyone from Pyrrho to Robert Audi has a solution. You can start by not reinventing the wheel, one of the advantages of attentive study. Consider any one of the strategies following from the WP article "[Philosophical Skepticism](https://en.wikipedia.org/wiki/Philosophical_skepticism)". But you have to find something that appeals to your intuitions.

这个条目中提及的 solution 包含：
- 对于 Pyrrhonian suspension of judgment 我承认可以暂停判断以换取内心平静（并且人们应该就是如此生活的），但这没有给出正面回应
- Mitigated skepticism / Fallibilism 主张我们仍可拥有可错而可靠的 knowledge（或 virtual knowledge）。我能理解它的实践价值；不过如果我们讨论的是“确定知识”，那么它等于改变题目：把“确定”降格成“足够好”，而不是证明“确定”存在。
- Foundationalism (basic beliefs) 想用“基础信念”终止无限追问，但为什么这些基础可以被接受是无法被回答，而我原先直觉接受强怀疑论在结构上与之相同
- 我完全不认可 Common-sense anti-skepticism 
- 对于 Kantian strategy (secure empirical knowledge by limiting knowledge claims) 的回应参考我对第一个可能的反驳的回应
- Methodological/pragmatic response (treat skepticism as a tool, not a final worldview) 并没有回答我的问题

> However, if you're looking for something sophisticated and contemporary, consider the arguments put forth for [process reliabilism (IEP)](https://iep.utm.edu/reliabilism/#H2) which essentially puts forth the following claim:
> 
> > Process reliabilism, by contrast, asks whether the general belief-forming process by which S formed the belief that p would produce a high ratio of true beliefs to false beliefs.

随后还提及了 [constructive empiricism (SEP)](https://plato.stanford.edu/entries/constructive-empiricism/) 与 [pragmatism (IEP)](https://iep.utm.edu/pragmati/) 等路径。这里不妨看被采纳回答的第一部分：

> Radical scepsis is not necessarily bad or something to avoid. Why would you need to avoid it? If you conclude by some reasoning, that appears to be valid to you, that all the voices in a dispute express opinions that are not tenable or not completely convincing, then that conclusion is your new starting point. It may not enable you to take sides in the debate, or to judge who is right, but is that a bad thing? For one, it frees you up. It may show you that all the other participants seem to be missing something - even if you yourself don't have all the answers either. So, it may free you up to ask further questions.

我认为这是一个足够好的回应，也带着某种哲学的人文关怀。[^care]

---

如果文章到这里收尾，读者可能还是会觉得并无新意。因此我想补一个两周前学到的有趣结果（可在一定程度上对标弱怀疑论）：“凡为真者皆可被知”在 KT[^kt] 下会导出矛盾。

{% admonition(type="tip", title="不可知性的基础") %}
考察 Moore 句：

$$\varphi \coloneqq p \wedge \neg K_i p$$

那么 $K_i \varphi$ 在 KT 中不一致：
1. 假设 $K_i (\varphi \coloneqq p \wedge \neg K_i p)$
2. 对命题重言式用 NEC 有 $\vdash K_i((\varphi \coloneqq p \wedge \neg K_i p) \to p)$
3. 使用 2 与 K 公理与假设与 Modus Ponens 得 $K_i p$
4. 对命题重言式用 NEC 有 $\vdash K_i((\varphi \coloneqq p \wedge \neg K_i p) \to \neg K_i p)$
5. 使用 4 与 K 公理与假设与 Modus Ponens 得 $K_i \neg K_i p$
6. 对 5 用 T 公理得 $\neg K_i p$

这里第 3 步与第 6 步矛盾。
{% end %}

Fitch 1963 据此论证，“凡真者皆可知”在逻辑上是有问题的。

这给我们的启示是：面对一个哲学观点，除了看它“是否动人”，还要看它会导向怎样的 consequence. 因此，如果有人认可强怀疑论的反面，也需要去看那个立场的后果/代价是什么，在用理论刻画世界时，并不总是一厢情愿。

[^word-vector]: 指词向量（还需要加上根据语境的调整），这只是一种失去精确性的比喻。
[^magic]: [把哲学看作一种魔术](https://www.zhihu.com/question/1894013843518223384/answer/1899710938837414347)
[^progress]: 我承认确实在极大改变了人类社会，但我说的是*终极意义上*的成果。
[^witt]: 或许我应该用的 reference 是维特根斯坦《哲学研究》？
[^certainty]: [Certainty (Stanford Encyclopedia of Philosophy)](https://plato.stanford.edu/entries/certainty/)
[^kt]: 参考[【逻辑学】模态逻辑及其应用](@/posts/logic_3.md) 中“性质对应”表格及“极小系统”部分。
[^care]: 即使哲学没有人文关怀，想必我从其中强行索取人文关怀也无人在意。
