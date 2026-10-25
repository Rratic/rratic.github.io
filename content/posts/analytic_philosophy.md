+++
title = "【草稿】《分析哲学概论》笔记"
date = 2026-10-25

[extra]
math = true
mermaid = true

[taxonomies]
categories = ["知识", "课程"]
tags = ["哲学"]
+++

<style>
blockquote em {
    font-style: normal;
    text-decoration: underline;
}
</style>

想要精准刻画什么是分析哲学比较困难（“分析哲学作为一种解释意义上的历史（‘An interpretive history’），其统一性也许只是错觉（‘illusion of unity’）”）。其大概分支有语言哲学（philosophy of language）、形而上学（metaphysics）、心灵/心智哲学（philosophy of mind）、知识论/认识论（epistemology/theory of knowledge）、哲学逻辑（philosophical logic）、伦理学（ethics）、政治哲学（political philosophy）、美学（aesthetics）等。

其大致是在：提出最普遍性/一般性/关于最根本对象的问题；探求合理信念之间的关系；检验合理信念的根据；而后形成合理的、融贯的（coherent）、一致的（consistent）、可辩护的（justifiable）信念系统。因此，其中一个重要特征是重视常识。

## Frege 论涵义与指称
### 语言哲学
语言大概是：有能力的使用者知道并使用的符号体系（a system of symbols, which competent speakers/users know and use）。

语言哲学关心：**语义**（meaning/semantics）、**语用**（linguistic uses/pragmatics）如反讽现象、**语力**（linguistic forces/performances）如某人声称渴时用“桌子上有一瓶水”表达许可、规范性（normativity）、规则（rules）等。

先来区分几个重要的术语：
- **类型**（type）的实例称为**殊例**（token），如“The cat is on the mat”有 6 个 word token，5 个 word type
- **使用**（use）与**提及**（mention）：提及某表达是讨论该表达本身，如“‘Is’ is a verb”
- **元语言**（meta-language）是用来讨论**对象语言**（object language）的语言，作出此区分就可解决说谎者佯谬
- **句子**（sentence）是语言系统中的物理序列，**陈述/断言/话语**（statement/assertion/utterance）是特定时间特定地点进行的一个人类行为；**命题**（proposition）是句子的语义内容；我们说：
> When a person utters/asserts a sentence, she/he thereby make a statement.

### 涵义
我们观察到：一些假的句子可以被理解，因此我们要区分真值、真值条件与陈述的验证。

另有一些基本的观察：某些记号构成的串是有意义的句子；有意义的句子有有意义的部分；有意义的句子有特别的意义（mean something in particular）；有能力的语言使用者能够不费力、瞬间地理解许多该语言的句子。

Locke 认为，语言是为了表达思想的外在可感知记号（涵义的观念理论[^idea-theory-of-meaning]）：

> Man, though he have great variety of *thoughts*, ... yet they are all within his own breast, invisible, and hidden from others, ... The comfort and advantage of society not being to be had without *communication of thoughts*, it was necessary, that man should find out *some external sensible signs*, whereby those *invisible ideas*, which his thoughts are made up of, might be made known to others.

然而观念是私人、不可见的，这无法解释不同人可以用语言交流的问题。

The Referential Theory 认为，
- linguistic expressions have the meanings they do because they stand for things
- what they mean is what they stand for

然而，其在辖域上有“不存在的事物”、组合关系、无穷倒退问题、一些名词副词自身不代表特定事物的反驳；无法同时解释有意义的句子成分和看似句子却无意义/不合语法的符号串；有 Frege 之谜的反驳。

### Frege 之谜
我们知道晨星（Phosphorus）与昏星（Hesperus）说的是一个东西，即金星。于是可以考虑以下命题对比：

| $a = b$ | $a = a$ |
| :-----: | :-----: |
| 晨星是昏星 | 晨星是晨星 |
| 点 $B$ 等于点 $A$ | 点 $B$ 等于点 $B$ |
| 线 $a, b$ 交点等于线 $b, c$ 交点 | 线 $a, b$ 交点等于线 $a, b$ 交点 |

这里我们区分内涵内容（intensional content）与外延内容（extensional content），前者大致是“决定其外延的方式”[^intensional-content]，后者是在真实世界中对应的对象/集合/真值。

如此，形如 $a = b$ 的命题是有信息量、有认知价值、后验的；而同一性/等同性（identity/equality）命题反之。如果同一性/等同性只是对象与自身的关系，那么在 $a = b$ 为真的前提下，无法解释为什么 $a = b$ 比 $a = a$ 有更多的信息。而如果同一性/等同性只是名称/对象的标记的关系，则无法解释为什么有不同的认知价值。由此提出一个双层的理论。

### 涵义与指称
Frege 的意义理论中，一个符号有两层语义因素：
- **指称**（reference/Bedeutung）是该符号所指向的对象（referent/object）
- **涵义**（sense/Sinn）中包含关于指称对象的呈现模式（the mode of presentation）

所以可以画关系为：

{% <mermaid> %}
graph LR;
    A["单称词项（singular term）"]
    B["涵义（sense）"]
    C["指称物（reference）"]

    A -->|"表达（expresses）"| B
    B -->|"决定（determines）"| C
    A -->|"指涉（denotes）"| C
{% </mermaid> %}

其中 Frege 本人对指涉的用词是指称（refer），这有直接从单称词项到指称物的嫌疑，因此现代更标准化的使用方式是**指涉**（denotes）。然而这一用词有倾向外延主义语义学之嫌，因此也有用中性的代表（stand for）、标示/指称（designate）。

这里的“呈现模式”不应简单理解为一个人脑中的图像或观念。John Perry 将它解释为一种内涵性的函数或规则：给定某些参数，通过某种方式确定一个对象，如几何中，“$a$ 与 $b$ 的交点”是一种确定点的规则。

### 专名与单称词项
Frege 关于**专名**（proper name）术语的使用是：

>  “By ‘*sign*’ and ‘*name*’ I have here understood any designation representing a proper name, which thus *has as its reference a definite object* (this word taken in the widest range), but *not a concept or a relation*, which shall be discussed further in another article. The designation of a single object can also consist of several words or other signs. For brevity, let every such designation be called a proper name.”

即，整体上指称一个确定对象的表达式。

**单称词项**包括：
- 专名，如“Jane”、“Winston Churchill”、“3”
- 限定摹状词，如“the Queen of England”、“the cat on the mat”
- 人称代词，如“you”、“she”
- 指示代词，如“this”、“that”
- 复杂指示语，如“this book”、“that table”
- ……

在日常语言中（定义个人语型（idiolect）：由一个人或一个群体使用的语言），对于“亚里士多德”之类的真正专名，人们对它的涵义的理解会出现分歧，如有人理解为“柏拉图的学生和亚历山大大帝的老师”，另一人理解为“出生于斯塔吉拉的亚历山大大帝的老师”；只要指称保持同一，涵义上的这些分歧是可以允许的（但严格的科学语言中应避免，理想语言中不应出现）。

涵义与知识状态也有联系：熟悉相关语言的人可以把握专名的涵义，但这只照亮了指称对象的一个方面，关于对象的完备知识要求能够直接说出给定的涵义是否属于它，而这是永远无法达到的。

合语法的、代表专名的表达式总是有涵义，但涵义不一定有指称，如“收敛最慢的级数”、“以太”。

### 与观念之别
观念（idea）是个人心理中的内部图像（internal image），而不是涵义。Frege 使用望远镜的类比：观念是心灵中的；涵义是呈现在望远镜目镜上的图像；指称物是天体本身。

与 Locke 的观念理论相比，Frege 的理论中同一个涵义可以被不同人把握，于是有了公共、可交流的思想内容。

一个观念主义者/怀疑论者可能反驳：在说“月亮”时，如何知道名称“月亮”有指称物？Frege 的回应是，我们说“月亮”并不是想谈月亮的观念，而是预先假设了一个指称物存在。我们说“月亮比地球小”而不是“我的月亮观念比我的地球观念小”。

### 陈述句
一个完整的陈述句包含一个**思想**（thought）。如果把句中一个词替换为指称相同、涵义不同的词，句子的指称不会变，故思想应当是句子的涵义。

一些陈述句有真值，此时句子的指称就是这个真值。而 Frege 理论中陈述句也可能出现真值空缺（truth-value gap），这可能来自于：无指称的单称词（“现任法国国王是秃头”，但没有现任法国国王）、范畴错误、前提未满足、歧义与含混等。

这会产生奇怪的后果：所有真句在指称层面完全相同，所有假句也完全相同。故而我们不能只关心指称。

判断（judgement）可以理解为从思想到真值的推进。

### 组合性原则
我们给出三种组合性：
- 指称的组合性：句子的真值由各部分的指称（及句子的组合方式）决定
- 涵义的组合性：句子的思想由各部分的涵义（及句子的组合方式）决定
- 句法的组合性：句子的结构由各部分的句法性质（及句子的组合方式）决定

![组合性原则](/images/misc/2026_10_26.png)

### 反思
关于早期、后期 Frege 的区分，惯常、直接的涵义与指称的区分略去。

对于观点，可以采取宽容/善意原则（Principle of Charity）[^principle-of-charity]: 

{% <quote by="Humanity, principle of; Radical interpretation (Tanesini 2007, 23)"> %}
A principle to which, according to Davidson, we appeal when interpreting the words of other people. We apply charity to their pronouncements by assuming that they hold beliefs that are mostly true. Thus, we interpret other people’s words in a way that maximises truth. Thus, if one says, ‘That saucepan has a teflon coating’, and I see that the person is gesturing towards a frying pan, I would normally take that person to mean frying pan by ‘saucepan’. That is to say, I would normally apply the principle of charity and take that person to be saying something which is true.
{% </quote> %}

## Russell 的摹状词理论

---

[^idea-theory-of-meaning]: 解释成纯观念其实应该是可以的，但我们希望发展一点应用的理论。
[^intensional-content]: 在可能世界语义学中，可理解为从可能世界到外延的函数。
[^principle-of-charity]: “总有大儒为我辩经。”
