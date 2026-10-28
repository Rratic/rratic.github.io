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

另有一些基本的观察：某些记号构成的串是有意义的句子；有意义的句子有有意义的部分；有意义的句子有特别的意义（“mean something in particular”）；有能力的语言使用者能够不费力、瞬间地理解许多该语言的句子。

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

如此，形如 $a = b$ 的命题是有信息量、有认知价值、后验的；而同一性/等同性（identity/equality）命题反之。如果同一性/等同性只是对象与自身的关系，那么在 $a = b$ 为真的前提下，无法解释为什么 $a = b$ 比 $a = a$ 认知价值更高。而如果同一性/等同性只是名称/对象的标记的关系，则无法解释为什么有 $a = b$ 是具有认知价值的。由此提出一个双层的理论。

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
### 单称词项与摹状词
Russell 认为摹状词可分为两种，**限定性摹状词**（definite description）形如“那个某某”（the so-and-so），非限定摹状词（indefinite description）形如“一个某某”（a/an F）。

注意有些限定性摹状词可能不含 `the`，如“his book”，这可改写为“the book of him”；含 `the` 的表达式不一定是摹状词，如“the Holy Roman Empire”，更应当是专名。

Russell 强调一种现实感，独角兽、金山、圆的方等“不真实的对象”不应被当作命题成分。命题“$x$ 是非真实的”只有在 $x$ 是一个摹状词时才有意义。这样，“独角兽不存在”句子有意义且为真（在 Frege 理论中真值空缺），断言不存在满足“独角兽”描述条件的对象，而不是谈论一个叫“独角兽”的对象并赋予它“不存在”性质。

### 标准的理论
句子“an object having the property $\varphi$ has the property $\psi$”中 `a` 只要求存在性，可以理解为：

> The joint assertion of $\varphi_x$ and $\psi_x$ is not always false.

句子“The F is G”中 `the` 要求唯一性（至少一[^winged-horse]且至多一），可以理解为：

$$\exists x (F_x \land \forall y(F_y \to y = x) \land G_x)$$

如此、表面上像主词的摹状词，实际上被消解为存在量词、唯一性条件和谓词条件。

### 专名与摹状词的差异
Russell 区分：名称是简单符号，直接指称某个对象，意义不依赖于其他词的意义；摹状词：由多个已有意义的词组成，其逻辑作用来自这些词的组合。

故，“Scott 是 *Waverley* 的作者”不能简单等同于“Scott 是 Scott”，前一句包含存在性、唯一性和作者身份条件；“*Waverley* 的作者是 *Waverley* 的作者”也有信息，因为不存在现任法国国王时，“现任法国国王是现任法国国王”在 Russell 的分析中为假。

于是体现了一个方法论：在进行句子的语义/逻辑分析（semantic/logical analysis）时，不应该不加反思地使用其表面语法形式（surface/apparent grammatical/syntactical form）。

### 组合性原则与语境原则
Frege 的语境原则说，不能孤立地追问词的意义，只能在命题语境中理解词。Russell 的理论中摹状词的贡献要在完整句子中展开体现了这一点。

在现代形式语义分析中，一般仍用组合性原则，因为语境原则难以贯彻。

### 摹状词理论的形而上学应用
在形而上学中，Russell 的分析解决了三个经典问题：
- 对不存在对象的显示指称问题
- 否定存在命题问题
- 排中律问题：在 $F$ 不存在时“The F is G”与“The F is not G”都不真，但这里存在辖域歧义的问题（否定在摹状词内部或谓词内部而不是命题外部），因此要区分主要出现（primary occurrence）与次要出现（secondary occurrence）；从物（de re）与从言（de dicto）

### 摹状词理论的语言哲学应用
在语言哲学领域，Russell 针对非摹状词相关的语言哲学现象作出了回应。在说 Homes 是否存在时，Homes 是被缩写的摹状词，是“the author of the *Iliad* and the *Odyssey*”的缩写，同样的考虑可以应用到其它看起来是专名的地方。Russell 的 Name Claim 认为，日常专名不一定是真正的（Mill 式）名称，而可能是被伪装或缩写的摹状词。

这一主张与摹状词理论是独立的。Kemp 解读为：某个主体在某次使用或理解一个专名时，会把它当作某个摹状词的缩写，故不同主体、不同场合可能使用的是不同摹状词。

Searle 认为，核心的问题是：“专名在我们的语言中独特的作用是什么”。与指示代词（demonstratives）相较，专名的指称不需要预设任何设定或者特殊的语境条件（“refers without presupposing any stage settings or any special contextual conditions surrounding the utterance of the expression”）；与摹状词相较，专名一般不规定（“do not in general specify”）其指称的对象的任何特征，尽管如此，我们预设了某些特定的描述陈述的真，尽管无法标明是哪些陈述。

这里预设（presupposition）是指为了让论证有效/立场站得住脚而必要的被忽略的前提或背景框架。Blackburn 更正式地将其定义成，一个对另一个陈述的真、假必要的命题的真性（“a proposition whose truth is necessary for either the truth or the falsity of another statement”），故，如果 $p$ 预设 $q$，则 $p$ 真或假必须 $q$ 为真。

Searle 的专名的簇理论认为，专名与一簇描述相联系（如“亚里士多德”可能联系“柏拉图的学生”、“《形而上学》的作者”、“逍遥学派创始人”等），专名不是作为描述起作用，而是提供一个“挂钩”（peg），让我们围绕对象附加描述。这种松散性正是分开语言的指称功能与描述功能的必要条件。

Lycan 对 Searle 簇理论的解读中，这种联系是模糊（vague）[^vague]的，这正是我们拥有并使用专名而不只摹状词的原因。

### 摹状词理论的知识论应用
**亲知知识**（knowledge by acquaintance）是主体能够直接把握的东西，这只包含感觉材料（sense-data）、共相（universals）与自我（the self）。Russell 的亲知原则认为，任何能够被理解的命题必须完全由主体亲知的实体构成。

假设某人看到一张杂乱的桌子并说“这张桌子杂乱”，则我们实际上是经由感觉材料去讨论桌子（因此可能对桌子存在性出错但不对感觉印象出错），想的是：

> The external cause of THIS (table-shaped batch of sense-data) is cluttered.

存在一个唯一的对象，它是 THIS 的外在原因，并且它是乱的。

假设两个人站在一张桌子前，一人说“这张桌子是圆的”。设这个人相应的感觉材料 $\varphi$，另一个人相应的感觉材料 $\psi$，则情况是：
- 第一个人想：“$\varphi$ 的外在原因是圆的”
- 第二个人想：“$\psi$ 的外在原因是圆的”
- $\varphi$ 的外在原因等于 $\psi$ 的外在原因

我们都不直接亲知对方的感觉材料，故不能直接把握“$\varphi$ 的外在原因等于 $\psi$ 的外在原因”这一命题。但我们同时有描述知识（knowledge by description）：那个桌子存在。故我们都知道存在一个单称命题“$a$ 是圆的”，但我们不能亲知 $a$，不能把握这个命题。

由此种图景，Russell 被认为是经验主义者。与 Frege 更重视公共的涵义结构相较，Russell 把对象知识和逻辑分析建立在亲知与描述的区分上。

### Strawson 的批评
Russell 的理论是一个纯语义的理论，忽视了说话者、使用场合和语用行为。Strawson 认为，指称不是表达式与对象之间抽象的二元关系，而是某人在特定时间、场合使用表达式所完成的行为（an act done by a person at a time on an occasion）。

回忆不存在 $F$ 时“The F is G”是有意义而否定的命题。Strawson 的分析是：说话者使用了一个表面上像是指称表达式的说法，但根本没有成功指称任何东西，因而这句话是不完整、夭折（abortive）的，谈不上真假。

Strawson 进一步提出，存在 $F$ 是“The F is G”的预设。如果预设失败，话语可能没有真假值。Blackburn 指出，这一文本解读有“说话者没有作出陈述”与“陈述的真值空缺”两个可能，两种理论都有代价，对于限定摹状词这类例子，Russell 的处理更为省事，既不真又不假的直觉可以理解为会话含义（implicature）的效果。Strawson 的替代形式化方案为：$\iota_x(F_x) \bullet (G_x)$.

另外，Strawson 认为 Russell 的分析漏掉了语境显著性（context salience）。“The table is covered with books”说的是语境中显著、可识别、说话双方都知道的那张桌子，唯一性相对于语境而不是整个世界。

### Donnellan 的调和
Donnellan 认为限定性摹状词的用法有双重功能：
- 归属性用法（attributive use）：说话者并不预先指向某个特定对象，而是断言“满足该描述的对象具有某性质”
- 指称用法（referential use）：说话者使用摹状词把听者的注意力引向某个特定对象；即使描述严格来说不适用于该对象，听者仍可能成功理解说话者在谈谁

假设在聚会上看到一个手里拿着酒杯的人，某人说“那个男人喝醉了”。如果说话者只是借“那个男人”让听者找到眼前的某个人，即使那个人其实是女性，听者仍可能知道说话者指的是谁；这是指称用法；如果是说“无论谁是那个唯一的男人，他都喝醉了”，则接近归属性用法。“杀害了 Smith 的人是疯子”同理。

Donnellan 主张，两种用法的差异不在句法或语义层面，而是取决于说话者的意图，在语用上有歧义。

### Kripke 的区分
Kripke 作出了语义学指称（semantic meaning）与说话者指称（speaker meaning）的重要区分。前者是：语言约定与语境共同确定某场合中的语义内容；后者是说说话者说特定词想表达的含义。

可以说，在给定的个人语型中，前者是一般的意图，后者是特定的意图。

## Kripke 论日常专名

---

[^idea-theory-of-meaning]: 解释成纯观念其实应该是可以的，但我们希望发展一点应用的理论。
[^intensional-content]: 在可能世界语义学中，可理解为从可能世界到外延的函数。
[^principle-of-charity]: “总有大儒为我辩经。”
[^winged-horse]: 在 Russell 看来“The winged horse is winged”假。直觉上可能认为其真，解释为把它放在虚构语境中。
[^vague]: 不能像查字典排除歧义一样用技术手段澄清。
