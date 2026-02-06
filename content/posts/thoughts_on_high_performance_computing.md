+++
title = "关于高性能计算的混乱感想"
date = 2026-02-06

[taxonomies]
categories = ["杂物"]
tags = ["计算机"]
+++

注：此文主要为吐槽，缺乏实质性内容。大致是发现有一个第三届 PKU HPCGame 的活动可以参加，在其中读题所见的浮光掠影与感想。

<!-- more -->

回忆从这里开始：某个时刻我由于未知原因知道了 C++ 有一个标准库叫 `stdint`, 里面提供了 `int32_t`, `size_t` 这样的类型别名。于是我产生了一个想法：正确的编程语言的基本类型应该直接使用这样的带位数的名称而不是什么 `long` 之类的东西，在使用时应该仔细地选取到底需要多少位的类型。因此，像 Javascript, Lua 这种没有单独整数类型[^lua-integer-type]全部糊在一个“数字”类型里的语言就十恶不赦了。而直到我做 HPCGame 的题才知道世界上还有 `BF16`[^BF16], `NVFP4`[^NVFP4] 这样的*基本类型*存在，认识到大公司*呼风唤雨*的力量。[^swiss-table]

总之，我就跑去搜索微信公众号，大概使用了 int64 之类的关键词，然后搜到一篇介绍 Julia 语言的文章，于是我找到了其[中文文档](https://docs.juliacn.com/latest/)阅读，看到它优雅的语法设计和所谓“接近 C 语言的性能”感到大为愉悦。于是我认为这个语言会被越来越广泛的使用（显然是因为邪恶地希望我作为较早接触到的能够进行某种获利），开始尝试用它做各种事情，并且看到社区的一些帖子后自己做了[一套文档](https://learn.juliacn.com/)（深深地误解了 roadmap 一词的含义），因为认为当时用于文档生成的包 [Documenter.jl](https://documenter.juliadocs.org/stable/) 功能不够丰富，仿照它做了一个文档生成器（加了一些现在看来根本没必要的功能，而且无力维护、更新）。但直至今日，这个语言还只是作为科学计算/数值计算的可选项，在通用领域始终不温不火。为什么它各方面设计都更现代，且有较优的性能，却没有实现替代 Python 呢？它确实有设计不完美的地方（最糟糕的一点是数组下标从 1 开始，其次是不易静态编译），但更大的原因恐怕还是路径依赖——有太多的人习惯了 Python, 产生了太多的教程，甚至于由于历史的积累，添加了相当多的优化（而不是由更好的设计自然地达到）。同样出于路径依赖，人们还在广泛使用 QWERTY 键盘，浏览器上还在运行[^use-wasm]（并且做到了逆天的优化）不支持参数类型标注的 Javascript, 现行公历的缺陷无法被优化[^calendar]……[^idea-on-escape]而随着智能时代的来临，这样的问题可能变得更加坚固[^new-languages]。何时才能脱离这些*局部最小值*呢……反正到目前为止，我由于各种原因写过的 Python 代码应该不到 200 行。

这里我要继续称赞一下 Julia 类型系统的设计。在 Julia 中，存在抽象类型 abstract type 和具体类型 primitive type 的区分，一个类型可能是另一个类型的子类型（读者可理解成集合的包含）。例如 `Bool` 是 `Integer` 的子类型，而 `Integer` 是 `Real` 的子类型，往上还有，记作 `Bool <: Integer <: Real <: Number <: Any`. 实际上，所有类型都是 `Any` 的子类型，而 `Union{}` 是所有类型的子类型（术语是 bottom type, 但我不清楚有什么用）。另一方面，我们可以用 `typeof()` 获取一个值的类型，而类型本身也是有类型的（如 `typeof(Bool)` 与 `typeof(Any)` 是 `DataType`, 而 `typeof(DataType)` 本身也是 `DataType`[^union] [^type-theory-argue]）。

这个类型系统带来的最重要的事情是函数的设计：在 Julia 的概念中，一个函数 function 由多个方法 method 组成，每个 method 有不同的参数列表，而它们共用一个函数名。举一个例子（这种用法称为多重分派 multiple dispatch, 注意这里 `Val(:a)` 的类型是 `Val{:a}`, 所以做到了对不同的值选择实现）：

```jl
function action(content: AbstractString, mode: Any)
	# ...
end

function action(content: AbstractString, mode: Val{:plain})
	# ...
end

function action(content: AbstractString, mode: Val{:clone})
	# ...
end

function main()
	# ...
	mode = Symbol("mode_name_string")
	result = action(content, Val(mode))
	# ...
end
```

这就产生了一种效果：我可以规定默认的行为是什么，然后后来的人可以任意地拓展在一个特定情形下的不同行为，并且得到原生的支持。我不知道本质上发生的事情和自己维护一个映射/字典是否是一样的，但一旦有原生的内建的支持，就可以把优化问题留给语言开发者，而不是每个人有一套自己的并非 best practice 的实现。我不清楚 C++ 的虚表是否说的是类似的事情，而在 Rust 中我知道 bevy_reflect 库做了这样的事情，本质上是用宏简化了流程：

```rs
#[reflect_trait]
pub trait MyTrait {}
impl<T: Reflect> MyTrait for T {}
```

这个代码会生成一个名为 `ReflectMyTrait` 的类型，如果类型 `T` 实现了 `MyTrait` 这个 trait, 那么存在某种方式从 `T` 类型实例获取到 `ReflectMyTrait` 实例，从而调用对应的方法。这套东西就丑陋很多，并且我怀疑存在不必要的性能开销。

当然对整个 Rust（以及一些别的静态类型语言）来说一个很诱人的特性是所谓零成本抽象。这意味着我在它允许的范围内无论进行多少层抽象，做很多中间件，最终的效率和完全摊平成函数调用是一样的（我不清楚产生的二进制文件体积是否会偏大）。这样一来就可以在不追求底层优化的条件下只关心顶层的东西。对 Rust 来说写一个很长的 `#[derive(Copy, Clone, Eq, PartialEq, ...)]` 确实不舒服，而使用泛型，使用 `one(), is_zero()` 这种函数[^num-crate]倒是优雅且放心。

最近看到说 Reflection for C++26 给 C++ 添加了零成本的反射能力。[^cpp26-reflection]我怀疑本质上和 bevy_reflect 库的原理是一样的。举一个最简单的例子：

```cpp
constexpr auto r = ^^int; // 类型是 std::meta::info
typename[:r:] x = 42;
```

这已经不是我认识的 C++ 了……而我在 HPCGame 中看到的 C++ 代码虽然没有那么*新奇*，但同样地混乱，看起来到处是编译器开洞。例如说使用 OpenMP 进行并行化：

```cpp
#include <omp.h>

int main() {
	// ...
	#pragma omp parallel for
	for (int i = 0; i < 10000; i++) {
		a[i] = b[i] + c[i];
	}
	// ...
}
```

又比如说，把 OOP (Object-Oriented Programming) 思想的 AoS (Array of Structures) 布局改为 DOD (Data-Oriented Design) SoA 布局，有利于向量化（SIMD）和提升缓存效率。也许 DOD 能够整理出一套比 OOP 更符合直觉的体系，但在十年内，恐怕还是两种思想同时存在的局面（就好像 C++ 中有人用 `int` 那一套，有人用 `size_t` 那一套，有人用 `WORD` 那一套）。

在 C 题 AI 给我的回答[^ai-era]中我看到了这样的用法：

```cpp
struct alignas(64) MyStruct {
    double field_1;
	// ...
}; // 或者使用 `;`
```

这是在 C++ 11 引入的指定[对齐要求](https://cppreference.cn/w/cpp/language/object#Alignment)的说明符，这里的作用是把不足 64 字节的结构体强行填充到 64 字节。如果缓存行大小为 64 字节而结构体大小为 32 字节，一个缓存行中同时有两个结构体，使用多核加速时可能导致伪共享 false sharing, 不同 CPU 核心频繁修改缓存行中的不同变量，引发缓存同步开销。

在 `alignas` 出现之前可以用的写法是把最后一行 `}` 后改成 `__attribute__((aligned(64)));`, 这个修饰符应该是 GNU 系列编译器特有的功能。我之前也听说过类似的事（所谓 `__stdcall` 和 `__cdecl` 修饰符，这个我不知道对提升性能是否重要）。从这一点所见，写高性能程序确实混乱。

这里缓存行的例子实际上含有针对特定的 CPU/GPU 优化的意味。而硬件本身也在改变，如越来越不推荐不连续的内存访问[^linked-nodes]，甚至会带来严重的安全性问题。[^safety-bugs]

所以我认为理想的情况是：用一套 API 完成程序，然后这个程序可以被自动映射到在指定环境中的高效代码（包括分支预测的结果和对应的写死的优化，虽然这会导致分发编译结果很麻烦）。[^smart-pointers] [^infinite]但我怀疑即使如此，总会有人试图*暴力地*再增加一点点性能。有必要如此吗？HPCGame 前的讲座提到量化交易相当关心性能；或许有人还会说，恰恰是因为*暴力地*增加性能才有了分支预测之类的硬件发展，此外还带来了就业。但总之，无论如何我不会站在进行琐碎的工作这一侧。

当然，我不是信科的，所以如果我不走计算方向[^option-calculation]，这个事情和我也没有什么关系。更有关的如并行计算的设计（这里主要印象来自[上学期计算概论课程中的算法推导](@/posts/calculation_theory_final.md)，本质上是动态规划）是有趣但不知是否明朗的。或许 H 题的算子设计也是有趣的，但我没有耐心看了。

……混乱的感想就到此为止。无论其中的偏见程度多么严重，暂且让它留在这里……

---

[^lua-integer-type]: 实际上在 Lua 5.3 版本中引入了整数类型和浮点数类型的区分。
[^BF16]: Google Brain 团队提出的浮点数类型，具有和 32 位浮点数相同的数值范围（8 位指数位）但精度减小（7 位尾数位）。
[^NVFP4]: NVIDIA 提出的 4 位浮点格式，有 1 位符号位 + 2 位指数位 + 1 位尾数位。
[^swiss-table]: 之前还听说 Google 开发的性能更高的哈希表 Swiss Table 进入了若干个语言的标准库，但感受没有那么强烈。
[^use-wasm]: 如果大家都改用别的语言，运行 WebAssembly 就好一些了。
[^calendar]: 1923 年国际天文学联合会在国际天文学大会上提出“世界历”改革方案（一年分成 4 个 91 天的季，均以周日开始周六结束；额外的一天及闰年增加的一天置于年末，不属于任何一周），该方案之后也被多次提交，但未被通过。一部分阻力来自宗教。而数字时代使得这个改革变得彻底不可能。
[^idea-on-escape]: 实际上我有一个想法是修改一下字符串的 escape 规则，用 `\=` 而不是 `\"` 来表示双引号；用 `\-` 而不是 `\'` 来表示单引号。这样正则表达式、语法解析器会好写很多，不容易出错且运行一次或许能减少三四条 CPU 指令。不知道有多少人认可呢……
[^new-languages]: 有人悲观地说：在能够写代码的 LLM 出现以前没有火起来的语言，今后不会有机会火了。
[^union]: 实际上还有 `Union{A, B}`, `UnionAll` 类型这种细节，此处略去。
[^type-theory-argue]: 类型论认为一个类型的类型是自身会导致一些问题，因而引入了层级 level 的概念，但是我不是很明白在实际使用中到底有什么问题。
[^num-crate]: 这是 num 库里的，需要 use `num::traits::{One, Zero}`.
[^cpp26-reflection]: 看到的是知乎文章 [reflection for C++26 如期而至](https://zhuanlan.zhihu.com/p/1919860255870948496)。
[^ai-era]: 感到大模型会比我想象地深刻很多地改变这个世界…… 我不希望未来的一切建立在不确定上，但过往的一切同样是不确定的（硬件的原始物理错误率存在，只是整个系统的错误率以各种方式降低了，这里一个好的结果是 Shannon 的噪声信道编码定理），无话可说……
[^linked-nodes]: 这里看的是一个关于链表的知乎回答 <https://www.zhihu.com/question/1997631452192466334/answer/1999052955018629727>.
[^safety-bugs]: 看的是这个知乎回答 <https://www.zhihu.com/question/1989719718551122390/answer/1993038170217022411>.
[^smart-pointers]: 这个想法显然来自于 Rust 的使用所有权、生命周期和一套智能指针可以大部分时候不使用 `unsafe` 关键字安全地实现程序。
[^infinite]: 当然更理想的情况是无限的空间和无限的算力（比方说任何计算可以被巧妙地运行，使得在 1 秒内得到结果），但是这不现实而且会导致密码学必须重构。
[^option-calculation]: 大致听学长说是从机器解微分方程这种东西开始，考虑它的优化；之前在各种演讲、宣传中经常听说矩阵计算的算法优化，大概是 O(N^k) 的 k 的小数点后优化之类。
