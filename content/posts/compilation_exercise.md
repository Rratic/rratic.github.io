+++
title = "记一个 Martin-Löf 类型论的实现"
draft = true

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["项目"]
tags = ["概述", "计算机"]
+++

参考资料包括：
- [北大编译实践在线文档](https://pku-minic.github.io/online-doc/#/)
- 完整项目 [Tiny MLTT](https://github.com/rycheung/tiny-mltt) 及对应的解释文章：动手实现 Martin-Löf 类型论 [1](https://zhuanlan.zhihu.com/p/1983946142866761398) [2](https://zhuanlan.zhihu.com/p/1984272006582117419) [3](https://zhuanlan.zhihu.com/p/2014486687678477470)
- How to implement dependent type theory [I](https://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/) [II](https://math.andrej.com/2012/11/11/how-to-implement-dependent-type-theory-ii/) [III](https://math.andrej.com/2012/11/29/how-to-implement-dependent-type-theory-iii/)
- [如何把模式匹配编译为归纳子](https://zhuanlan.zhihu.com/p/97737374)

代码仓库在 [Rratic/my-mltt](https://github.com/Rratic/my-mltt). 它的一个副产品是 [my-lam](https://github.com/Rratic/my-lam).

处理一个程序的主要流程包括：
- 词法分析器 Lexer: 将原始字符串读取为 Token 流，原理就是单纯的读字符作匹配，只需注意最大匹配原则
- 语法解析器 Parser: 将 Token 流解析为表面语法的抽象语法树，只需写出设计的语法的 BNF 范式然后翻译成代码
- 繁饰器 Elaborator: 转为核心语法
