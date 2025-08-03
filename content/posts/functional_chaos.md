+++
title = "基于 Lua 的模拟环境"
date = 2025-05-06
updated = 2025-08-03

[extra.sitemap]
priority = "0.7"

[taxonomies]
categories = ["杂物"]
tags = ["发布", "说明", "展示", "计算机", "含模拟", "在线", "Lua"]
+++

<style>
	table code {
		color: orange !important;
		border: 1px grey solid;
		padding: 4px;
		border-radius: 4px;
	}
</style>

[函数式混沌](/playground/chaos.html)

一个基于此想法但 API 不同的项目见于 [Milfoil](https://github.com/FoamWorld/milfoil).

## 介绍
一直以来都有一个横亘在我们眼前的难题，即如何弥合现实（这里指现实的离散、抽象部分）与模拟之间的鸿沟。

曾经语言学家认为可以通过设定规则把自然语言处理（Natural Language Processing）做好，然而对比统计方法，尽显其弊端：复杂性使规则难以穷举、歧义会引入新的复杂性、成本过高（如欧盟的 Eurotra 项目）。

然而我们有时还是希望确保一个模拟系统的准确性（我认为对于分而治之、菱形继承问题的处理、ECS 架构等等思想部分体现了这一点），乃至探讨引入特殊的、不同优先级规则的结果，因而我还是希望去逐步地设计一个这样的框架。

这个过程必然不是一蹴而就的，而且并不是封闭的，因此它必须有足够的灵活性。

我认为 [Lua](https://www.lua.org/) 具有相当的优势：
- 优雅
	- 语法可以简单地讲清
	- **函数是一等公民**
- 有足够的灵活性，主要体现在动态类型和 `table`
	- 没有原生的类，但可以通过 `setmetatable` 等模拟
	- 模块化的最佳实践也使用 `table`
- LuaJIT 足够快，相比 js 来说，提供原生整数类型更易令人接受
- 作为一个广泛存在的插件语言有足够的支持

## 技术
使用了 [fengari-web](https://github.com/fengari-lua/fengari-web) 直接加载嵌入在页面中的代码，它是基于 Lua 虚拟环境 [Fengari](https://fengari.io/) 和额外的 js 和 DOM 接口 [fengari-interop](https://github.com/fengari-lua/fengari-interop)。其支持的版本是 [Lua 5.3](https://www.lua.org/manual/5.3/manual.html)。

未来可能会采用 Rust + tauri 作为桌面端的渲染基础。

## 陈设
可以通过命令栏在虚拟环境中运行 Lua 代码。虚拟环境提供的唯一模块接口是 `commands`。

特别地，使用 `/preload` 注册的代码会在完整的环境（即 `_ENV`）中运行。

以 `/` 开头的命令，如 `/help`，会被自动替换为 `commands.help:run()`。

此外，提供了快捷键和亮暗色模式（跟随整个站点的设置 `localStorage["linkita-color-scheme"]`）。

## API
只提供部分文档，希望自行阅读[源代码](https://github.com/Rratic/rratic.github.io/tree/main/static/script/chaos)。

由于进行了模块化（尽管还不够完全），你可以随意地编写 mod 带来翻天覆地的改变，然后放到 `preload` 中。

### 节点控制 Nodes
`Nodes` 是两层的字典，一个节点是第一层 `Nodes.map[name]` 的值，一个结点是第二层 `Nodes.map[name][subname]` 的值，是一个函数。

| 方法 | 效果 |
| :-: | :-: |
| `Nodes.add(name: string, knots: dictionary<function>)` | 添加一组节点 |
| `Nodes.run(node: string, knot: string)` | 运行结点 |
| `Nodes.jump(dest: array<string>)` | 跳转到结点，若长为 1 则在当前结点下跳转，否则视作完整的路径 |

### 消息队列 Queue
每一条消息都有一个整数的重要性指标，存储在 HTML 标签的 `dataset["l"]`。

`Queue.clear(level: integer, level_decrease: integer)` 会遍历所有消息，直接移除重要性指标小于 `level` 的，并将所有重要性指标减去 `level_decrease`

| 方法 | 效果 | 重要性指标 |
| :-: | :-: | :-: |
| `Queue.push_line(string: string, type: string)` | 增加一行消息，可选类型 `markdown`（默认），`plain`，`html` | 1 |
| `Queue.push_info(message: string)` | 增加一行“信息”消息 | 1 |
| `Queue.push_success(message: string)` | 增加一行“成功”消息 | 2 |
| `Queue.push_warning(message: string)` | 增加一行“警告”消息 | 3 |
| `Queue.push_error(message: string)` | 增加一行“错误”消息 | 4 |

`Queue.push_choices(list: array<dictionary>)` 可以添加一组选项，每个选项有以下字段

| 字段 | 用于 | 默认 |
| :-: | :-: | :-: |
| `on: boolean` | 控制是否被使用 | 是 |
| `t: string` | 文本内容 | **必填** |
| `rm: boolean` | 选择后是否运行 `Queue.clear(1, 0)` | 执行 |
| `w: string` | 选择后显示的 Markdown 消息 | 无显示 |
| `cl: boolean` | 选择后是否运行 `Queue.clear(2, 1)` | 不执行 |
| `f: function` | 选择后额外调用的函数 | 无 |
| `j: array<string>` | 选择后调用 `Nodes.jump(j)` | 不执行 |
