+++
title = "推荐一个 Markdown 转 PDF 的流程"
date = 2026-02-23

[taxonomies]
categories = ["杂物"]
+++

有时我们想要排版出一份数学/物理试卷或者 cheatsheet, 可能涉及到数学公式、代码块，并以 PDF 形式给出，但是不希望使用麻烦的 PDF 编辑器或者 Word, 也不希望使用一整套 [pdftex](https://tug.org/applications/pdftex/). 此时可用此流程，只需用到 [VSCode](https://code.visualstudio.com/) 与一个浏览器。

<!-- more -->

## 流程
首先需要下载的 VSCode 插件是 [Markdown Preview Enhanced](https://marketplace.visualstudio.com/items?itemName=shd101wyy.markdown-preview-enhanced). 这个插件提供了相当丰富的功能，可参考其[文档](https://shd101wyy.github.io/markdown-preview-enhanced/#/zh-cn/)。

我们新建一个后缀为 `.md` 的文件，填入 [Markdown](https://docs.net9.org/basic/markdown/) 格式的内容（这支持代码块与使用 [KaTeX](https://katex.org/) 嵌入数学公式）。你可以在编辑框中看到对应的高亮辅助。

在编辑这个文件的状态下点击<kbd>鼠标右键</kbd>，在菜单中选择 "MPE:打开侧边预览". 结果如下：

![侧边预览](/images/misc/2026_02_23.png)

现在在侧边预览中点击<kbd>鼠标右键</kbd>，在菜单中选择 "Open in Browser" 或者选择 "Export" -> "HTML" -> "HTML (offline)" 然后手动用浏览器打开。此时打开的是你的 Markdown 内容生成的 [HTML](https://docs.net9.org/frontend/web_foundation/html/) 文件。

最后使用浏览器的打印功能，选择 "另存为 PDF" 即可。其它打印选项对于转 PDF 也是有效的。

## 修改 CSS
排版出的效果可能存在令人不满意的地方。例如说，引用的效果看起来很奇怪。由于我们是先生成 HTML 而且 Markdown 天然支持嵌入 HTML, 我们可以在文件的最开头加上：

```html
<style>
	html body blockquote {
		padding: 12px 12px;
	}
</style>
```

这些都可以自己调。特别地本文提供在一页上排版成两列的方法（这适用于 cheatsheet）。在开头的 `<style></style>` 中添上：

```css
.row {
	display: flex;
}

.card {
	position: relative;
    display: flex;
    flex-direction: column;
}

.pad {
	padding: 16px;
}
```

然后内容部分仿照这样写（前面的空可以省略）：

```html
<div class="row">

<div class="card">
	第一列的内容，这里直接放代码块

	<div class="pad">
	第一列的内容，文本
	</div>
</div>

<div class="card pad">
	第二列的内容，全是文本
</div>

</div>
```

甚至于因为是用浏览器打开的，读者可以嵌入可运行的 [Javascript](https://docs.net9.org/frontend/web_foundation/javascript/) 脚本来编辑内容。

## 打印控制
对一个较长的文章来说，另存为 PDF 时可能产生令人不适的分页。此时可以使用 [CSS 的媒体查询](https://developer.mozilla.org/zh-CN/docs/Web/CSS/Guides/Media_queries/Using)。在开头的 `<style></style>` 中添上以下内容，用于在单独的一、二、三级标题前分页，并阻止在图片内部分页：

```css
@media print {
	h1, h2:not(h1 + h2), h3:not(h2 + h3) {
        page-break-before: always;
    }

	img {
        page-break-inside: avoid;
    }
}
```

这些同样都可以自己调，例如要求表格内容的颜色在 PDF 中保留：

```css
@media print {
    table * {
        print-color-adjust: exact;
        -webkit-print-color-adjust: exact;
    }
}
```

## 目录控制
对于更长一些的文章（甚至是书）来说，可以使用创建目录 Table of Contents 的功能。在单独的一行写 `[toc]`, 将会在该处插入一个使用嵌套列表展示的目录。

为了便于管理，可以把不同章节的内容写在不同文件中，然后在主文件中使用 `@import "文件路径"` 来引入对应文件。具体规则可参考文档的[导入文件](https://shd101wyy.github.io/markdown-preview-enhanced/#/zh-cn/file-imports)一节。

主文件将会长成：
```md
# 大标题
[TOC]

@import "ch-01.md"
@import "ch-02.md"
@import "ch-03.md"
```
