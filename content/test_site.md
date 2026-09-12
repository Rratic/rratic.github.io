+++
title = "功能测试"
date = 1900-01-01

[extra]
math = true
mermaid = true
toc = true

[extra.cover]
image = "images/cover/gear.jpg"
width = 800
height = 450

[extra.sitemap]
priority = "0.0"
+++

## Zola
### Markdown
封面图来自 [Shadertoy: Planetary gears](https://www.shadertoy.com/view/MsGczV) 作品。

你的系统支持 *italic* 的 **FontFace**，并且你的浏览器（或者别的什么东西）~~继承~~了这一特性。

> 瞻彼阕者，虚室生白，吉祥止止。

功能评注：
- `` `text` `` 会产生 `text` 的效果
	* 默认的 `<code></code>` 样式令人不悦。
		1. 使用 `content: "" !important;` 覆盖前后的反引号。
		2. 使用 `text-decoration: 3px gold underline;` 制作高亮。
	* Markdown 源文件中的链接无法自动转化同样令人不悦。
- 这个列表的间距很好。

| 猫的类型 | 颜色 |
| :-: | :-: |
| 橘猫 | `#ffa940`[^1] |

[^1]: 采自 [Ant Design](https://ant-design.antgroup.com/docs/spec/colors-cn)

### Codeblock
```rs, linenos, hl_lines = 13
#[derive(Reflect, Clone, Copy)]
#[reflect(SerializeWithRegistry, DeserializeWithRegistry)]
struct ComponentTypeLink(pub TypeId);

impl SerializeWithRegistry for ComponentTypeLink {
	fn serialize<S>(&self, serializer: S, registry: &TypeRegistry) -> Result<S::Ok, S::Error>
	where
		S: Serializer,
	{
		let registeration = registry.get(self.0).unwrap();
		let info = registeration.type_info();
		let path = info.type_path();
		serializer.serialize_str(path)
	}
}
```

```diff
- let mut me = self.entry::<FreeWill>.mut();
- world.execute(me);
+ if Some(mut me) = self.entry::<FreeWill>.get_mut() {
+     world.execute(me);
+ }
```

```agda
module Agda.Builtin.Bool where

data Bool : Set where
  false true : Bool
```

### HTML
<p>按下 <kbd>Ctrl</kbd> + <kbd>Alt</kbd> + <kbd>Delete</kbd> 以结束会话。</p>

<p>
	<iframe width="640" height="360" src="https://lazyfly.me/iframe" allowfullscreen></iframe>
</p>

## Linkita
### KaTeX
$\R^{1,3} \rtimes \operatorname{SO}(1,3)$ 是一个 $\set{A_n}$ 的 $\cancel{\boxed{~}}$.

$$
\begin{Vmatrix}
   a & b \cr
   c & d
\end{Vmatrix}
$$

$$
\begin{CD}
   A @>a>> B \cr
   @VbVV @AAcA \cr
   C @= D
\end{CD}
$$

### Shortcodes
{% <mermaid> %}
graph LR;
	赤狐-->乙木;
	赤狐-->丙火;
{% </mermaid> %}

## Custom
### My Shortcodes
{% <tip> %}
以玉作六器，以礼天地四方：以苍璧礼天，以黄琮礼地，以青圭礼东方，以赤璋礼南方，以白琥礼西方，以玄璜礼北方。
{% </tip> %}

{{ <todo /> }}

{% <quote by="H. P. Lovecraft"> %}
That is not dead which can eternal lie,\
And with strange aeons even death may die.
{% </quote> %}

{% <quote by="《尚书·周书·洪范第四》"> %}
水曰润下，火曰炎上，木曰曲直，金曰从革，土爰稼穑。
{% </quote> %}

{% <shell text="What is a **fish** without an eye?"> %}
A **fsh**.
{% </shell> %}

## #Test
### 解析
**谓词（predicate）**和**性质（property）**应该分别是粗体。

$1*2+3*4$

$$\left\{x \in A \middle| |x| = 1 \right\}$$

### 页面显示
$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f) = f((\lambda x. f(x x))(\lambda x. f(x x))) = f((\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x))) = f(f(Y f))$$

{% <note> %}
$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f) = f((\lambda x. f(x x))(\lambda x. f(x x))) = f((\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x))) = f(f(Y f))$$
{% </note> %}
