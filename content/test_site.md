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
封面图来自 [Shadertoy: Planetary gears](https://www.shadertoy.com/view/MsGczV)

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

```rs
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

<style>
.notifications-container {
	width: 320px;
	height: auto;
	font-size: 0.875rem;
	line-height: 1.25rem;
	display: flex;
	flex-direction: column;
	gap: 1rem;
}

.flex-box {
	display: flex;
}

.flex-shrink-0 {
	flex-shrink: 0;
}

.alert {
	background-color: rgb(254 252 232);
	border-left-width: 4px;
	border-color: rgb(250 204 21);
	border-radius: 0.375rem;
	padding: 1rem;
}

.alert-svg {
	height: 1.25rem;
	width: 1.25rem;
	color: rgb(250 204 21);
}

.alert-prompt-wrap {
	margin-left: 0.75rem;
	color: rgb(202 138 4);
}

.alert-prompt-link {
	font-weight: 500;
	color: rgb(141, 56, 0);
	text-decoration: underline;
}

.alert-prompt-link:hover {
	color: rgb(202 138 4);
}
</style>

<div class="notifications-container">
	<div class="alert">
		<div class="flex-box">
			<div class="flex-shrink-0">
				<svg aria-hidden="true" fill="currentColor" viewBox="0 0 20 20" xmlns="http://www.w3.org/2000/svg" class="h-5 w-5 alert-svg"><path clip-rule="evenodd" d="M8.257 3.099c.765-1.36 2.722-1.36 3.486 0l5.58 9.92c.75 1.334-.213 2.98-1.742 2.98H4.42c-1.53 0-2.493-1.646-1.743-2.98l5.58-9.92zM11 13a1 1 0 11-2 0 1 1 0 012 0zm-1-8a1 1 0 00-1 1v3a1 1 0 002 0V6a1 1 0 00-1-1z" fill-rule="evenodd"></path></svg>
			</div>
			<div class="alert-prompt-wrap">
				<p class="text-sm text-yellow-700">
					Earth Online v1.0 just crashed!<br>
					<a class="alert-prompt-link" href="https://uiverse.io/kennyotsu/fast-emu-70">Click here for more details.</a>
				</p>
		</div>
	</div>
	</div>
</div>

<p>
	<iframe width="640" height="360" frameborder="0" src="https://www.shadertoy.com/embed/MsGczV?gui=true&paused=true&muted=false" allowfullscreen></iframe>
</p>

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
{% mermaid() %}
graph LR;
	赤狐-->乙木;
	赤狐-->丙火;
{% end %}

{% admonition(type="tip", title="提示") %}
	{% admonition(type="warning", title="警告") %}
		您的想法已被删除。
	{% end %}
{% end %}

## Custom
### My Shortcodes
{{ todo() }}

{% quote(by="H. P. Lovecraft") %}
That is not dead which can eternal lie,\
And with strange aeons even death may die.
{% end %}

{% quote(by="《尚书·周书·洪范第四》") %}
水曰润下，火曰炎上，木曰曲直，金曰从革，土爰稼穑。
{% end %}

{% shell(text="What is a **fish** without an eye?") %}
A **fsh**.
{% end %}

## #Test
### 解析
**谓词（predicate）**和**性质（property）**应该分别是粗体。

$1*2+3*4$

$$\left\{x \in A \middle| |x| = 1 \right\}$$

### 页面显示
$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f) = f((\lambda x. f(x x))(\lambda x. f(x x))) = f((\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x))) = f(f(Y f))$$

{% admonition(type="abstract", title="摘要") %}
$$Y f = (\lambda x. f(x x))(\lambda x. f(x x)) = (\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x)) = f(Y f) = f((\lambda x. f(x x))(\lambda x. f(x x))) = f((\lambda x. f(x x))(\lambda x. f(x x))(\lambda x. f(x x))) = f(f(Y f))$$
{% end %}
