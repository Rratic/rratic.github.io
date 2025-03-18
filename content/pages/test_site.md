+++
title = "功能测试"
date = 1900-01-01
updated = 2025-03-16

[extra]
comment = true
math = true
mermaid = true

[extra.cover]
image = "images/cover/gear.png"
width = 800
height = 450

[taxonomies]
categories = ["项目"]
tags = ["测试"]
+++

## Markdown
封面图来自 [Shadertoy: Planetary gears](https://www.shadertoy.com/view/MsGczV)

你的系统支持 *italic* 的 **FontFace**，并且你的浏览器（或者别的什么东西）~~继承~~了这一特性。

> 庄子将死，弟子欲厚葬之。\
> 庄子曰：“吾以天地为棺椁，以日月为连璧，星辰为珠玑，万物为齎送。吾葬具岂不备邪？何以加此！”\
> 弟子曰：“吾恐乌鸢之食夫子也。”\
> 庄子曰：“在上为乌鸢食，在下为蝼蚁食，夺彼与此，何其偏也。”\
> 以不平平，其平也不平；以不徵徵，其徵也不徵。明者唯为之使，神者徵之。夫明之不胜神也久矣，而愚者恃其所见入于人，其功外也，不亦悲夫！
> — 《庄子·杂篇·列御寇》

功能评注：
- `` `text` `` 会产生 `text` 的效果
	* 这个默认的 `<code></code>` 样式令人不悦，可以在 [LISP 模式](/pages/mode-lisp/) 找到一个自定义样式的展示。
		1. 使用 `code::before, code::after { content: "" !important; }` 覆盖前后的反引号。
		2. 使用 `p code { text-decoration: 3px gold underline; }` 制作高亮。
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
-let mut me = self.entry::<FreeWill>.mut();
-world.execute(me);
+if Some(mut me) = self.entry::<FreeWill>.get_mut() {
+	world.execute(me);
+}
```

## HTML
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

.flex {
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
		<div class="flex">
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

## KaTeX
$\mathbb{R}^{1,3} \rtimes \operatorname{SO}(1,3)$

## Mermaid

{% mermaid() %}

graph LR;
	赤狐-->乙木;
	赤狐-->丙火;

{% end %}

## Extensions
{% admonition(type="tip", title="提示") %}
	{% admonition(type="warning", title="警告") %}
		您的想法已被删除。
	{% end %}
{% end %}
