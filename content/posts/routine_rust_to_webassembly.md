+++
title = "记 Rust 编译至 WebAssembly 流程"
description = "一个完整的极度简化和减少下载量的 Rust 编译至 WebAssembly 的流程。"
date = 2026-01-27

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["计算机"]
+++

本文假定读者已经熟悉 Rust 工具链，涉及的流程不会额外安装 npm 之类的工具链。

参考的文档是 [`wasm-bindgen` 指南](https://wasm.rust-lang.net.cn/docs/wasm-bindgen/introduction.html)。

---

首先需要安装：

```sh
rustup target add wasm32-unknown-unknown
cargo install wasm-bindgen-cli
```

现在切到一个文件夹下新建一个项目：

```sh
cargo new --lib example
cd example
cargo add wasm-bindgen
```

此时 `Cargo.toml` 中的内容大概形如：
```toml
[package]
name = "example"
version = "0.1.0"
edition = "2024"

[lib]
crate-type = ["cdylib"]

[dependencies]
wasm-bindgen = "0.2"
```

我们在 `./src/lib.rs` 中填入：
```rs
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn greet(name: &str) {
    alert(&format!("Hello, {}!", name));
}
```

现在可以运行：

```sh
cargo build --target wasm32-unknown-unknown --release
wasm-bindgen --out-dir ./dist --target web ./target/wasm32-unknown-unknown/release/example.wasm
```

这将在 `./dist` 下生成若干个文件。

现在添加一个 `index.html`，内容为：
```html
<!DOCTYPE html>
<html lang="zh">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
</head>
<body>
    <script type="module">
		import init, { greet } from './dist/example.js';

		async function run() {
			await init();

			greet('WebAssembly');
		}

		run();
    </script>
</body>
</html>
```

由于 CORS 限制，在本地测试时直接用浏览器打开 `index.html` 无法成功，需要启动一个 HTTP 服务器。

这里可以使用 miniserve 轻量级服务器：
```sh
cargo install miniserve
miniserve . --index "index.html" -p 8080
```

现在就可由浏览器打开 `http://localhost:8080` 看到正确效果。

---

现在看一个更完整一点的例子（读者自行修改 `index.html` 对应的调用）：
```rs
use wasm_bindgen::prelude::*;

// `start` 意味着这是初始化时会运行的函数
#[wasm_bindgen(start)]
fn run() {
    bare_bones();
    using_a_macro();
}

// 这里绑定了 `console.log`

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);

	// 由于 `console.log` 是多态的，可以绑定多个函数签名
    // 使用 `js_name` 指定我们绑定的是 `log`
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_u32(a: u32);

    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn log_many(a: &str, b: &str);
}

fn bare_bones() {
    log("Hello from Rust!");
    log_u32(42);
    log_many("Logging", "many values!");
}

// 这里文档作者进行了一点炫技
// 定义了一个类似于 `println!` 的宏

macro_rules! console_log {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

fn using_a_macro() {
    console_log!("Hello {}!", "world");
    console_log!("Let's print some numbers...");
    console_log!("1 + 3 = {}", 1 + 3);
}
```

实际上 `web-sys` crate 定义好了 `web_sys::console::log_1` 这样的函数，如果读者认为这是好的选择可以尝试。

另参考[导入非浏览器 JS](https://wasm.rust-lang.net.cn/docs/wasm-bindgen/examples/import-js.html) 一节。

---

注意在发布时使用发布模式 `cargo release` 编译. 如果希望获得最小的二进制文件，可启用 LTO.
```toml
[profile.release]
lto = true
```
