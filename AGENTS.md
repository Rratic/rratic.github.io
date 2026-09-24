## 生成 Markdown 任务
规范见于 [CODE OF CONDUCT](CODE_OF_CONDUCT.md)，特别注意其中关于段落的规则。

另注意：当前构建方式存在数学公式的解析问题，会先进行一次转义再运行 KaTeX，因此**存在反斜杠解析问题**，对于 `\\`，`\{`，`\,`，`\|` 等需要使用两倍的反斜杠。同时，紧凑的下划线、星号可能被错误解析为斜体。

## 生成交互项目任务
目录结构如下：

```
static/playground/
  <project-name>/
    index.html          # 页面入口
    ...                 # 该项目 JS / Lua / WASM 等资源
```
