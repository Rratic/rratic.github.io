# 错行
我的个人站点源码，使用 [Zola](https://www.getzola.org/) 及 [Linkita](https://www.getzola.org/themes/linkita/) 主题构建。内容以数学与相关随笔为主；部分文章配有可直接在浏览器中运行的交互演示。

本地预览：

```bash
zola serve
```

## 约定
### 交互
目录结构如下：

```
static/playground/
  <project-name>/
    index.html          # 页面入口
    ...                 # 该项目 JS / Lua / WASM 等资源
```

## 使用的资源
### Sublime Syntax
- Agda: [spyder-ide/pysyntect](https://github.com/spyder-ide/pysyntect/blob/master/syntect/grammars/Agda.sublime-syntax)
- Lean4: [lean-ja/lean-sublime-syntax](https://github.com/lean-ja/lean-sublime-syntax)
