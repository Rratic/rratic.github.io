+++
title = "记 MarkItDown 工具试用"
date = 2026-06-11

[taxonomies]
categories = ["杂物"]
tags = ["计算机"]
+++

很早听说微软的 [MarkItDown](https://github.com/microsoft/markitdown) 工具，简单尝试。由于本地空间有限，我在学校提供的云计算实验平台 [PKU CLab](https://clab.pku.edu.cn/) 上配置了一个 Ubuntu 环境，在我的 Windows 中远程连接。

<!-- more -->

---

连接上服务器后，先安装 Python 环境：
```sh
sudo apt update
sudo apt install python3 python3-venv
```

然后创建虚拟环境，在其中安装：
```sh
ubuntu@my-clab:~$ python3 -m venv .venv
ubuntu@my-clab:~$ source .venv/bin/activate
(.venv) ubuntu@my-clab:~$ pip install 'markitdown[all]'
```

新开一个窗口，把文件复制到云服务器桌面上：
```sh
scp my_path/test.pdf ubuntu@my-clab:~
```

在前面虚拟环境中调用工具：
```sh
(.venv) ubuntu@my-clab:~$ markitdown ~/test.pdf > ~/test.md
```

然后把导出的文件复制过来：
```sh
scp ubuntu@my-clab:~/test.md my_path/test.md
```

---

用起来的感受是相当不满意的。首先，它和其它工具一样并不具有读图的功能，这意味着我并不能通过它将手写笔记转为 Markdown. 其次它对数学公式的解析也很糟糕，会犯典型的一个 `\R` 导出 `R R R` 的问题，在我的试用中甚至把公式解析成了表格。另外，在我的试用中没有解析出任何标题。

据说对表格可以比较好地完成转化，但是如果只是表格，用传统脚本就行，并不需要这样复杂的工具。

目前它的所有功能都可以被我手上 [Cursor](https://cursor.com/) 的各个 Agent 替代。
