+++
title = "Haskell 学习（一）"
date = 2025-08-26

[extra]
math = true
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "Haskell"]
+++

前置知识
- 一般的编程基础
- [无类型 λ 演算](/posts/lambda-calculus/)

参考的是 <https://www.bilibili.com/video/BV1pwdgYmE9L>

## 环境配置
参考 <https://www.haskell.org/downloads/> 的指引

在 Windows 下，可使用以下 Powershell 命令
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

安装完毕后，运行命令 `ghci` 就可打开交互式窗口。

## 函数与列表
函数只接受一个输入。GHCi 中可以用 `:t` 命令查看类型。
```hs
ghci> :t isUpper
isUpper :: Char -> Bool
ghci> isUpper 'A'
True
ghci> isUpper ' '
False
ghci> isUpper "abc"

<interactive>:5:9: error: [GHC-83865]
    ? Couldn't match type ‘[Char]’ with ‘Char’
      Expected: Char
        Actual: String
    ? In the first argument of ‘isUpper’, namely ‘"abc"’
      In the expression: isUpper "abc"
      In an equation for ‘it’: it = isUpper "abc"
```

括号表示元组，例如
```hs
ghci> addInt (a, b) = a + b
ghci> addInt (101, 103)
204
```

但这不是正确的 Haskell 使用方式。我们应当这样：
```hs
ghci> addInt a b = a + b
ghci> :t addInt
addInt :: Num a => a -> a -> a
ghci> incr = addInt 1
ghci> :t incr
incr :: Num a => a -> a
ghci> incr 10
11
ghci> (incr . incr) 10
12
```

列表
```hs
ghci> 0 : [1,2,3] ++ [4,5]
[0,1,2,3,4,5]
ghci> [1..10]
[1,2,3,4,5,6,7,8,9,10]
ghci> ['A'..'Z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

`[1..]` 将生成一个无限列表。这得益于 Haskell 是懒惰求值的。
```hs
ghci> take 10 [1..]
[1,2,3,4,5,6,7,8,9,10]
```

这里可以使用列表构造的语法糖，在 `|` 左侧为值，右边为符合的条件。这类似于集合的一种表达方式。
```hs
ghci> map (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
ghci> map' f xs = [ f n | n <- xs ]
ghci> map' (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
ghci> filter' p xs = [ n | n <- xs, p n ]
ghci> filter' even [1..10]
[2,4,6,8,10]
```

让我们做一些大一点的东西。

现在可以在位于 `./Main.hs` 的文件中编辑代码
```hs
module Main where

import Data.Char
import Data.List

unused :: String -> [(Char, Int)]
unused = undefined -- canonical

canonical :: String -> String
canonical = filter (/= ' ') . map normalise

normalise c | isUpper c = c
            | isLower c = toUpper c
            | otherwise = ' '

main :: IO ()
main = putStrLn "Hello, Haskell!"
```

并使用 `:load Main` 运行，这会得到
```hs
ghci> :load Main
[1 of 2] Compiling Main             ( Main.hs, interpreted )
Ok, one module loaded.
ghci> canonical "Hello, world!"
"HELLOWORLD"
ghci> f = group . sort . canonical
ghci> f "Hello, world!"
["D","E","H","LLL","OO","R","W"]
```

用反斜杠 `\` 声明 Lambda 表达式。
```hs
ghci> display = map (\x -> (head x, length x))
ghci> unused = display . group . sort . canonical
ghci> unused "Hello, world!"
[('D',1),('E',1),('H',1),('L',3),('O',2),('R',1),('W',1)]
```

## 类型
我们可以定义类型别名
```hs
ghci> type Id = Int
```

也可以从头开始定义类型
```hs
ghci> data Lis = Empty | Cons Int (Lis)
ghci> :t Empty
Empty :: Lis
ghci> :t Cons 1 (Cons 2 Empty)
Cons 1 (Cons 2 Empty) :: Lis
```

可以增加泛型
```hs
ghci> data Lis ty = Empty | Cons ty (Lis ty)
ghci> :t Cons 1 (Cons 2 Empty)
Cons 1 (Cons 2 Empty) :: Num ty => Lis ty
ghci> :t Cons 'A' (Cons 'B' Empty)
Cons 'A' (Cons 'B' Empty) :: Lis Char
```

在文件中写如下内容
```hs
data Lis ty = Empty | Cons ty (Lis ty)

toHList :: Lis a -> [a]
toHList Empty       = []
toHList (Cons x xs) = x : toHList xs
```

即有 `toHList (Cons 'a' (Cons 'b' Empty))` 给出 `"ab"`

或可使用语法糖 `toHList $ Cons 'a' $ Cons 'b' Empty`

也可以实现我们在现代语言中熟悉的 `Option`
```hs
data Option a = None | Some a deriving Show

safeDiv :: Int -> Int -> Option Int
safeDiv a 0 = None
safeDiv a b = Some (a `div` b)
```

对于结构体，有更简单的定义方法
```hs
data Person = Person {
    name :: String,
	id   :: Int
	dob  :: (Int, Int, Int) -- day of birth
}
```

## 类型类
我们之前用到了 `show`，可以将数据变为显示的字符串
```hs
ghci> show 'A'
"'A'"
ghci> (putStrLn . show) [1, 2, 3]
[1,2,3]
```

我们可以通过 `class` 和 `instance` 自己实现。
