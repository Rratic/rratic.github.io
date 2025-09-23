+++
title = "密码学（一）"
description = "使用 Haskell 完成的 The Cryptopals Crypto Challenges - Set 1"
date = 2025-09-22

[extra]
toc = true

[extra.sitemap]
priority = "0.8"

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "密码学", "Haskell"]
+++

注：曾经我使用别的语言（Julia）写过类似的内容，但没有良好的解耦。因此改用 Haskell 进行更清晰的实现，读者也可使用自己熟悉的语言实现。

关于密码学一个很好的书是 *Real World Cryptography*，兼具细节和高观点。但本文不会涉及太复杂的内容。

本文实现 [The Cryptopals Crypto Challenges](https://cryptopals.com/) 的基础练习集 Set 1

## 格式转换
> Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing.

```hs
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe

type Raw = [Bool]

writeSized :: Int -> Int -> [Bool]
writeSized sz int
  | sz == 0 = []
  | otherwise = (int `mod` 2 == 1) : writeSized (sz - 1) int
```

### 解耦
一些常用的功能。

以指定的方式填充：
```hs
pad :: Int -> [a] -> (Int -> a) -> [a]
pad len list gen
  | pos == 0 = list
  | otherwise = list ++ map gen [(pos + 1) .. len]
  where
    pos :: Int = length list `mod` len
```

分段：
```hs
splits :: Int -> [a] -> [[a]]
splits len = unfoldr split
  where
    split [] = Nothing
    split xs = Just (splitAt len xs)
```

### Hex
这里实际上存在大小端的约定问题。

```hs
readHex :: String -> Raw
readHex = concatMap read1
  where
    read1 c = writeSized 4 $ fromJust $ lookup c (zip table [0 .. 15])
      where
        table = "0123456789abcdef"

(readHex "1f") == [True,False,False,False,True,True,True,True]
```

### Base64
{{ todo() }}

有了这些东西我们已经可以实现 Challenge 1
