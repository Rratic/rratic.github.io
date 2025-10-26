+++
title = "迷宫生成算法：固定大小迷宫与无限迷宫"
description = "固定大小迷宫的随机化 Prim 算法及其 C++ 示例；无限迷宫的递归分割算法及其 Javascript 示例。"
date = 2022-08-29
updated = 2024-01-21

[taxonomies]
categories = ["知识"]
tags = ["笔记", "计算机", "算法"]
+++

注：本文迁移自之前的文章集

## 固定大小
考虑一种随机化 Prim 算法，思路如下：

```c
地图 = 全部为墙
标记 = 全部为空
起点 = 随机选一格
列表 = [起点]
while (列表 非空) {
	一格 = 从列表中 随机挑
    从列表中 删除 该格
	if (该格 被标记) continue;
    标记 该格
    if (该格 周围墙数 ≤ 1) { 将周围4面墙加入列表 }
	终点 = 该格
}
```

代码：
```cpp
#include <array>
#include <iostream>
#include <vector>

using namespace std;
#define isin(x,y) (x>=0 && y>=0 && x<m && y<n)

template<unsigned short l=15,unsigned short w=15>
void prim (unsigned short maze_len, unsigned short maze_wid, array<array<unsigned short,l>,w>&a){
    int m = maze_len;
    int n = maze_wid;
    short mox[4] = {0,0,1,-1};
    short moy[4] = {1,-1,0,0};
    vector<pair<unsigned short,unsigned short>>li;
    bool was[21][21];
    for (int i=0; i<m; ++i){
        for (int j=0; j<n; ++j){
            a[i][j] = '#';
            was[i][j] = false;
        }
    }
    unsigned short stx, sty, enx, eny;
    stx = rand() % m;
    sty = rand() % n;
    a[stx][sty] = '.';
    li.push_back(make_pair(stx, sty));
    while (!li.empty()) {
        auto rr = rand() % li.size();
        auto r = li[rr];
        li.erase(li.begin() + rr);
        if (was[r.first][r.second]) continue;
        was[r.first][r.second] = true;
        unsigned short c = 0;
        for (int i=0; i<4; ++i) {
            if(isin(r.first+mox[i], r.second+moy[i]) && a[r.first+mox[i]][r.second+moy[i]]=='.')
				++c;
        }
        if (c <= 1) {
            a[r.first][r.second] = '.';
            for (int i=0; i<4; ++i)
                if (!was[r.first+mox[i]][r.second+moy[i]] && isin(r.first+mox[i], r.second+moy[i]))
                    li.push_back(make_pair(r.first+mox[i], r.second+moy[i]));
        }
        enx = r.first;
        eny = r.second;
    }
    a[stx][sty] = '@';
    a[enx][eny] = '%';
}

int main() {
	srand(time(nullptr));
    array<array<unsigned short,15>,15>a;
    prim<15, 15>(15, 15, a);
    for (int i=0; i<15; ++i) {
        for (int j=0; j<15; ++j) {
            cout << char(a[i][j]);
        }
        cout << '\n';
    }
}
```

产生的迷宫以弯道为主，例如：
```txt
..#.#..#.##.#%.
.##..#.#.....#.
...#.#...###...
#.#...#.##..###
#..#.#..#..#...
..##...#.#..#.#
#...#.##...#...
..##.....##.#.#
#..##.##..#.#..
..#.#.#.##..#.#
#.........#.#..
..###.#.#.....#
#....##..#.##..
.@#.#...#....#.
.#..#.##..#.#..
```

关于更自由地使用此算法，可参考 <https://zhuanlan.zhihu.com/p/27381213>

## 无限迷宫
我们需要做的是将迷宫分割成若干个区域（如 64×64 大小的区域），然后将它们衔接

在每个区域，可使用递归分割算法
```c
void func (args...) {
	建造横墙
	建造竖墙
	在墙上挖三个洞
	for (区域 : 四个区域) {
		func (args...);
	}
}
```

代码：
```js
// C-flavour random
let gsrand = 0
function srand(x) { gsrand = x; }
function rand() {
    gsrand = (gsrand * 1103515245+12345) & 0xffffffff;
    return gsrand >> 16 & 32767;
}

class Chunk {
    matrix
    constructor() {
        // 64×64 chunk
        this.matrix = new Uint8Array(4096);
    }
}

Chunk.prototype.put = function(x, y, type) {
    this.matrix[x << 6 | y] = type == 'space' ? 0 : 1;
}

/* * * Core * * */
Chunk.prototype.generate__infmaze_4 = function (lx, ly, rx, ry) {
	let x0 = rx - lx;
	let y0 = ry - ly;
	// room small enough (width = 1)
	if (x0 == 0 || y0 == 0) {
		for (let i = lx; i <= rx; i++) {
			for (let j = ly; j <= ry; j++) this.put(i, j, 'space');
		}
		return;
	}
	let mx = lx + 2 * (rand() % (x0 >> 1)) + 1;
	let my = ly + 2 * (rand() % (y0 >> 1)) + 1;
	for (let i = lx; i <= rx; i++) this.put(i, my, 'wall');
	for (let i = ly; i <= ry; i++) this.put(mx, i, 'wall');
	// split the map into four smaller rooms
	this.generate__infmaze_4(lx, ly, mx - 1, my - 1);
	this.generate__infmaze_4(lx, my + 1, mx - 1, ry);
	this.generate__infmaze_4(mx + 1, ly, rx, my - 1);
	this.generate__infmaze_4(mx + 1, my + 1, rx, ry);
	// three exits serve as passages through rooms
	let d = rand() % 4;
	let myl = (my - ly + 1) >> 1;
	let myr = (ry - my + 1) >> 1;
	let mxl = (mx - lx + 1) >> 1;
	let mxr = (rx - mx + 1) >> 1;
	if (d != 0) this.put(lx + 2 * (rand() % mxl), my, 'space');
	if (d != 1) this.put(rx - 2 * (rand() % mxr), my, 'space');
	if (d != 2) this.put(mx, ly + 2 * (rand() % myl), 'space');
	if (d != 3) this.put(mx, ry - 2 * (rand() % myr), 'space');
}

Chunk.prototype.generate__infmaze = function(x, y) {
    // isolate chunks
    for(let i = 0; i < 64; i++) {
        this.put(i, 0, 'wall');
        this.put(0, i, 'wall');
    }

    // use seed
    srand((x << 15 + y) ^ seed << 3);
    this.generate__infmaze_4(1, 1, 63, 63);

    // connect chunks
    let r1 = rand() % 32;
    this.put(2 * (rand() % 32) + 1, 0, 'space');
    this.put(0, 2 * (rand() % 32) + 1, 'space');
}
```
