+++
title = "猫猫棋"
date = 2023-12-09
updated = 2025-03-23

[taxonomies]
categories = ["归档"]
tags = ["展示", "在线", "游戏", "含模拟"]
+++

## 规则
### 棋盘
棋盘为方形，默认为 8×8，初始时两方各有一排白猫和一排黑猫。

棋盘的上下边界是循环的。

### 棋子
* 橘猫：最简单直接的棋子，可以向周围 4 个方向移动或吃对方的子
* 黑猫：类似国际象棋的兵
	* 处于**开始状态**可以选择向前移动两步
	* 可以选择向前移动一步
	* 可以选择向侧前方吃对方的子
	* 没有「吃过路兵」规则
* 白猫：类似跳棋
	* 在周围 8 个方向若有棋子，且跳过棋子到达空格则可以**跳**
	* 会改变跳过棋子的毛色（白 => 黑 => 橘 => 白）
	* 跳过棋子若变为黑则重置为**开始状态**

## 交互
AI 采用了 α-β 剪枝算法。棋盘评估函数不一定合理。

<script type="module" src="/script/chess/catchess_handler.js"></script>

<style>
	input {
		width: -webkit-fill-available;
    	margin: 20px;
    	padding: 10px;
    	padding-left: 20px;
    	font-weight: 400;
    	text-align: center;
    	border-radius: 20px;
    	line-height: 1.8;
    	border: 1px dashed #999;
	}

	#launch_button_container {
		text-align: center;
	}

	#canvas_box {
		display: flex;
		justify-content: center;
	}

	#canvas_box > canvas {
		border: 8px solid gold;
	}
</style>

<p id="canvas_box">&lt;棋盘生成处&gt;</p>
<input id="input_board_size" type="number" placeholder="棋盘大小 5~16" min="5" max="16" />
<input id="input_use_ai" type="number" placeholder="1=单人 2=双人" min="1" max="2" />
<p id="launch_button_container"><a id="launch_button" onclick="launch()">建立</a></p>
