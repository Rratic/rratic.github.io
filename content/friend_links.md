+++
title = "友链"

[extra]
toc = true
+++

<style>
	.friend-links {
		display: grid;
		grid-template-columns: 1fr;
		gap: 1rem;
		margin: 1rem 0;
	}

	@media (min-width: 768px) {
		.friend-links {
			grid-template-columns: repeat(2, 1fr);
		}
	}

	.friend-link img,
	.friend-link .friend-link-placeholder {
		width: 3.5rem;
		height: 3.5rem;
		max-width: 3.5rem;
		max-height: 3.5rem;
		flex-shrink: 0;
	}

	.friend-link img {
		object-fit: cover;
	}
</style>

如有添加需求，可在此页下评论或通过主页给出的邮箱联系。

<div class="friend-links">

{{ friend_link(name="乌鸡卷", url="https://wjjpku.github.io/", desc="睡觉提高工作效率，午睡提高睡觉效率。", avatar = "https://wjjpku.github.io/img/headelephant.jpg") }}

{{ friend_link(name="383494", url="https://blog.383494.xyz/", avatar = "https://blog.383494.xyz/favicon.png") }}

{{ friend_link(name="Plenilune Liao", url="https://plen09.github.io/", avatar = "https://plen09.github.io/image/avatar.png") }}

</div>
