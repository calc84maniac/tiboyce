---
layout: post
title: "TI-Boy CE GBC Preview"
categories:
  - Videos
tags:
  - Preview
  - Video
---

Hey all! I've been slowly but surely working on Game Boy Color support, and I'm approaching completion of all the features needed to run games.

There are a few remaining issues that I'll save for after the next release, including:

-   Lack of support for mid-frame palette changes (used by some games to display more than 56 colors at a time)
-   Poor performance in some games (though I'll try to work on anything that's easy to fix)
-   Storage space limitations (most 4MB ROMs are not usable simply because they don't fit in storage)

My plan moving forward is to try out as many games as I can, and address any remaining stability issues. Hopefully, I can make a release in the near future.

In the meantime, [Adriweb](https://github.com/adriweb) has recorded a video showcasing the opening cutscene of Pokemon Crystal in the emulator! Watch here:

<div class="video-container">
  <iframe class="video" src="https://www.youtube-nocookie.com/embed/AXLLm5DT8sk" frameborder="0" allowfullscreen></iframe>
</div>

I've also recorded some direct gameplay footage of Metal Gear Solid: Ghost Babel using [CEmu](https://ce-programming.github.io/CEmu/):

<img alt="Metal Gear Solid gameplay" src="https://i.imgur.com/jRxJAi4.gif"/>
