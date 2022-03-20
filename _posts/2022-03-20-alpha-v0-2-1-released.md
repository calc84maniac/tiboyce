---
layout: post
title: "Alpha v0.2.1 Released"
categories:
  - Releases
tags:
  - Release
---

This is a minor release which uses newly discovered LCD documentation to eliminate diagonal screen tearing in fullscreen display mode.

Additionally, it lays groundwork for future Game Boy Color rendering by allowing 8 bits per pixel without doubling the framebuffer RAM usage.

In this release, I also leverage the 8-bit framebuffers to greatly improve performance when sprite palettes are changed mid-frame.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Changed
-   Now using 8 bits per pixel in all display modes, eliminating code for special-casing 4-bit pixels.
-   Emulator overlays are horizontally stretched in fullscreen mode, as required by the tearing fix.

### Fixed
-   Fixed diagonal screen tearing in fullscreen display modes by using VSYNC interface and interlaced scan.
-   Greatly improved performance of frame rendering when sprite palettes are changed mid-frame.
