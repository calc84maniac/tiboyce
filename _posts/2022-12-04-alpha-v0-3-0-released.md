---
layout: post
title: "Alpha v0.3.0 Released"
categories:
  - Releases
tags:
  - Release
  - Video
---

After a long year of work, TI-Boy CE finally supports Game Boy Color!
This involved an extensive rewrite of the memory system to support both GB and GBC memory maps, also allowing improved compatibility and performance.
The color correction feature was also greatly improved from what was used in colorization for previous releases. The full change log is below.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

I've also recorded a video showing gameplay of Pokémon Crystal and Mario Golf:

<div class="video-container">
  <iframe class="video" src="https://www.youtube-nocookie.com/embed/vJ7nHP612eQ" frameborder="0" allowfullscreen></iframe>
</div>

The next main work for the project will be updating the GBC renderer to handle mid-frame palette changes, without which some games have incorrect colors.

## Changelog

### Added
-   Game Boy Color support.
-   GBA backwards-compatibility to unlock features in some GBC games.
-   Color correction to approximate the color spectrum of a GBC or GBA screen, leveraging native gamma settings.
-   Game Boy Color skin for no-scaling mode.
-   Brightness change hotkeys usable in menus.

### Changed
-   Confirmation setting for Load State now also applies to Restart Game.
-   Various improvements to the menu, courtesy of [runer112](https://github.com/runer112):
    -   Mark per-game options instead of global options with an asterisk.
    -   Improved menu item alignment and font.
-   Majorly rewrite the CPU emulation to improve performance and flexibility of memory access emulation.
    -   Rearrange the JIT's static register allocation and the ABI of routines called from it.
    -   Remove dynamically-generated per-opcode memory routines, reducing overhead of memory region selection.
    -   For special memory accesses (I/O reads or non-RAM writes), dynamically generate minimal trampolines instead.
    -   For writes to MBC registers, directly use a routine for the specific register to remove range-checking overhead.
    -   Always pass valid timing info to memory accesses which may require it, removing the need for validity checks.
    -   Enable complex memory instructions to directly read/write RAM, such as bitwise operations and INC/DEC (HL).
    -   Allow absolute reads/writes to have variable-length implementations, for better performance with banked memory.
    -   Change stack bounds-checking to speed up close SP modifications (INC/DEC/ADD SP) and free up a register.
    -   Use inclusive bounds for the stack to avoid thrashing when touching the high edge of a memory region.
    -   Improve performance of return prediction by ignoring the stack offset unless the prediction misses.
    -   Improve performance of self-modifying code checks by using 24-bit comparisons.
    -   Improve performance of self-modifying JP instructions by using dynamic dispatch instead of recompiling.
    -   Expand the list of supported instructions in waitloop detection to reduce false negatives.
    -   Optimize ROM bank switching routine at ROM load time based on the ROM size, to eliminate redundant masking.
    -   Simplify MBC3 RTC bank switching by handling RTC updates immediately before register writes.
-   Refactor the PPU scheduler to speed up writes to STAT and LYC registers.
    -   Predict future writes to the LYC register based on previous frames, to avoid excessive rescheduling.
    -   Remove post-vblank event cache to simplify all reschedules, and determine the event only when vblank is reached.

### Fixed
-   Reduce SPI transfer clock to fix glitchy display on some calculator revisions.
-   Increase Flash wait states for increased stability on some calculator revisions.
-   Fix game screen corruption in the main menu when showing a confirmation dialog.
-   Fix garbled character display when internal ROM title contains invalid characters.
-   Fix corruption of a couple of pixels when restoring the home screen.
-   Implement more accurate open-bus read behavior for disabled cartridge RAM.
-   Implement correct mirroring behavior for MBC2 cartridge RAM.
-   Fix reads past the end of ROM banks trimmed by the ROM converter.
-   Fix conditions for the window trigger to include writes to LCDC or WY during hblank.
-   Fix display of mid-frame sprite changes resulting from direct OAM writes.
-   Allow NR30 writes to disable audio channel 3.
-   Fix a scheduler edge case which could cause crashes in some games. Fixes Space Invaders (Japan).
