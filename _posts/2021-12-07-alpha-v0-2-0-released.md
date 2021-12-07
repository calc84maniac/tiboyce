---
layout: post
title: "Alpha v0.2.0 Released"
categories:
  - Releases
tags:
  - Release
  - Video
---

As a culmination of the last two years of work, finally a new TI-Boy CE release is available!
The primary focus has been performance and accuracy improvements, fixing all reported game issues and improving playability of many games such as Pokémon Gen 2.
There are also some user experience improvements, such as per-game configuration support and improved colorization. The full change log is below.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

I've also recorded a comparison video showing the improvements to the opening cutscene of Pokémon Gold:

<div class="video-container">
  <iframe class="video" src="https://www.youtube-nocookie.com/embed/lMxI3kxr9Ss" frameborder="0" allowfullscreen></iframe>
</div>

The next goal for this project is to add Game Boy Color support, so please look forward to future updates!

## Changelog

### Added
-   Per-game configuration settings.
-   Backlight adjustment hotkeys.
-   Classic Game Boy palette option.
-   Color adjustment option to more closely match a GBC screen when colorizing.
-   Configurable confirmation dialog for loading and/or overwriting save states.
-   Ability to delete save states and ROM files through the emulator menu.
-   Ability to unmap most emulator hotkeys.

### Changed
-   Darkened menu background color to improve text contrast.
-   Load State option is no longer shown for state slots that do not exist.

### Fixed
-   Majorly improve performance in many games. Notably, Pokémon Gold and Silver now run at playable speeds.
    -   Refactor cycle counting and event scheduling to speed up branches and avoid speculative event schedules.
    -   Bind cycle offsets to memory access instructions dynamically, rather than using a fixed-size cache.
    -   Make address mapping cache use bucket-based linear lookup instead of binary search.
    -   Speed up callstack caching by tracking aggregated memory bank deltas using the bank switches themselves.
    -   Create a dynamic PPU state machine that caches LY and STAT register values with expiration times.
    -   Use tables of overlapped pixel data to speed up Game Boy VRAM writes and native palette generation.
    -   Speed up VRAM caching up to 2x by deferring pixel generation until the second byte is written.
    -   Queue BGP writes to handle at end of frame, allowing the maximum number of scanlines to use native palettes.
-   Potentially fix white screen issue on Python models by reinitializing hardware on emulator start.
-   Improve double-buffering logic, fixing occasional display of incomplete frames when frameskip is disabled.
-   Implement bounds checking and full memory side effects on stack accesses. Fixes emulator glitches and crashes in many games.
-   Implement MBC RAM protection. May help prevent cartridge save corruption in the case of game bugs or crashes.
-   Prevent the emulator from freezing if a game turns the LCD on and off repeatedly.
-   Add simple support for APU enable/disable and channel length counters.
-   Implement delayed update of IME flag when an EI instruction is executed.
-   Implement emulation of the CPU's HALT bug. Fixes a game crash in The Smurfs.
-   Support changing interrupt target during dispatch. Fixes a game crash in Pinball Deluxe.
-   Implement STAT interrupt blocking behavior. Fixes a game crash in Joust.
-   Support executing instructions overlapping memory regions. Fixes emulator crashes in FIFA 2000 and Hyper Lode Runner.
-   Skip rendering the first frame after the LCD turns on to avoid glitch frames. Fixes a graphical glitch in Tetris Blast.
-   Use correct window triggering behavior (window is active after LY==WY). Fixes a graphical glitch in Tetris Blast.
-   Catch up rendering on VRAM writes. Fixes a graphical glitch in V-Rally Championship Edition.
-   Support mid-frame sprite palette changes. Fixes a graphical glitch in V-Rally Championship Edition.
-   Fix mid-frame sprite size changes. Fixes a graphical glitch in the dmg-acid2 test ROM.
-   Improve accuracy when fast-forwarding LY-based waitloops, and add support for STAT-based waitloops.
-   Emulate differing flag outputs of RLA/RRA/RLCA/RRCA/CCF. Fully passes Blargg's cpu_instrs test ROM.
-   Implement many timing and accuracy improvements for MBC, PPU, timer, serial, DMA, and RTC to pass test ROMs.
