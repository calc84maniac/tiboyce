---
layout: post
title: "Alpha v0.1.2 Released"
categories:
  - Releases
tags:
  - Release
---

Here's another release of TI-Boy CE, which mainly has UI improvements and bug fixes. In addition to a restructured ROM list and save state hotkeys, there are various compatibility and performance improvements.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Added
-   Save file converter between binary and AppVar formats.
-   A launcher icon and description for shells such as Cesium.
-   Overlay messages for state saving and loading (can be disabled).
-   Hotkeys for quickly selecting the current state slot and saving or loading states.

### Changed
-   Updated ROM list, now integrated into the emulator menu system and sorted alphabetically by title.
-   Refactored LCD mode switching and error handling, for smoother transitions between screens.
-   Cleaned up the skin image, thanks [ndye](https://github.com/ndye)!

### Fixed
-   Fix some cycle counting glitches related to serial port hardware.
-   Only trigger a STAT interrupt on LYC write if the value changes. Fixed a graphical glitch in Kid Icarus: Of Myths and Monsters.
-   For RETI, count cycles before attempting to schedule an interrupt. Fixed a graphical glitch in Kid Icarus: Of Myths and Monsters.
-   Load the ROM again from the beginning after archiving a file. Fixed a game title display glitch if a Garbage Collect occurred.
-   Use an asm_data_ptr to track the ROM title. Fixed a game title display glitch if the metadata file is unarchived.
-   Refactor JIT flushing logic; force a flush after two cache flushes. Fixed a continuous performance drop in certain scenarios.
-   Schedule the initial event one cycle after the loaded state. Prevents state save/load from triggering the same event twice.
