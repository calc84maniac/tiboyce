---
layout: post
title: "Alpha v0.1.1 Released"
categories:
  - Releases
tags:
  - Release
---

Here's a new release of TI-Boy CE, which mainly has bug fixes. It's recommended to update ASAP because it fixes a crash that could potentially affect any game.

You can download the release from [GitHub](https://github.com/calc84maniac/tiboyce/releases).

## Changelog

### Added
- Support for loading ROMs up to 4MB, increased from 2MB.
- Key names for TI-83 Premium CE in the control options.

### Changed
- Display the paletted white-equivalent color when the Game Boy screen is off, rather than pure white.
- Rearrange the emulator menu and reset the main menu option after (re)starting a game.
- Output ROM converter errors to the standard error stream.

### Fixed
- Fix sprites not displaying when an OAM DMA transfer is initiated mid-frame.
- Check for the RET instruction returning to a different ROM bank than its corresponding CALL.
  Fixes some crashes in specific games (e.g. Street Fighter II).
- Prevent a JIT overflow from overwriting the code path to the flush handler when decoding a banked CALL.
  Fixes some erratic crashes.
