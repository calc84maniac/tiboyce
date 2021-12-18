# TI-Boy CE Changelog
All notable changes to this project will be documented in this file.

## [HEAD](https://github.com/calc84maniac/tiboyce/compare/v0.2.0b...HEAD)

## [Alpha v0.2.0b — 2021-12-17](https://github.com/calc84maniac/tiboyce/releases/tag/v0.2.0b)

### Fixed
-   Fix a game freeze occuring in certain conditions after a delayed EI is followed by a HALT. Fixes Donkey Kong Land.
-   Fix a likely runtime error when a memory access instruction reads an I/O register and later reads other memory.

## [Alpha v0.2.0a — 2021-12-09](https://github.com/calc84maniac/tiboyce/releases/tag/v0.2.0a)

### Fixed
-   Fix an intermittent code corruption when loading RTC save states.

## [Alpha v0.2.0 — 2021-12-07](https://github.com/calc84maniac/tiboyce/releases/tag/v0.2.0)

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

## [Alpha v0.1.3 — 2019-08-24](https://github.com/calc84maniac/tiboyce/releases/tag/v0.1.3)

### Fixed
-   Updated hardware accesses to support new (Revision M) calculators, including TI-83 Premium CE Python Edition.

## [Alpha v0.1.2a — 2019-04-03](https://github.com/calc84maniac/tiboyce/releases/tag/v0.1.2a)

### Fixed
-   Updated ROM converter to respect a new AppVar size limitation on OS 5.3.5 (at this time, only on TI-83 Premium CE).
-   Dynamically linked the converter EXEs against the Universal C Runtime, avoiding false positives on many antivirus engines.

## [Alpha v0.1.2 — 2019-02-16](https://github.com/calc84maniac/tiboyce/releases/tag/v0.1.2)

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

## [Alpha v0.1.1 — 2018-07-28](https://github.com/calc84maniac/tiboyce/releases/tag/v0.1.1)

### Added
-   Support for loading ROMs up to 4MB, increased from 2MB.
-   Key names for TI-83 Premium CE in the control options.

### Changed
-   Display the paletted white-equivalent color when the Game Boy screen is off, rather than pure white.
-   Rearrange the emulator menu and reset the main menu option after (re)starting a game.
-   Output ROM converter errors to the standard error stream.

### Fixed
-   Fix sprites not displaying when an OAM DMA transfer is initiated mid-frame.
-   Check for the RET instruction returning to a different ROM bank than its corresponding CALL. Fixes some crashes in specific games (e.g. Street Fighter II).
-   Prevent a JIT overflow from overwriting the code path to the flush handler when decoding a banked CALL. Fixes some erratic crashes.

## [Alpha v0.1.0 — 2018-06-19](https://github.com/calc84maniac/tiboyce/releases/tag/v0.1.0)

### Added
-   Initial emulator release.
