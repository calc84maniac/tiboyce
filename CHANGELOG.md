# TI-Boy CE Changelog
All notable changes to this project will be documented in this file.

## [HEAD](https://github.com/calc84maniac/tiboyce/compare/v0.1.3...HEAD)

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
