# TI-Boy CE

[![Build Status](https://travis-ci.org/calc84maniac/tiboyce.svg)](https://travis-ci.org/calc84maniac/tiboyce)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Release](https://img.shields.io/github/v/release/calc84maniac/tiboyce)](https://github.com/calc84maniac/tiboyce/releases/latest)

TI-Boy CE is a Game Boy emulator for the TI-84 Plus CE and the TI-83 Premium CE graphing calculators.

Currently only the original Game Boy is supported, no Game Boy Color (and never Game Boy Advance).

This emulator is currently in the alpha state, so while it is intended to be stable, it is possible that it could crash and cause data loss. It is advised to put any important files in Archive memory before running the emulator.

Grab the latest pre-built releases at <https://github.com/calc84maniac/tiboyce/releases> or check out the [Build Instructions](#build-instructions) to build from source.

## Features

*   Emulates original Game Boy hardware (except audio and linking)
*   Emulates real-time clock for certain cartridges
*   Save states with compression
*   Fullscreen and 1:1 scaling modes (with optional skin)
*   Automatic and manual frameskip
*   Turbo mode (with speed display)
*   GBC-style selectable color palettes for Game Boy games
*   Customizable controls

## Converting ROM Files

The easiest method is to use the online converter [here](https://calc84maniac.github.io/tiboyce/converter).
All conversion is done locally in your browser, so there's no need to worry about uploading copyrighted data.

Alternatively, a command-line utility (tiboyce-romgen.exe) to convert ROM files to TI AppVars is included. There are a couple of ways to do this:

1.  Open a command prompt and navigate to the directory containing the utility using `cd`. Then run the following:

        tiboyce-romgen.exe -t "Game Title Here" "path\to\romfile" NamePrefix

2.  Drag-and-drop a ROM file onto the utility, which will then prompt for a game title and a name prefix.

Note that the name prefix provided for the ROM must be at most 5 characters long. This is because the remaining 3 characters are used for naming additional files created by the utility and the emulator itself. The game title provided will be displayed in the emulator's ROM list.

The utility will generate multiple AppVar (*.8xv) files with the given prefix. Send all of them to the calculator in Archive memory.

If you get a missing DLL error when running the command-line utility, you may need to install the [Universal C Runtime](https://support.microsoft.com/en-us/help/2999226/update-for-universal-c-runtime-in-windows).

To convert save files between PC and AppVar formats, see the [Converting Save Files](#converting-save-files) section below.

## Running the emulator

Send the `TIBOYCE.8xp` and `TIBoyDat.8xv` files to the calculator. Optionally also send the `TIBoySkn.8xv` file which contains a skin image.

If your calculator is running OS v5.3 or newer, you can keep all of these files in Archive and run `prgmTIBOYCE` from the <kbd>prgm</kbd> menu.

If your calculator is older than OS v5.3, the `TIBOYCE` program must be unarchived. Run it with `Asm(prgmTIBOYCE)`.

You should now see a list of the ROMs on the calculator. Choose one with <kbd>↑</kbd> <kbd>↓</kbd> and start it with <kbd>2nd</kbd> <kbd>enter</kbd>.

## Default controls

### Game controls:
*   D-Pad: <kbd>↑</kbd> <kbd>↓</kbd> <kbd>←</kbd> <kbd>→</kbd>
*   A: <kbd>2nd</kbd>
*   B: <kbd>alpha</kbd>
*   Start: <kbd>mode</kbd>
*   Select: <kbd>XTθn</kbd>

### Emulator controls:
*   Open menu: <kbd>clear</kbd>
*   Turbo mode: <kbd>zoom</kbd>
*   Save state: <kbd>sto></kbd>
*   Load state: <kbd>ln</kbd>
*   State slot: <kbd>log</kbd> + <kbd>0</kbd>-<kbd>9</kbd>
*   Quick exit (non-configurable): <kbd>on</kbd>

### Menu controls (non-configurable):
*   Choose menu item: <kbd>↑</kbd> <kbd>↓</kbd>
*   Change option: <kbd>←</kbd> <kbd>→</kbd>
*   Select item: <kbd>2nd</kbd> <kbd>enter</kbd>
*   Close menu: <kbd>clear</kbd>

## File Types

The various files used by the emulator are as follows (replace Name with ROM prefix and # with digits):

| File         | Description                                                  |
|:-------------|:-------------------------------------------------------------|
| Name.8xv     | The file specifying the game title and how large the ROM is. |
| NameR##.8xv  | Multiple files containing the actual ROM data.               |
| NameSAV.8xv  | The contents of the battery-backed cartridge save data.      |
| NameStA.8xv  | The automatic save state for this game.                      |
| NameSt#.8xv  | The manual save state for the given numbered slot.           |
| NameSv#.8xv  | The cartridge save associated with a numbered save state.    |
| TIBoyCE.8xp  | The executable launcher.                                     |
| TIBoyCfg.8xv | The current emulator configuration.                          |
| TIBoyDat.8xv | The core emulator data, loaded by the launcher.              |
| TIBoySkn.8xv | An optional skin to be displayed in “no scaling” mode.       |

Note that save states cannot be loaded properly if the associated cartridge save data file is deleted or replaced. When transferring save states, make sure to include both the `St#` and `Sv#` files. However, this doesn't apply to games that have no cartridge save data in the first place.

## Converting Save Files

You can convert between PC emulator and TI-Boy CE save file formats using the provided utilities.

Note that only cartridge save files (`*.sav` or `*.srm` on PC or `*SAV.8xv` on calculator) may be converted. Save states are emulator-specific and cannot be converted.

The easiest method is to use the online converter [here](https://calc84maniac.github.io/tiboyce/saveconverter).
All conversion is done locally in your browser, so there's no need to worry about uploading personal data.

Alternatively, a command-line utility (tiboyce-convertsav.exe) to convert between save file formats is included. There are a couple of ways to do this:

1.  Open a command prompt and navigate to the directory containing the utility using `cd`. Then run the following:

        tiboyce-convertsav.exe "path\to\inputfile" "path\to\outputfile"

2.  Drag-and-drop a save file onto the utility, which will then prompt for an output file name.

Note that when creating a `*SAV.8xv` AppVar, the name prefix provided must be the same as the converted ROM, or TI-Boy CE will not load it. This utility may also be used to change the prefix of a save AppVar if a ROM was converted with a different name.

If you get a missing DLL error when running the command-line utility, you may need to install the [Universal C Runtime](https://support.microsoft.com/en-us/help/2999226/update-for-universal-c-runtime-in-windows).

## Build Instructions

To build the emulator from source, first grab the latest release of [SPASM-ng](https://github.com/alberthdev/spasm-ng/releases).

SPASM-ng v0.5-beta.3 is new enough to build properly; however, if you use that release you should ignore any warnings about file sizes exceeding 24KB.

For simplicity's sake, I'll call the name of the executable `spasm` below. Run the following to produce the emulator files:
```
spasm -E -A launcher.asm TIBOYCE.8xp

spasm -E -A tiboyce.asm TIBoyDat.8xv

spasm -E -A skin.asm TIBoySkn.8xv
```
To build the rom generation tool, use the provided Visual Studio solution in the [tiboyce-romgen](tiboyce-romgen) directory, or you can build the source for any platform with your C compiler of choice.

The same applies to the save converter in the [tiboyce-convertsav](tiboyce-convertsav) directory.

## Issues / Bugs

Report issues / bugs to the issue tracker, found here:

<https://github.com/calc84maniac/tiboyce/issues>

## License

TI-Boy CE — a Game Boy emulator for the TI-84 Plus CE calculator family.
Copyright (C) 2018–2019 Brendan Fletcher

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
