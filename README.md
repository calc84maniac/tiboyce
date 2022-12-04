# TI-Boy CE

[![Build Status](https://travis-ci.org/calc84maniac/tiboyce.svg)](https://travis-ci.org/calc84maniac/tiboyce)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

TI-Boy CE is a Game Boy emulator for the TI-84 Plus CE and the TI-83 Premium CE graphing calculators.

Now supports both the original Game Boy and Game Boy Color, but not Super Game Boy (and never Game Boy Advance).

This emulator is currently in the alpha state, so while it is intended to be stable, it is possible that it could crash and cause data loss. It is advised to put any important files in Archive memory before running the emulator.

Grab the latest pre-built releases at <https://github.com/calc84maniac/tiboyce/releases> or check out the [Build Instructions](#build-instructions) to build from source.

## Features

*   Emulates Game Boy and Game Boy Color hardware (except audio and linking)
*   Supports GBA backwards-compatibility to unlock features in some GBC games
*   Emulates real-time clock for certain cartridges
*   Save states with compression
*   Fullscreen and 1:1 scaling modes (with optional skins)
*   Automatic and manual frameskip
*   Turbo mode (with speed display)
*   GBC-style selectable color palettes for Game Boy games
*   Color correction to approximate the color spectrum of a GBC or GBA screen
*   Customizable controls
*   Per-game configuration settings

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

Send the `TIBOYCE.8xp` and `TIBoyDat.8xv` files to the calculator. Optionally also send the `TIBoySkn.8xv` file which contains skin images.

If your calculator is running OS v5.3 or newer, you can keep all of these files in Archive and run `prgmTIBOYCE` from the <kbd>prgm</kbd> menu.

If your calculator is running OS v5.5 or newer, you must also jailbreak your calculator with a tool such as [arTIfiCE](https://yvantt.github.io/arTIfiCE).

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
*   Brightness up: <kbd>+</kbd>
*   Brightness down: <kbd>-</kbd>
*   Quick exit (non-configurable): <kbd>on</kbd>

### Menu controls (non-configurable):
*   Choose menu item: <kbd>↑</kbd> <kbd>↓</kbd>
*   Change option: <kbd>←</kbd> <kbd>→</kbd>
*   Select item: <kbd>2nd</kbd> <kbd>enter</kbd>
*   Delete ROM or save state: <kbd>del</kbd>
*   Unmap key or remove per-game config item: <kbd>del</kbd>
*   Brightness up: <kbd>+</kbd>
*   Brightness down: <kbd>-</kbd>
*   Close menu: <kbd>clear</kbd>

## Configuration

All emulator configuration items can be edited on either a global or per-game basis. The configuration to edit can be selected on the main menu after loading a ROM.

When editing the per-game configuration, changing an option will override the global configuration and mark it with a `*`. Press <kbd>del</kbd> on an overridden configuration item to revert it to global.

### Graphics Options
*   Scaling mode: Choose the display mode.
    *   No scaling: Displays pixel-accurate and has slightly better performance, but appears small.
    *   Fullscreen: Fills the screen, but has a squished aspect ratio and uses interlaced scan.
*   Scaling type: Choose the scaling method to use for fullscreen mode.
    *   Static: The same horizontal lines on the screen are always doubled. This appears consistent but may cause shimmering effects when scrolling vertically.
    *   Scrolling: Attempts to double the same lines relative to the game's scrolling background. This reduces scrolling artifacts but may cause sudden shifts.
*   Skin display: Choose whether to display a skin in no scaling mode. Requires the `TIBoySkn.8xv` file.
*   Frameskip type: Choose the type of frameskip.
    *   Auto: Skip up to N frames as needed, or fewer if there is enough processing time. Always skips N frames when turbo mode is active.
    *   Manual: Always skip N frames, so 1 of every N+1 frames is rendered.
    *   Off: Never skip frames. May cause lag depending on the game.
*   Frameskip value: Choose the value of N described above, from 0 to 9. Ignored if frameskip is off.
*   Speed display: Choose when to show an indicator of the percentage of actual game speed.
    *   Never: Never display.
    *   Turbo: Display when turbo mode is active. Note this may be below 100% if the game cannot be emulated at full speed.
    *   Slowdown: Display when turbo mode is active, or when running below 100% speed.
    *   Always: Always display.
*   Message display: Choose whether to display emulator message overlays, for example when a save state is loaded.
*   GB palette selection: Chooses GBC-style colorization options to apply to original Game Boy games.
    *   Default: Use the official palette for a game as a GBC would select. If no official palette exists, reverts to Classic.
    *   Classic: Use a green-ish palette to evoke the feel of an original Game Boy.
    *   Others: The palettes choosable on a GBC through button combinations are made available as additional options.
*   Adjust colors: Choose whether to adjust colors to more closely mimic a GBC or GBA display. Ignored for the Classic palette.

### Control options

Remap controls as described in the [Default controls](#default-controls) section. Select an option to remap, then press the button to remap it to or press <kbd>on</kbd> to cancel.

If a chosen button is already mapped to another button, the two buttons are swapped. Any conflicting buttons between the global and per-game configuration will be removed from the per-game configuration automatically.

Emulator shortcut buttons, aside from the menu and quick exit buttons, may be unmapped using the <kbd>del</kbd> key.

### Emulation options
*   Preferred model: Choose the preferred type of Game Boy to emulate. Requires a game restart to take effect.
    *   Game Boy: Forces emulation of the original Game Boy, even for Game Boy Color enhanced titles.
    *   Game Boy Color: Emulates a Game Boy Color if the title is enhanced, otherwise the original Game Boy.
    *   GBA back-compat: Same as Game Boy Color, but tells the game that it is running on a Game Boy Advance.
*   Auto save state: Choose whether to create an auto save state when exiting a game. This state is separate from slots 0-9, and is automatically loaded the next time the game is started.
*   Confirm state save/load: Choose whether to prompt for confirmation when loading a state and/or overwriting a state. Useful to prevent accidental loss of progress. Restarting the game is also considered as loading a state.
*   Turbo mode: Choose how turbo mode is activated.
    *   Toggle: Press the turbo button once to toggle the turbo mode on or off.
    *   Hold: Hold the turbo button to turn turbo mode on, and release the button to turn it off.
*   Time zone: For games using real-time clock functionality, choose the time zone used relative to the time set in the calculator OS. Relevant when [Converting Save Files](#converting-save-files).
*   Daylight Saving Time: Turn this option on or off after adjusting the time in the calculator OS to keep the game clock consistent.

## File Types

The various files used by the emulator are as follows (replace Name with ROM prefix and # with digits):

| File         | Description                                                  |
|:-------------|:-------------------------------------------------------------|
| Name.8xv     | The file specifying the game title and how large the ROM is. |
| NameCfg.8xv  | Per-game configuration settings for this ROM, if any.        |
| NameR##.8xv  | Multiple files containing the actual ROM data.               |
| NameSAV.8xv  | The contents of the battery-backed cartridge save data.      |
| NameStA.8xv  | The automatic save state for this game.                      |
| NameSt#.8xv  | The manual save state for the given numbered slot.           |
| NameSv#.8xv  | The cartridge save associated with a numbered save state.    |
| TIBoyCE.8xp  | The executable launcher.                                     |
| TIBoyCfg.8xv | The current emulator configuration.                          |
| TIBoyDat.8xv | The core emulator data, loaded by the launcher.              |
| TIBoySkn.8xv | Optional skins to be displayed in “no scaling” mode.         |

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

A build newer than SPASM-ng v0.5-beta.3 is required to build TI-Boy CE properly; currently, there is not a release corresponding to that so you must build it yourself.

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
Copyright © 2018 – 2022 Brendan Fletcher
