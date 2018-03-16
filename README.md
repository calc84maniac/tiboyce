TI-Boy CE
=========

TI-Boy CE is a Game Boy emulator for the TI-84 Plus CE and the TI-83 Premium CE
graphing calculators.

Currently only the original Game Boy is supported, no Game Boy Color (and never
Game Boy Advance).

This emulator is currently in the alpha state, so while it is intended to be
stable, it is possible that it could crash and cause data loss. It is advised
to put any important files in Archive memory before running the emulator.

Grab the latest pre-built releases at https://github.com/calc84maniac/tiboyce/releases
or check out the build instructions below to build from source.

Features
--------

* Emulates original Game Boy hardware (except audio and linking)
* Emulates real-time clock for certain cartridges
* Save states
* Fullscreen and 1:1 scaling modes (with optional skin)
* Automatic and manual frameskip
* Turbo mode (with speed display)
* GBC-style selectable color palettes for Game Boy games
* Customizable controls

Converting ROM files
--------------------

A command-line utility (tiboyce-romgen.exe) to convert ROM files to TI AppVars
is included. There are a couple of ways to do this:

1. Open a command prompt and navigate to the directory containing the utility
   using `cd`. Then run the following:

       tiboyce-romgen.exe -t "Game Title Here" path\to\romfile NamePrefix

2. Drag-and-drop a ROM file onto the utility, which will then prompt for a
   game title and a name prefix.

Note that the name prefix provided for the ROM must be at most 5 characters
long. This is because the remaining 3 characters are used for naming additional
files created by the utility and the emulator itself.
The game title provided will be displayed in the emulator's ROM list.

The utility will generate multiple AppVar (*.8xv) files with the given prefix.
Send all of them to the calculator in Archive memory.

Running the emulator
--------------------

Send the `TIBOYCE.8xp` and `TIBoyDat.8xv` files to the calculator.
Optionally also send the `TIBoySkn.8xv` file which contains a skin image.

If your calculator is running OS v5.3 or newer, you can keep all of these
files in Archive and run `prgmTIBOYCE` from the `prgm` menu.

If your calculator is older than OS v5.3, the `TIBOYCE` program must be
unarchived. Run it with `Asm(prgmTIBOYCE)`.

You should now see a list of the ROMs on the calculator.
Choose one with `up/down` and start it with `2nd/enter`.

Default controls
----------------

Game controls:
* D-Pad: `Arrow buttons`
* A: `2nd`
* B: `alpha`
* Start: `mode`
* Select: `XTθn`

Emulator controls:
* Emulator menu: `clear`
* Turbo: `zoom`
* Quick exit (non-configurable): `on`

Menu controls (non-configurable):
* Choose menu item: `up/down`
* Change option: `left/right`
* Select item: `2nd/enter`
* Close menu: `clear`

File Types
----------

The various files used by the emulator are as follows
(replace Name with ROM prefix and # with digits):

    Name.8xv - The file specifying the game title and how large the ROM is.
    NameR##.8xv - Multiple files containing the actual ROM data.
    NameSAV.8xv - The contents of the battery-backed cartridge save data.
    NameStA.8xv - The automatic save state for this game.
    NameSt#.8xv - The manual save state for the given numbered slot.
    TIBoyCE.8xp - The executable launcher.
    TIBoyCfg.8xv - The current emulator configuration.
    TIBoyDat.8xv - The core emulator data, loaded by the launcher.
    TIBoySkn.8xv - An optional skin to be displayed in "no scaling" mode.

Note that the auto save state cannot be loaded properly if the cartridge save
data file is deleted.

Build Instructions
------------------

To build the emulator from source, first grab the latest release of [SPASM-ng](https://github.com/alberthdev/spasm-ng/releases).

For simplicity's sake, I'll call the name of the executable `spasm` below. Run the following to produce the emulator files:

    spasm -E -A launcher.asm TIBOYCE.8xp

    spasm -E -A tiboyce.asm TIBoyDat.8xv

    spasm -E -A skin.asm TIBoySkn.8xv

To build the rom generation tool, use the provided Visual Studio solution in the [tiboyce-romgen](tiboyce-romgen) directory, or theoretically you can build [romgen.c](tiboyce-romgen/romgen.c) with your C compiler of choice.

Issues/Bugs
-----------
Report issues/bugs to the issue tracker, found here:

https://github.com/calc84maniac/tiboyce/issues

License
-------
    TI-Boy CE - a Game Boy emulator for the TI-84 Plus CE calculator family.
    Copyright (C) 2018 Brendan Fletcher

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
