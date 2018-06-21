---
layout: page
title: FAQ
sidebar_link: true
---

**I ran into a bug or problem, how should I report it?**

<p class="message">
  Check out the <a href="{{ site.github.repo }}/issues">Issues on GitHub</a> and see if there's already an open issue.
  Also, read the rest of this FAQ to see if it addresses your problem.
  <br/><br/>
  If not, create an issue for the problem through the link above. Thanks!
</p>

**Can this play Game Boy Color games?**

<p class="message">
  Not for now, but support may be added in a future version.
  <br/><br/>
  I can't make any promises about performance, though.
</p>

**What about Game Boy Advance?**

<p class="message">
  No. Just no.
</p>

**My game isn't colorized, why not?**

<p class="message">
  Only games in the Game Boy Color's colorization database will receive an automatic color palette.
  For the others, you can select a palette manually in the settings.
</p>

**How does saving work?**

<p class="message">
  If a game has in-game saves stored on the cartridge, these are always saved when exiting the game (to a file ending in SAV).
  These will be preserved when restarting the game, and in the unfortunate case of a soft crash the emulator will attempt to save this data.
  <br/><br/>
  By default, the emulator will also automatically save the current game state when exiting (to a file ending in StA)
  and automatically load it when starting again, much like Nintendo's Virtual Console.
  Note that the StA file will not load without the corresponding SAV file.
  <br/><br/>
  In addition, you can save or load the current game state to one of 10 slots through the emulator menu
  (to a file ending with St#, and potentially one with Sv# if there are in-game saves, where # is the slot number).
  These will stick around more reliably than auto save states, but keep in mind that a pair of St# and Sv# files can't be mixed and matched.
  <br/><br/>
  Currently all these files must be managed through the calculator's Memory menu, but in future releases I may make it possible to delete
  files directly from the emulator.
  However, all of this information will still be useful when you need to transfer files to a PC or another calculator.
</p>

**I tried sending converted ROM files to my calculator, but it tells me I'm out of space even though I clearly have enough! What's going on here?**

<p class="message">
  This is a quirk of TI's Archive storage system. Archive memory is split into 64KB sectors, and any given variable must be contained completely within that sector.
  <br/><br/>
  Meanwhile, converted ROM files are split into 16KB contiguous chunks for performance reasons (several of these chunks may be contained in a single AppVar).
  <br/><br/>
  If there is no empty space at the end of these ROM chunks that can be trimmed out, only three of them can fit into an AppVar, taking slightly over 48KB of space.
  Whichever Archive sector holds this variable can only store files less than 16KB large in the remaining space, which means no more ROM data.
  <br/><br/>
  In this worst case scenario (when no trimming is possible), this would mean you can only use 3/4 of your Archive space to store ROMs.
  <br/><br/>
  <b>tl;dr:</b> It's expected behavior but there's no good solution for it other than deleting large files to make room. Sorry!
</p>

**I want to link to my buddy's calculator so we can trade pokeymans! Pretty please?**

<p class="message">
  I'd like to make linking work, it would be pretty cool! I even bought a second calculator so I can mess around with that idea.
  <br/><br/>
  USB is difficult though, and so is synchronizing emulation between two devices. I can't make any promises, but I'll at least give it a try sometime!
</p>