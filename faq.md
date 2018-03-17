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