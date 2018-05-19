---
layout: post
title: "Progress Update: Save File Management"
categories:
  - Progress Update
tags:
  - Progress Update
---

I took a longer break from working on TI-Boy than intended, but when I got back to it, as originally planned
I reworked my save file management to allow for future extensibility for features like compression,
or supporting more platforms like Game Boy Color.

As far as future platform support is concerned, this meant splitting save states into two files: one for the internal Game Boy state
and one for the battery-backed cartridge RAM used for save files on real hardware. But why, you ask?

The first reason is simple: the Game Boy Color's internal state and the cartridge save combined could exceed the maximum size
of a calculator variable, which is around 64KB. This isn't a problem as far as my currently supported platform (original Game Boy) is concerned,
but it would be more trouble to change this later and break compatibility.

The second reason is that it can reduce the frequency of Garbage Collects. Having two smaller files means that finding free space in
Archive sectors is more likely, but more importantly, I can avoid deleting and recreating the cartridge save file if it hasn't changed
since the last time the state was saved. Of course, the internal state file will practically always be different, so this optimization doesn't apply there.

As for save file compression, well... I had originally planned to defer it to after the first release, but I got so into the idea when adding
a framework for it that I went ahead and implemented it. Since the calculator's processor is fairly weak and I didn't want saving
the state to take a long time (or to take an excessive amount of code space), I decided to go with [LZF compression](http://oldhome.schmorp.de/marc/liblzf.html).
It seems to work as advertised, and it's definitely fast. And fortunately, since I only compress data after exiting the emulator
core, there's enough free scratch memory at that point for the needed hash table.

On a side note, you may be wondering why the ROM itself isn't compressed. Well, that's simply because there's not enough RAM
to load a decompressed ROM, and the emulator needs instant access to any byte in the entire thing. When stored in Archive,
the entire ROM is readable directly, without needing to be copied to RAM first.

With all that out of the way, I know you're going to ask when it's coming out, but hold on a bit longer. I need to finish up these changes
and ensure they're thoroughly tested, because memory and data management are where you'll be the least happy when something goes wrong.
Trust me, I know.

Anyway, see you next time!
