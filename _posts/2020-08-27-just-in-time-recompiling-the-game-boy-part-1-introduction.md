---
layout: post
title: "Just-In-Time Recompiling the Game Boy: Part 1 (Introduction)"
categories:
  - Design
tags:
  - Emulator Design
  - JIT Recompiler
---

This will be a series of blog posts describing how JIT recompilation is used in the TI-Boy CE emulator.
A general familiarity with assembly languages, especially Game Boy or Z80, is recommended background for
reading.

Let's start with some information about the platforms we're working with, shall we?

## The TI-84+ CE Processor

The TI-84+ CE calculator runs on a Zilog eZ80 processor, which is a slightly more modern Z80
with a larger address space of 24 bits, compared to the Z80's 16 bits. This allows all memory to be
directly addressed without memory paging. It is still classified as an 8-bit processor, however.
The eZ80 also has a Z80 backwards-compatibility mode, which allows Z80 code to execute within
a specified 16-bit subset of the address space. TI-Boy makes extensive use of this mode, partially
because it allows for smaller, more efficient code and also to simplify emulation of 16-bit
Game Boy operations.

The CPU is clocked to 48 MHz and the improved pipeline of the eZ80 allows for single-cycle execution
of many instructions. However, the TI-84+ CE has a slow memory bus which adds wait states to all
memory accesses, preventing the processor from reaching its full potential.
Specifically, 3 wait states are added to reads from RAM, and 1 wait state is added to writes.
Since the CPU has no instruction cache, the opcode fetches multiply most instruction cycle times
by a factor of 4. This makes the cycles per instruction closer to that of the original Z80 processor,
though 16-bit additions and address offsets are still considerably faster on the eZ80.

Note: The TI-84+ CE added significantly more wait states for reads from Flash memory, so execution
from Flash was less viable for speed-critical code. However, on newer CE hardware revisions, the Flash
chip was replaced with one that had extremely slow random access but relatively fast sequential access.
To compensate for this, TI added a transparent cache on top of the Flash memory region which would
sequentially load data into cache lines on miss. Only 2 wait states are added for cached reads,
which are even faster than RAM reads as long as the cache hits. TI-Boy has not yet been updated to
take advantage of this, though notably a large portion of the emulation cannot benefit from the cache
because JIT recompiled code must run from RAM. It does implicitly make reads from the Game Boy ROM
a bit faster, however.

## The Game Boy Processor

The original Game Boy runs on a custom 8-bit Sharp CPU, which has many similarities to the Z80 processor
(though in some ways it's closer to the Z80's predecessor, the Intel 8080, in terms of feature set).
These similarities include a 16-bit address space, a mostly identical set of commonly used registers,
and mostly identical flag outputs from arithmetic and logical operations. Notably however, the Game Boy's
processor is missing index registers, shadow registers, and sign/overflow/parity flags, as well as many of
Z80's extended instructions like 16-bit subtraction and memory block operations. The Game Boy also has
no instructions to deal with an I/O port address space, opting instead to memory-map its I/O ports.

The Game Boy's CPU is clocked to 4 MHz, but unlike the Z80, all of its machine cycles are exactly 4 clock
cycles long (whereas the Z80's would vary from 1 to 6). So, it tends to be useful to count machine cycles
rather than clock cycles when emulating timing of the CPU, to reduce the range of the counter values.
For example, the number of clock cycles in a single video frame is 70224 clock cycles (which can't fit
in a 16-bit counter) but only 17556 machine cycles (which can). From this point on, any mention of
Game Boy CPU cycles in these posts should be assumed to be machine cycles.

## Why JIT Recompile?

You may be wondering, why not just make a normal CPU interpreter? That's what everyone does for
these old systems! Your processor is more than 10 times as powerful as the one you're
emulating anyway, right? That's the rule of thumb!

The simple answer: I've always really wanted to try this back on the Z80 with the original TI-84+,
but there just wasn't enough memory or address space to reasonably do so. The TI-84+ CE has 256 KiB
of RAM plus 150 KiB of frame buffer RAM, which is more than decent enough to do JIT recompilation
without running over memory capacity every frame.

The technical answer: I sincerely believe an interpreter wouldn't have given the performance I needed.
At its core, the eZ80 is still a legacy 8-bit system and doesn't lend itself well to many operations
that seem trivial on more modern systems, like dispatching through a jump table (or at a higher level,
using a switch statement). The instruction pipeline makes this even worse than on the Z80, due to
needing to refill the pipeline after each dispatch. The overhead from the interpreter dispatch alone,
which I estimate at around 50 clock cycles per instruction in the best case, puts the emulator in a
dangerous area of not emulating the simplest common Game Boy instructions at full speed.

That's not even considering the overhead of using CPU registers which would otherwise be available for
storing Game Boy registers, or emulating the memory map of the Game Boy (which can't leverage
any native CE hardware due to its flat memory map, meaning even more switch statements or at least
table lookups). Cycle counting for accurate timing is also out of the question at that point. And forget
any future prospects of emulating the Game Boy Color, which can clock its processor twice as fast!

In comparison, JIT recompilation offers these runtime advantages for TI-Boy, among others:

* One-time cost for translation -- at least until running out of code memory, which should be infrequent.
  Additional one-time costs (such as taking a jump for the first time) are deferred until needed.
* No register swapping between most instructions -- all common Game Boy registers can be made
  directly available in the main eZ80 registers, and CPU flags don't need to be saved and restored.
* Fast emulation of instructions once recompiled -- many common instructions run 12 times as fast
  as they would on the Game Boy, and a few (such as 16-bit operations) even run 24 times as fast!
  This is thanks to both the register availability mentioned above and the similarity of flag outputs.
* Coalesced cycle counting -- allows determining before entering a recompiled block whether a time-based
  event (e.g. a CPU interrupt) would occur at some point within that block, in most cases allowing the
  emulator to avoid counting cycles for each individual instruction. When individual instructions do need
  to be counted, their counts are dynamically coalesced as needed prior to entering the block.
* Abusing locality of reference -- specializing individual memory access instructions based on their most
  recently accessed memory region, greatly reducing the average overhead of memory map emulation.
* Identifying waitloop patterns -- when Game Boy code is in a loop waiting for an external
  change (either a hardware state change reflected by the value of an I/O port or the value of a RAM address
  to be modified by an interrupt), the emulator can determine that it is safe to skip redundant executions
  of the loop until the first time that external change can occur.

However, there are also disadvantages to JIT, aside from the obvious aspect of increased code complexity:

* Arbitrary jumps in Game Boy code can be slow -- these cannot be resolved at recompilation time because
  they cannot be known until each time they are executed. This includes both register-based jumps
  and subroutine returns (for which the return address may have been modified or entirely fabricated).
* Precise timing for memory accesses and CPU interrupts becomes difficult -- memory accesses must be able
  to determine their relative cycle count within the recompiled block, and interrupts must be dispatchable
  from an arbitrary point within any recompiled block. Additionally, some memory accesses to hardware I/O
  may cause a hardware state change which needs to schedule a new interrupt within the currently executing
  recompiled block.
* Dealing with memory mapping -- code on the Game Boy can be mapped in and out of its address space, so it
  is important to avoid making assumptions when emulating jumps between different areas of memory. Since
  jumps on the Game Boy refer to only a 16-bit address and exclude any mapping information, it cannot
  be assumed that the same jump instruction will always target the same recompiled code block, unless
  both the jump and destination are within the same memory page (or the destination is in a area that
  cannot be remapped). There are also issues when code is allowed to run straight from one memory area
  to the next without jumping.
* Detecting and responding to self-modifying code -- when Game Boy code is executed from RAM, there is
  a possibility the code may be replaced at a later time, or even just one byte of the code may be modified.
  At that point, the recompiled block does not match the current state of the code, which will cause
  inaccuracies. This issue is compounded if Game Boy code modifies data within its own currently executing
  block. Fortunately, most Game Boy code executes from ROM, but certain games do tricky things when copying
  code to RAM.
* Testing can be inconsistent -- the inherent extra context involved in JIT means that even if individual
  instructions are confirmed to emulate properly in isolation, there are many different contexts in which
  they can be executed, which can hide subtle bugs (not just in inputs/outputs, but also in timing).
  One thing I do when stress testing the emulator is reduce the size of the code pool to force more
  frequent recompilation, which greatly increases coverage of the different situations that can crop up.

Some of these disadvantages have been partially or fully mitigated in TI-Boy through various strategies,
which will likely be discussed in future posts. Overall, I've found that the advantages have greatly
outweighed the disadvantages, allowing the emulator to make great strides in both performance and accuracy.

## What about static recompilation?

Since I get asked this question sometimes, static recompilation is not a suitable option for many reasons.
The biggest reason is that it's not feasible to differentiate between code and data when not actively
running the game. Even if that were possible, it would also greatly increase the amount of storage space
taken by ROMs, and most likely cycle counting would have to be thrown out. Add to that the slowness of
executing from Flash memory on many calculators, and it's a very suboptimal solution.

Next time, we'll take a look at some of the JIT recompiler design details. Until then, happy coding!
