---
layout: post
title: "Just-In-Time Recompiling the Game Boy: Part 2 (Recompiler Design)"
categories:
  - Design
tags:
  - Emulator Design
  - JIT Recompiler
---

[Last time]({{site.baseurl}}{% post_url 2020-08-27-just-in-time-recompiling-the-game-boy-part-1-introduction %}), we went over the design goals leading to the use of JIT recompilation to make a Game Boy emulator on a calculator. Now, let's dig more into how this is accomplished.

## The Execution Environment

As briefly mentioned in the previous post, the TI-84+ CE runs on a Zilog eZ80 processor, which has both 24-bit and 16-bit addressing modes. The processor can execute in either of these modes, which determines the width of the PC (Program Counter) register and the default width of operations on other multi-byte registers. However, there are also opcode prefixes which allow individual instructions to explicitly use an addressing mode, or in the case of branches, change the mode. So, it is possible (at a slight cost) to access 24-bit addresses in 16-bit mode, or vice versa.

TI-Boy runs recompiled code in 16-bit mode for several reasons:

* 16-bit code is smaller and more efficient than 24-bit code -- multi-byte immediate values (including for branches like JP and CALL) are one byte smaller, and stack pushes/pops (including for branches like CALL and RET) also access one fewer byte.
* Since the base of the 16-bit address space within the 24-bit address space is configurable, it can be set to an entire 64 KiB of RAM. This means that any special addresses (like the RST vectors) are under control of the emulator, unlike the 24-bit equivalents which are under control of Texas Instruments' read-only boot code. Being able to use RST vectors allows further improvements to generated code size and efficiency, because the RST instruction is effectively a CALL that's only 1 byte in size instead of 3, with the limitation that only 8 call destinations are usable.
* Most values kept in multi-byte registers during CPU emulation need only be 16-bit, because they mostly represent Game Boy CPU registers. Additionally, this means no opcode prefixes are necessary for emulating the Game Boy's few 16-bit operations.

There are a couple of downsides to using 16-bit mode, which are arguably not dealbreakers:

* The 16-bit address space puts a hard limit on the amount of recompiled code that can be cached at a time. However, given the relatively small amount of RAM available on the system in the first place, there would have had to be some kind of limit regardless. In practice, I've not seen any games where the 50-something KB limit on JIT code (accounting for statically allocated 16-bit subroutines) was so small that it couldn't cache all the code for many game frames in a row.
* Accesses to the emulated Game Boy address space (such as ROM and RAM) require usage of 24-bit pointers. This means either using opcode prefixes or switching entirely to 24-bit mode, depending on how much data is being accessed. However, the largest source of memory accesses – opcode fetches – are implicitly eliminated by the usage of recompiled code. For other typical memory accesses by an emulated CPU instruction, only 3 prefixed instructions are needed (load base address, add offset, and access memory), which is already balanced out by the reduced CALL and RET overhead. Additionally, the Game Boy's OAM, I/O, and HRAM are allocated in the 16-bit address space at their original addresses (0xFE00 to 0xFFFF), which allows direct access via 16-bit pointers.

## JIT Code Management

Now that we've gone over where the recompiled code is executed, how is it put there and tracked? To start, here's the layout of the 16-bit address space:

* RST routines (64 bytes)
* Statically allocated routines (around 10 KB as of this writing)
* JIT code space (grows forward)
* JIT trampoline space (grows backward)
* CPU flags translation table (256 bytes)
* Hardware stack (256 bytes)
* Game Boy OAM (256 bytes)
* Game Boy I/O (128 bytes)
* Game Boy HRAM (128 bytes)

JIT code is allocated using a bump allocator, and JIT trampolines are allocated in the same buffer, but growing back from the end. I'll cover trampolines at a later time, but just know that they're used to patch recompiled instructions which are later determined to need more space than was originally allocated. When newly allocated JIT code collides with the trampolines, or vice versa, the buffer is flushed of all code.

The JIT code, like in most JIT implementations, is organized in "blocks" of contiguous code. However, unlike many other JITs, these blocks are not limited to [basic blocks](https://en.wikipedia.org/wiki/Basic_block). The only requirement is that execution can flow all the way from the beginning to the end of the block – meaning blocks may continue through conditional branches, or even through unconditional CALL instructions (since execution is extremely likely to flow back through the corresponding RET). However, basic blocks contained within blocks are still used as the fundamental unit of cycle counting.

Blocks are uniquely tagged by a representation of their starting Game Boy virtual and physical addresses. This sounds like a lot of data, but it's really just the 16-bit virtual address combined with an 8-bit physical memory bank for swappable regions (since the remainder of the physical address is implied by the virtual address). The virtual address is required because each block may implicitly use the value of the PC register in any instruction, and it's possible for the same physical address to be mapped to multiple virtual addresses (for example, ROM bank 0 in MBC5 cartridges). Note that this representation excludes 8 MiB cartridges with 512 ROM banks, but the calculator's usable Flash storage is only 3 MiB, so it's not worth the effort of tracking 9-bit banks.

Each block is tracked in an 11 KiB array of structures of the following information (allowing for up to around 1400 blocks at a time in total):

* JIT code start address (2 bytes)
* Block tag (3 bytes)
* Opcode byte count (1 byte)
* Reserved (formerly upper byte count) (1 byte)
* Cycle count of first basic block (1 byte)

## JIT Code Lookups

Another different design choice from many other JITs is having a clear border between each emulated instruction where the registers are in a consistent state, which allows entry into a block starting at any instruction. This is made possible without a large amount of overhead mainly due to the CPU flag similarities between the Game Boy and eZ80 processors. This allows much more effective block reuse, saving a lot of memory and justifying the larger block size. Additionally, this allows for _exiting_ a block between any two instructions, which I'll discuss later when we get to interrupt handling.

The block tag, combined with the opcode byte count, is used as a range check to determine if a desired code address may be present in the block. It only _may_ be present because Game Boy opcodes are variable-length, so it's not known whether an address is at the start of an instruction until the block is scanned.

Blocks can be easily scanned without storing auxiliary information about each instruction in the block, aside from referencing the original opcodes. This is because with a few exceptions, a given opcode always translates to the same number of bytes when recompiled and takes the same number of cycles, no matter which block it's contained in or what its operands are. The only special cases needed are for opcodes which terminate a basic block (after which the cycle count for the next basic block must be extracted from the JIT code) and the few opcodes with variable-length JIT implementations.

Similarly, the code can be scanned to translate a JIT code address back to a Game Boy code address (which is primarily used for trampoline generation for memory accesses), or to scan forward by a given number of cycles within a basic block (which is primarily used for interrupt scheduling). Both of these usages will be discussed in more detail later.

## Cached Lookups

Code scanning is very nice for memory usage, but not so great for performance if done repeatedly. Ideally, a given Game Boy code address should only be looked up once, and the result cached for future uses.

### Block Linking

The most basic form of JIT address caching is through direct block linking. That is, when a branch instruction with a constant destination address (such as JR, JP, or CALL) is executed, after looking up the JIT target, that target address can be patched directly into the implementation to avoid any future lookups. This is known as "block linking" because it often creates a direct reference to another JIT block, though my implementation easily allows intra-block linking as well.

The most likely case for the same branch target being looked up from multiple different instructions is from CALLs, but I suspect that the cost of doing so is not very high due to CALL targets likely being at the very start of a block, which has no instruction scanning overhead. As such, it's not worth having an additional cache for such lookups.

There is one hitch with block linking, which is that the branch instruction only specifies the _virtual_ address being branched to. In the context of memory bank swapping, this may cause some links to be invalid if a different bank is swapped in at the time of the branch. In fact, I discovered this the hard way when some games did this intentionally. My solution to this was to use a special implementation when branching into a banked memory region, which validates the current bank. You might think this would cause a huge dip in performance, but fortunately this is only necessary when jumping from _outside_ the banked region. Jumps within the banked region are guaranteed not to have a bank mismatch, so this doesn't really cause a lot of extra checks in the end.

### Variable Jumps

Of course, not all jumps in a game will be to absolute addresses. Using function pointers or jump tables is a very common technique, and on the Game Boy this is accomplished through the JP HL or RET instructions.

To deal with these kinds of jumps, I use a simple hash map of tagged Game Boy addresses to JIT address and cycle count. In fact, when I say simple, it's _extremely_ simple. There are 256 fixed hash buckets with expandable lists, and each bucket corresponds to the low byte of the Game Boy address. This trick allows the low byte to be omitted from the list entries, reducing each entry's size to 5 bytes. This technically makes lookups linear time, but it's linear with a constant factor of 256 divided out, which in practice is almost constant time with typical usage of this cache.

How does this cache avoid getting thrashed with the large number of RET destinations from CALLs, or worse, interrupts? That leads to the final type of cached lookup:

### Return Prediction

Just like for branch prediction in modern CPU design, it's quite effective to assume a RET will return to the same place as the most recent CALL. This is the primary reason I allow JIT blocks to continue through even unconditional CALL instructions – the JIT address of the following code can be placed directly onto a return prediction stack. In fact, by storing this stack on the eZ80's hardware stack, this address can be directly pushed by calling the CALL implementation.

I store four pieces of information on the prediction stack:

* The predicted JIT return address (2 bytes)
* The cycle count at that JIT address (1 byte)
* The relative level of the Game Boy stack (1 byte)
* The predicted Game Boy return address, tagged with a memory bank change sentinel (3 bytes)

Much like for block linking, changes to the memory bank must be detected to know if the prediction is valid. However, it would be quite inconvenient when emulating RET to check which memory region the predicted return address is in, load the appropriate bank index, and compare to it. This is especially wasteful when most CALL/RET pairs perform no bank swaps inbetween whatsoever.

So, instead of storing the predicted memory bank on the prediction stack, instead a sentinel is placed on the stack, always initialized to 0. Then, whenever a bank is swapped, the topmost entry of the prediction stack which corresponds to that region is updated, using the XOR of the old and new bank values. Eventually, if the bank is swapped back to what it was when the CALL occurred, the sentinel value will return to 0.

Now, when emulating RET, the real 16-bit return address is popped from the Game Boy's stack, and is compared to both the predicted return address and the sentinel by using a 24-bit comparison. If the comparison matches (as it would in the vast majority of cases), it simply counts cycles using the predicted cycle count and returns to the predicted JIT address.

If the comparison mismatches, it first checks the relative stack level (based on the low bits of the Game Boy's SP register). If the popped return address was higher on the stack than the prediction, then it is assumed that the prediction may still be valid in the future, and ensures anything that was popped is pushed back. Otherwise, the prediction is removed from the stack, and any non-zero memory bank sentinel is propagated down to the next entry in that region. If the popped return address was _lower_ on the stack than the prediction, it loops back and attempts to pop the next prediction entry. Finally, if no prediction matched, it falls back on the variable jump cache.

The final consideration is stack overflow – naturally, I must bound the return prediction stack in order not to overflow the hardware stack. This is done using a simple comparison when pushing in the CALL implementation, to avoid exceeding 32 stack entries. If the limit is exceeded, the stack is flushed, meaning any predictions on the stack that would have been valid will end up falling back on the variable jump cache. This is a fairly rare situation in practice, however.

## Self-Modifying Code

So far, I've ignored the elephant in the room – JIT blocks which are sourced from code in RAM, which may change after (or during!) their initial execution. My current solution is not very cleverly designed, but because the frequency of RAM-based code in Game Boy games is quite low, I used a potentially inefficient solution which doesn't interfere with other emulator operations, such as memory writes.

For RAM-based JIT blocks, I copy the original opcode bytes to the end of the block, and add a prologue at the beginning which compares this copy of the opcodes to the actual opcode bytes in Game Boy RAM. If the bytes all match, the block is allowed to run normally. Otherwise, the JIT engine will attempt to recompile the new opcodes within the space originally allocated for the block. If this causes a buffer overflow, it flushes the JIT code and increases a global RAM block padding amount by the amount of overflow, to avoid that overflow in the future. It's potentially wasteful, but it works.

Since RAM-based blocks may be recompiled in-place, it is not allowed to perform block linking or variable jump caching into the middle of a block. Return prediction is allowed, but the return prediction stack is checked after an in-place recompile to remove any predictions pointing to that block.

However, lack of block linking could make RAM-based loops extremely slow, since it would perform the opcode comparison on every loop iteration. As such, I do specifically allow _intra-block_ linking for RAM-based code. This is fine because any such links are implicitly destroyed along with the old block when the in-place recompile occurs.

One other performance consideration is dealing with a relatively common self-modifying code idiom in Game Boy games: implementing function pointers by placing an absolute JP opcode in RAM, which is often more efficient than loading HL from memory and using JP HL. In my emulator's case, it is quite inconvenient to perform an in-place recompile every time the jump target changes, so instead I use a special block prologue which only verifies the JP opcode, then uses the variable jump cache on the current jump target.

One final consideration, especially due to the JIT block size, is how to handle when a block modifies itself (the true form of "self-modifying" code). My current solution is to assume that such modifications are performed via absolute address writes, and I do analysis of such writes during recompilation. If such a write is targeting an address close ahead of PC, I end the current block early, which allows the next block's prologue to perform opcode comparison and do any recompilation which may be needed. This simple heuristic doesn't seem to have failed in any games I've tested.

## Conclusion

Hopefully, now you understand more about how my JIT is organized, and how I try to avoid redundant JIT code lookups, as well as how self-modifying code fits into the picture.

Next time, I may talk about cycle counting, scheduling, and interrupts. Until then, happy coding!
