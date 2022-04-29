# Memory Management Notes

## Memory management: Algorithms and Implementations by Bill Blunden

Memory management has two sides:

- Mechanism: Provided by the processor
- Policy: Provided by the operating system.

Relying on the mechanism ties the operating system to
a particular hardware. Relying entirely on
the virtual memory is simply too slow.

Random Access Memory (RAM), main memory or primary
memory implies that the data inside them is lost,
once the power is off.

There are 4 types of RAM:

- DRAM: Dynamic RAM recharged a lot of times each second
- SDRAM: Synchronous DRAM is refreshed at the clock speed of the processor
- SRAM: Static RAM is not recharged a lot like DRAM. It is mostly used in
  processor caches.
- VRAM: Video RAM, used by the video hardware, GPU etc.

A *bit* is a single binary digit 0 or 1. In a RAM chip, it corresponds to a
cell structure made up of certain configuration of transistors and capacitors.
The configuration depends on the type of RAM. 
A cell has two states, on that is 1, or off, that is 0.
These cells are grouped into 8-bit units, called bytes.
Byte is the fundamental unit for measuring the amount of memory
provided by a storage device.

The memory that is the data storage is not a unique feature to
RAM. Depending on the proximity to the processor, one can 
establish the following hierarchy:

1. Registers
2. Cache
    - L1: Located on the processor itself.
    - L2: is typically an SRAM chip outside of the processor
3. RAM
4. Disk storage

The main distinction between these storage areas is their lag time
or *memory latency*. Storage that is closer to processor takes less time
to access, for example Registers are faster than Cache which is in turn
faster than RAM, etc.

Traditionally, disk space has been used to create
*virtual memory*.
Each byte, 8bit unit, has an associated integer. This integer is often
referred as the *address* of the byte in DRAM.
The *low memory* refers to the region near the 
address 0. The *high memory* refers to the region near 
the final address. The *physical address space* refers to the
total number of physical bytes (in DRAM for example) that can
be addressed by the processor. It represents a potential.
The address of a byte is specified using address lines.
Address lines are small wires that connect the processor
to DRAM chip. Each line constitute a single bit in the 
address of a byte. For example if we have 8 address lines,
and a byte that is being addressed.
The address has, 8 slots each composed of bit referring to
an address line. For example an address like
01101011 indicates that first address line is off,
second is on, third is on, fourth is off, etc.

To access and update the physical memory, the processor
needs a control bus and a data bus. A *bus* is a collection of
related wires that connect the processor to a hardware
subsystem.
The *control bus* indicates if the processor wants to read or
write data to memory.
The *data bus* does data transactions between the processor and 
the physical memory.

The reading from the memory occurs the following way:

1. Processor places the address of the byte to be read 
on the address lines.
2. Processor sends the read signal via control bus.
3. DRAM chip(s) return the byte specified on the data bus.

The writing occurs the following way:

1. Processor places the address of the byte to be written 
on the address lines.
2. Processor sends the write signal via control bus.
3. Processor sends the byte to be written to memory on the data bus.

Processors tend to support two memory management mechanisms as well:
segmentation, and paging.
Segmentation happens by breaking up the address space into
specific regions, known as *segments*.
This affords memory protection, where each application is assigned
to at least one segment (large applications can be assigned to several
segments).

Segments are assigned to specific access writes so that the policies can
be created with regard to who can update what. Typically, the 
operating system code has the highest privilege and applications
are loaded to segments with less authority.

Paging is a way to implement virtual memory. The physical memory of DRAM
combined with the disk storage creates an amorphous storage space. In this
configuration, the total number of bytes that a processor is capable of
addressing is known as *virtual address space*.

A *page* is a small region in the virtual memory space.

An address in Intel assembly language is denoted by a segment:offset pair, for
example: 0x800 : 0x100 where 0x800 is the segment, and 0x100 is the offset. 
The offset is designates the value to add to the base address in order to
obtain the location. Base address is the lowest address of a memory segment.

Segment address specifies the base address, that is the lowest address of a
memory segment. Other addresses inside the same memory segment are designated
using an offset as seen above:

Each segment register has a specific use: 

CS: segment address of code that is being executed right now.
SS: segment address of stack
DS: segment address of data
ES: Extra segment address (usually data)
FS: Extra segment address (usually data)
GS: Extra segment address (usually data)

Six segment registers means, only six segments of memory can be manipulated. A
program can use more than six segments, but only six of them is accessible to
processor at a time.

As we evolve into protected memory management the segment addresses are
specified using specific data structures.

Segment registers holds segment selectors.
Segment selectors are 16 bit data structure containing three fields.
It holds privilege of the memory segment, the descriptor table type, 
and an index field.
The important part is the index field.
The index field points to the linear address after being processed by the
processor.

A descriptor table is an array. It contains segment descriptors. A segment
descriptor describes the attributes of a specific memory segment whose base
address is also contained inside the descriptor. 32-bit offset address is then
added to this base address in order to find the address of a byte in memory.

There are 2 descriptor tables:

- Global descriptor table: Every operating system has one.
Its base address is stored in GDTR system register.

- Local descriptor table: It is optional. When it is used, it is used for
  representing memory segments belonging to a specific process.


GDTR is 48bit in size. The first 16 bit contains 
the size limit of GDT, and the second 32 bit contains the base
*linear address* of GDT in memory.

When paging is not enabled, the linear address and physical address of a
memory segment is same.

A segment descriptor contains the following:

- bunch of metadata: 
    - segment limit
    - default size
    - privilege
    - presence bit: 1 if segment is in memory 0 if it is not.
    - etc

- SS: system segment bit: 1 if it is, 0 if it is not.
- segment type: 4 bits.
    - data: contains bit combinations signaling different
    states of data (accessible, read-only, etc)
    - code: contains bit combinations signaling different 
    states of code (execute-only, accessed, etc).

Privilege indicators help processor determine
memory violation.

This helps to protect memory at the hardware level.

p. 31 - Paging as Protection

