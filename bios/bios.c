#include "types.h"
#include "x86.h"

#define SECTSIZE 512

int
main(void)
{
  void *dst = (void *)0x7C00;
  uint32_t offset = 0;
  // wait for disk to be ready
  while ((inb(0x1F7) & 0xC0) != 0x40) { }

  // ref. "7.35 READ SECTOR(S)" in [ATA8-ACS]
  outb(0x1F2, 1);                     // sector count = 1
  outb(0x1F3, offset);                // LBA low : LBA28 [ 0: 7]
  outb(0x1F4, offset >> 8);           // LBA mid : LBA28 [ 8:15]
  outb(0x1F5, offset >> 16);          // LBA high: LBA28 [16:23]
  // NOTE. 0x1F6:
  //  Bit 7: obsolete in LBA, 1 in CHS
  //  Bit 6: mode, 1=LBA, 0=CHS
  //  Bit 5: obsolete in LBA, 1 in CHS
  //  Bit 4: dev, 0=master, 1=slave
  //  Bit 3-0: [24:27] in LSB, #head in CHS
  outb(0x1F6, (offset >> 24) | 0xE0); // Dev: LBA28 [24:27], LBA=1, master
  outb(0x1F7, 0x20);                  // cmd 0x20 - read sectors

  // wait for disk to be ready
  while ((inb(0x1F7) & 0xC0) != 0x40) { }

  // read a sector from a data port
  insl(0x1F0, dst, SECTSIZE/4);

  return 0;
}
