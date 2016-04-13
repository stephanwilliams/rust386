#ifndef X86_H
#define X86_H

static __inline uint8_t inb(int port) __attribute__((always_inline));
static __inline void insb(int port, void *addr, int cnt) __attribute__((always_inline));
static __inline uint16_t inw(int port) __attribute__((always_inline));
static __inline void insw(int port, void *addr, int cnt) __attribute__((always_inline));
static __inline uint32_t inl(int port) __attribute__((always_inline));
static __inline void insl(int port, void *addr, int cnt) __attribute__((always_inline));
static __inline void outb(int port, uint8_t data) __attribute__((always_inline));
static __inline void outsb(int port, const void *addr, int cnt) __attribute__((always_inline));
static __inline void outw(int port, uint16_t data) __attribute__((always_inline));
static __inline void outsw(int port, const void *addr, int cnt) __attribute__((always_inline));
static __inline void outsl(int port, const void *addr, int cnt) __attribute__((always_inline));
static __inline void outl(int port, uint32_t data) __attribute__((always_inline));


static __inline uint8_t
inb(int port)
{
  uint8_t data;
  __asm __volatile("inb %w1,%0" : "=a" (data) : "d" (port));

  return data;
}

static __inline void
insb(int port, void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\tinsb"                 :
                   "=D" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "memory", "cc");
}

static __inline uint16_t
inw(int port)
{
  uint16_t data;
  __asm __volatile("inw %w1,%0" : "=a" (data) : "d" (port));

  return data;
}

static __inline void
insw(int port, void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\tinsw"                 :
                   "=D" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "memory", "cc");
}

static __inline uint32_t
inl(int port)
{
  uint32_t data;
  __asm __volatile("inl %w1,%0" : "=a" (data) : "d" (port));

  return data;
}

static __inline void
insl(int port, void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\tinsl"                 :
                   "=D" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "memory", "cc");
}

static __inline void
outb(int port, uint8_t data)
{
  __asm __volatile("outb %0,%w1" : : "a" (data), "d" (port));
}

static __inline void
outsb(int port, const void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\toutsb"                :
                   "=S" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "cc");
}

static __inline void
outw(int port, uint16_t data)
{
  __asm __volatile("outw %0,%w1" : : "a" (data), "d" (port));
}

static __inline void
outsw(int port, const void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\toutsw"                :
                   "=S" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "cc");
}

static __inline void
outsl(int port, const void *addr, int cnt)
{
  __asm __volatile("cld\n\trepne\n\toutsl"                :
                   "=S" (addr), "=c" (cnt)                :
                   "d" (port), "0" (addr), "1" (cnt)      :
                   "cc");
}

static __inline void
outl(int port, uint32_t data)
{
  __asm __volatile("outl %0,%w1" : : "a" (data), "d" (port));
}

#endif
