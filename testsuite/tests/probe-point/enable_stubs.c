/* Enables OCaml probes in the running executable, in the same way as an
   external tool would: by reading the stapsdt notes of the executable,
   incrementing the OCaml part of each probe's semaphore and, for optimized
   probes, turning the [cmp] after the probe address into a [call]. */

#define _GNU_SOURCE
#include <elf.h>
#include <fcntl.h>
#include <link.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>

static uintptr_t load_bias;

static int find_bias(struct dl_phdr_info *info, size_t size, void *data)
{
  (void)size;
  (void)data;
  /* The first object is the executable. */
  load_bias = info->dlpi_addr;
  return 1;
}

static void patch_site(uintptr_t pc)
{
  unsigned char *site = (unsigned char *)(pc + 1);
  long page_size = sysconf(_SC_PAGESIZE);
  uintptr_t page = (uintptr_t)site & ~(uintptr_t)(page_size - 1);
  if (*site == 0xe8) return;
  if (*site != 0x3d) caml_failwith("unexpected probe site");
  if (mprotect((void *)page, page_size,
               PROT_READ | PROT_WRITE | PROT_EXEC) != 0)
    caml_failwith("mprotect failed");
  *site = 0xe8;
  if (mprotect((void *)page, page_size, PROT_READ | PROT_EXEC) != 0)
    caml_failwith("mprotect failed");
}

value test_enable_probe(value v_name)
{
  CAMLparam1(v_name);
  const char *name = String_val(v_name);
  int fd = open("/proc/self/exe", O_RDONLY);
  struct stat st;
  unsigned char *file;
  Elf64_Ehdr *ehdr;
  Elf64_Shdr *shdrs;
  const char *shstrtab;
  int found = 0;

  if (fd < 0 || fstat(fd, &st) != 0) caml_failwith("cannot open executable");
  file = mmap(NULL, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  if (file == MAP_FAILED) caml_failwith("cannot map executable");
  dl_iterate_phdr(find_bias, NULL);

  ehdr = (Elf64_Ehdr *)file;
  shdrs = (Elf64_Shdr *)(file + ehdr->e_shoff);
  shstrtab = (const char *)(file + shdrs[ehdr->e_shstrndx].sh_offset);
  for (int i = 0; i < ehdr->e_shnum; i++) {
    unsigned char *p, *end;
    if (strcmp(shstrtab + shdrs[i].sh_name, ".note.stapsdt") != 0) continue;
    p = file + shdrs[i].sh_offset;
    end = p + shdrs[i].sh_size;
    while (p < end) {
      Elf64_Nhdr *nhdr = (Elf64_Nhdr *)p;
      unsigned char *owner = p + sizeof(Elf64_Nhdr);
      unsigned char *desc = owner + ((nhdr->n_namesz + 3) & ~3);
      p = desc + ((nhdr->n_descsz + 3) & ~3);
      if (nhdr->n_type != 3 || strcmp((char *)owner, "stapsdt") != 0)
        continue;
      {
        uint64_t pc, semaphore;
        const char *provider = (const char *)desc + 24;
        const char *probe = provider + strlen(provider) + 1;
        memcpy(&pc, desc, 8);
        memcpy(&semaphore, desc + 16, 8);
        if (strcmp(provider, "ocaml_2") != 0 || strcmp(probe, name) != 0)
          continue;
        if (!found && semaphore != 0) {
          unsigned short *sem =
            (unsigned short *)(semaphore + load_bias + 2);
          (*sem)++;
        }
        found = 1;
        /* Dummy notes, for semaphores without optimized probes, have a zero
           pc. */
        if (pc != 0) patch_site(pc + load_bias);
      }
    }
  }
  munmap(file, st.st_size);
  if (!found) caml_failwith("probe not found");
  CAMLreturn(Val_unit);
}
