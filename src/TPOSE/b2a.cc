#include <iostream>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>

#ifndef _WIN32
#include <sys/mman.h>
#define O_BINARY 0
#else
#include "mmap-win32.h"
#endif

using namespace std;

int main(int argc, char **argv)
{
   int fd;
   if ((fd = open(argv[1], O_RDONLY|O_BINARY)) < 0){
      perror("cant openfile ");
      exit(errno);
   }
   long flen = lseek(fd, 0, SEEK_END);
   int *ary;
#ifndef DEC
   ary = (int *) mmap((char *)NULL, flen,
                          (PROT_WRITE|PROT_READ),
                          MAP_PRIVATE, fd, 0);
#else
   ary = (int *) mmap((char *)NULL, flen,
                          (PROT_WRITE|PROT_READ),
                          (MAP_FILE|MAP_VARIABLE|MAP_PRIVATE), fd, 0);
#endif
   if (ary == (int *)-1){
      perror("MMAP ERROR");
      exit(errno);
   }
   for (int i=0; i < (int) (flen/sizeof(int)); i++)	// CB
      cout << " " << ary[i];
   cout << endl;

   munmap((caddr_t)ary, flen);
   close(fd);
   
}
