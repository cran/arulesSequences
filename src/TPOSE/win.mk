# see Makefile
OBJS_WIN32 = mmap-win32.o

mmap-win32.o: mmap-win32.c mmap-win32.h
	$(CC) $(CFLAGS) -c -o mmap-win32.o mmap-win32.c

