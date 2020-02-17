#include <errno.h>
#include <iostream>
#include <stdio.h>
#include <fstream>

#if (!defined _WIN32 && defined _COMPAT_)
#include <strstream>
#endif

#include <stdlib.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <math.h>

#define ITSZ sizeof(int)

using namespace std;	// MH

const int lineSize=8192;
ifstream fin;
ofstream fout;

void convbin(char *inBuf, int inSize)
{
#if (!defined _WIN32 && defined _COMPAT_)
   const int wdSize=256;
   char inStr[wdSize];
   istrstream ist(inBuf, inSize);
   int it;
   while(ist >> inStr){
      it = atoi(inStr);
#else			// DD
   int it;
   for(char *p;; inBuf = p) {
      it = (int) strtol(inBuf, &p, 10);
      if (p == inBuf)
	 break;
#endif
      //cout << it  << " ";
      fout.write((char*)&it, ITSZ);
   }
   //cout << endl;
}

int main(int argc, char **argv)
{
   char inBuf[lineSize];
   int inSize;
   fin.open(argv[1]);
   if (!fin){
      perror("cannot open in file");
      exit(errno);
   }
   fout.open(argv[2], ios::binary);
   if (!fout){
      perror("cannot open out file");
      exit(errno);
   }
   
   while(fin.getline(inBuf, lineSize)){
      inSize = fin.gcount();
      //cout << "IN SIZE " << inSize << endl;
      convbin(inBuf, inSize);
   }
   fin.close();		// DD
   fout.close();	// DD
}
