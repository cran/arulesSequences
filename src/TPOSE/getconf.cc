#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <string.h>
#include <math.h>

#include "calcdb.h"

#ifndef _WIN32
#define O_BINARY 0
#endif

using namespace std;

#define ITSZ sizeof(int)

char input[300];       //input file name
char confn[300];
int use_seq = 1;

void parse_args(int argc, char **argv)
{
   extern char * optarg;
   int c;
   
   if (argc < 2) {
      cout << "usage: getconf [-a] -i<infile> -o<outfile>\n";
      exit(EXIT_FAILURE);
   }else{
      while ((c=getopt(argc,argv,"ai:o:"))!=-1){
         switch(c){
         case 'a': //work on assoc
            use_seq = 0;
	    printf("USE SEQ = 0\n");
            break;            
         case 'i':
            snprintf(input, sizeof(input), "%s.data",optarg);
            break;
         case 'o':
            snprintf(confn, sizeof(confn), "%s.conf", optarg);
            break;
         }
      }
   }
}


int main (int argc, char **argv)
{
   parse_args(argc, argv);

   int DBASE_NUM_TRANS=0;
   int DBASE_MAXITEM=0;
   int DBASE_NUM_CUST=0;
   int DBASE_MINTRANS=0;
   int DBASE_MAXTRANS=0;   
   float DBASE_AVG_TRANS_SZ=0;
   float DBASE_AVG_CUST_SZ=0;
   
   int i;

   int custid = 0, tid, nitem = 0;	// DD
   int *buf = NULL;			// DD
   int oldcustid=-1;
   int oldtcnt = 0;
   int tsizesum = 0;
   int tcustsum = 0;
   int tsizesq = 0;
   int maxnitem = 0;

   Dbase_Ctrl_Blk *DCB = new Dbase_Ctrl_Blk(input);
   DCB->get_first_blk();
   DCB->get_next_trans(buf, nitem, tid, custid);
   DBASE_MINTRANS = custid;  
   while (!DCB->eof()){
      //printf ("%d %d %d\n", custid, tid, nitem);
      DBASE_MAXTRANS = custid;  
      if (use_seq){
         if (oldcustid != custid){
            tcustsum += DBASE_NUM_TRANS - oldtcnt;
            oldtcnt = DBASE_NUM_TRANS;
            DBASE_NUM_CUST++;
            oldcustid = custid;
         }
      }
      DBASE_NUM_TRANS++;
      tsizesum += nitem;
      if (nitem > maxnitem) maxnitem = nitem;
      
      tsizesq += (nitem*nitem);
      for (i=0; i < nitem; i++)
         if (buf[i] > DBASE_MAXITEM) DBASE_MAXITEM = buf[i];
      DCB->get_next_trans(buf, nitem, tid, custid);
   }
   tcustsum += DBASE_NUM_TRANS - oldtcnt;
   DBASE_MAXITEM++;

   if (use_seq) DBASE_AVG_CUST_SZ = (1.0*tcustsum)/DBASE_NUM_CUST;
   DBASE_AVG_TRANS_SZ = (1.0*tsizesum)/DBASE_NUM_TRANS;
   double trans_sq_avg = (1.0*tsizesq)/DBASE_NUM_TRANS;
   double stddev = sqrt(trans_sq_avg - 
                        (DBASE_AVG_TRANS_SZ*DBASE_AVG_TRANS_SZ));
   

   //write config info to new file
   int conffd;
   if ((conffd = open(confn, (O_WRONLY|O_CREAT|O_BINARY), 0666)) < 0){
      perror("Can't open out file");
      exit (errno);      
   }
   if (use_seq){
      size_t FTSZ = sizeof(float);
      if (write(conffd,(char *)&DBASE_NUM_CUST,ITSZ) < ITSZ){
	 perror("writing (1)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MAXITEM,ITSZ) < ITSZ){
	 perror("writing (2)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_AVG_CUST_SZ,FTSZ) < FTSZ){
	 perror("writing (3)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_AVG_TRANS_SZ,FTSZ) < FTSZ){
	 perror("writing (4)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_NUM_TRANS,ITSZ) < ITSZ){
	 perror("writing (5)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MINTRANS,ITSZ) < ITSZ){
	 perror("writing (6)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MAXTRANS,ITSZ) < ITSZ){
	 perror("writing (7)");
	 exit(errno);
      }
   }
   else{
      size_t FTSZ = sizeof(float);
      if (write(conffd,(char *)&DBASE_NUM_TRANS,ITSZ) < ITSZ){
	 perror("writing (8)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MAXITEM,ITSZ) < ITSZ){
	 perror("writing (9)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_AVG_TRANS_SZ,FTSZ) < FTSZ){
	 perror("writing (10)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MINTRANS,ITSZ) < ITSZ){
	 perror("writing (11)");
	 exit(errno);
      }
      if (write(conffd,(char *)&DBASE_MAXTRANS,ITSZ) < ITSZ){
	 perror("writing (12)");
	 exit(errno);
      }
   }
   
   close(conffd);
   printf("CONF %d %d %f %f %d %d %d %f %d\n", DBASE_NUM_CUST, DBASE_MAXITEM,
          DBASE_AVG_CUST_SZ, DBASE_AVG_TRANS_SZ, DBASE_NUM_TRANS,
          DBASE_MINTRANS, DBASE_MAXTRANS, stddev, maxnitem);
   delete DCB;
   exit(0);
}

// remark: the implementation assumes that a customer's transactions
//         appear as a contiguous block in the binary input data, and
//         therefore, in the user-supplied database.
//
// ceeboo 2007
