CC  = g++
#CC = CC -DSGI -no_auto_include
#CFLAGS  = -g -O3 -Wno-deprecated
CFLAGS  = -O3 -Wno-deprecated
HEADER  = Array.h Itemset.h Lists.h Eqclass.h extl2.h partition.h\
	maxgap.h spade.h
OBJS	= Itemset.o Array.o Eqclass.o Lists.o extl2.o partition.o\
	maxgap.o
LIBS = -lm -lc
TARGET  = spade

default:	$(TARGET)

.SUFFIXES: .o .cc

clean:
	rm -rf *~ *.o $(TARGET)

spade: sequence.cc $(OBJS) $(HEADER) 
	$(CC) $(CFLAGS) -o spade sequence.cc $(OBJS) $(LIBS)


.cc.o:
	$(CC) $(CFLAGS) -c $<

# dependencies
# $(OBJS): $(HFILES)
Array.o: Array.h
Lists.o: Lists.h
extl2.o: extl2.h
Itemset.o: Itemset.h
Eqclass.o: Eqclass.h
partition.o: partition.h
maxgap.o: maxgap.h
