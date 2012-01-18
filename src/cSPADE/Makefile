# see Makeconf for compiler settings
ifdef WIN
OBJS_WIN32 = ../TPOSE/mmap-win32.o
endif
HEADER  = Array.h Itemset.h Lists.h Eqclass.h extl2.h partition.h\
	maxgap.h spade.h
OBJS	= Itemset.o Array.o Eqclass.o Lists.o extl2.o partition.o\
	maxgap.o $(OBJS_WIN32)
LIBS 	= -lm
TARGET  = spade

default:	$(TARGET)

.SUFFIXES: .o .cc

clean:
	rm -rf *~ *.o $(TARGET)

spade: sequence.cc $(OBJS) $(HEADER) 
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o spade sequence.cc $(OBJS) $(LIBS)


.cc.o:
	$(CXX) $(CXXFLAGS) -c $<


# dependencies
# $(OBJS): $(HFILES)
Array.o: Array.h
Lists.o: Lists.h
extl2.o: extl2.h
Itemset.o: Itemset.h
Eqclass.o: Eqclass.h
partition.o: partition.h
maxgap.o: maxgap.h

install-bin: $(TARGET)
	cp $(TARGET) ../../$(BINDIR)

