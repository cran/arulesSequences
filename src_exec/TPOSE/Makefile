#CXX = g++
#CXXFLAGS  = -O3 -DSGI -no_auto_include
CXXFLAGS = -O3 -Wno-deprecated
HEADERS = calcdb.h Array.h
OBJS	= calcdb.o Array.o
LIBS = -lm -lc
TARGET  = exttpose getconf makebin

default:	$(TARGET) 

clean:
	rm -rf *~ *.o $(TARGET)

tpose: tpose.cc $(OBJS)
	$(CXX) $(CFLAGS) -o tpose tpose.cc $(OBJS) $(LIBS)

exttpose: exttpose.cc $(OBJS) $(HEADERS)
	$(CXX) $(CXXFLAGS) -o exttpose exttpose.cc $(OBJS) $(LIBS)

Database.o: Database.cc Database.h
	$(CXX) $(CXXFLAGS) -c -o Database.o Database.cc

Array.o: Array.cc Array.h
	$(CXX) $(CXXFLAGS) -c -o Array.o Array.cc

getconf: calcdb.o getconf.cc calcdb.h 
	$(CXX) $(CXXFLAGS) -o getconf calcdb.o getconf.cc $(LIBS)

calcdb.o: calcdb.cc calcdb.h
	$(CXX) $(CXXFLAGS) -c -o calcdb.o calcdb.cc

makebin: makebin.cc
	$(CXX) $(CXXFLAGS) -o makebin makebin.cc
