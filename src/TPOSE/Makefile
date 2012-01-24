# see Makeconf for compiler settings
HEADERS = calcdb.h Array.h
OBJS 	= calcdb.o Array.o $(OBJS_WIN32)
LIBS	= -lm
TARGET  = exttpose getconf makebin

default:	$(TARGET) 

clean:
	rm -rf *~ *.o $(TARGET)

exttpose: exttpose.cc $(OBJS) $(HEADERS)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o exttpose exttpose.cc $(OBJS) $(LIBS)

Array.o: Array.cc Array.h
	$(CXX) $(CXXFLAGS) -c -o Array.o Array.cc

getconf: getconf.cc calcdb.o calcdb.h 
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o getconf getconf.cc calcdb.o $(LIBS)

calcdb.o: calcdb.cc calcdb.h
	$(CXX) $(CXXFLAGS) -c -o calcdb.o calcdb.cc

makebin: makebin.cc
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o makebin makebin.cc

b2a: b2a.cc $(OBJS_WIN32)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o b2a b2a.cc $(OBJS_WIN32)

install-bin: $(TARGET)
	mkdir -p ../../$(BINDIR)
	cp $(TARGET) ../../$(BINDIR)

