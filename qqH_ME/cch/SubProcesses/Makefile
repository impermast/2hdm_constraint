LIBDIR=../../lib
INCDIR=../../src
MODELLIB=model_sm_full
CXXFLAGS= -Ofast -I$(INCDIR) -I.
LIBFLAGS= -L$(LIBDIR) -l$(MODELLIB)
LDFLAGS= -no-pie

main=check

all: $(main)

objects=CPPProcess.o check_sa.o

$(main): $(objects) $(LIBDIR)/lib$(MODELLIB).a
	$(CXX) -o $@ $(objects) $(LIBFLAGS) $(LDFLAGS)

$(LIBDIR)/lib$(MODELLIB).a: CPPProcess.o check_sa.o
	make -C $(INCDIR)

.PHONY: clean

clean:
	rm -f $(main) $(objects)

