LIBDIR=../lib
CXXFLAGS= -Ofast -I. 
LDFLAGS= -fPIE

target=$(LIBDIR)/libmodel_sm_full.a

all: $(target)

objects=HelAmps_sm_full.o Parameters_sm_full.o rambo.o read_slha.o

$(target): $(objects)
	$(AR) cru $@ $(objects)
	ranlib $(target)


.PHONY: clean

clean:
	rm -f $(target)
	rm -f $(objects)

