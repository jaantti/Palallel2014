srcdir = src/
objdir = obj/
libdir = lib/
src = $(wildcard $(srcdir)*.f90)
lib = $(wildcard $(libdir)*.a)
obj = $(src:$(srcdir)%.f90=$(objdir)%.obj)
bin = main.exe
COMP = ifort

build: $(bin)

all: clean build

$(bin): $(obj)
	$(COMP) $(obj) $(lib) -o $(bin) -L$(libdir) -assume:byterecl

$(objdir)%.obj: $(srcdir)%.f90
	$(COMP) -c $(srcdir)$*.f90 -o $(objdir)$*.obj	-assume:byterecl
	
clean:
	rm -f $(obj)
	rm -f $(bin)