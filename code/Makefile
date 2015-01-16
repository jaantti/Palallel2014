srcdir = src/
objdir = obj/
libdir = lib/
src = $(wildcard $(srcdir)*.f90)
lib = $(wildcard $(libdir)*.a)
obj = $(src:$(srcdir)%.f90=$(objdir)%.obj)
flags = /Qopenmp /Qfpp
bin = main.exe
COMP = ifort

build: $(bin)

all: clean build

$(bin): $(obj)
	$(COMP) $(flags) $(obj) $(lib) -o $(bin) -L$(libdir) -assume:byterecl

$(objdir)%.obj: $(srcdir)%.f90
	$(COMP) $(flags) -c $(srcdir)$*.f90 -o $(objdir)$*.obj -assume:byterec
	
clean:
	rm -f $(obj)
	rm -f $(bin)