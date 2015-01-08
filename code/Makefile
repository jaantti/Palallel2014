srcdir = src/
objdir = obj/
src = $(wildcard $(srcdir)*.f90)
obj = $(src:$(srcdir)%.f90=$(objdir)%.obj)
bin = main.exe
COMP = ifort

build: $(bin)

all: clean build

$(bin): $(obj)
	$(COMP) $(obj) -o $(bin) -assume:byterecl

$(objdir)%.obj: $(srcdir)%.f90
	$(COMP) -c $(srcdir)$*.f90 -o $(objdir)$*.obj	-assume:byterecl
	
clean:
	rm -f $(obj)
	rm -f $(bin)