srcdir = src/
objdir = obj/
libdir = lib/
lapackdir = lapack/
src = $(wildcard $(srcdir)*.f90)
lib = $(wildcard $(libdir)*.a)
obj = $(src:$(srcdir)%.f90=$(objdir)%.obj)
flags = -openmp -fpp -warn all -g -traceback -gen-interfaces -warn interfaces -check
bin = main
COMP = ifort

build: $(bin)

all: clean lib build

$(bin): $(obj)
	$(COMP) $(flags) $(obj) $(lib) -o $(bin) -L$(libdir) -assume byterecl

$(objdir)%.obj: $(srcdir)%.f90
	$(COMP) $(flags) -c $(srcdir)$*.f90 -o $(objdir)$*.obj -assume byterecl
lib: FORCE
	$(COMP) $(flags) -c $(lapackdir)BLAS/SRC/*.f $(lapackdir)double/*.f $(lapackdir)lapack_routine/*.f
	ar rc geev_lib.a *.o
	mv geev_lib.a $(libdir)
	rm *.o
clean:
	rm -f $(obj)
	rm -f $(bin)
	rm -f *.o
FORCE: