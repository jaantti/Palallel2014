srcdir = src/
objdir = obj/
libdir = lib/
lapackdir = lapack/
src = $(wildcard $(srcdir)*.f90)
lib = $(wildcard $(libdir)*.a)
obj = $(src:$(srcdir)%.f90=$(objdir)%.obj)
flags = -openmp -fpp
bin = main.exe
COMP = ifort

build: $(bin)

all: clean lib build

$(bin): $(obj)
	$(COMP) $(flags) $(obj) $(lib) -o $(bin) -L$(libdir) -assume:byterecl

$(objdir)%.obj: $(srcdir)%.f90
	$(COMP) $(flags) -c $(srcdir)$*.f90 -o $(objdir)$*.obj -assume:byterec
	
lib: 
	$(COMP) $(flags) -c $(lapackdir)BLAS/SRC/*.f $(lapackdir)double/*.f $(lapackdir)lapack_routine/*.f -o $(lapackdir)
	ar rc geev_lib.a $(lapackdir)*.obj
	mv geev_lib.a $(libdir)
clean:
	rm -f $(obj)
	rm -f $(bin)
	rm -f $(lapackdir)*.obj
	