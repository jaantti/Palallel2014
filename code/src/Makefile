src = main.f90
COMP = ifort

all: clean
	$(COMP) $(src) -assume:byterecl
clean:
	rm -f main.obj main.exe