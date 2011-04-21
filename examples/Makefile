
BINS=$(basename $(wildcard *.pbcc */*.pbcc */*/*.pbcc))


all: $(BINS)

%: %.pbcc ../src/pbc
	@(../src/pbc $< 2>/dev/null >&2 && echo PASSED: $*) || echo FAILED: $*

clean:
	@echo removing binaries
	@rm -f $(BINS)
	@echo removing *.o 
	@rm -f $(addsuffix .cpp, $(BINS))
	@echo removing *.cpp
	@rm -f $(addsuffix .o, $(BINS))
	@echo removing *.info 
	@rm -f $(addsuffix .info, $(BINS))

distclean: clean
	@echo removing *.cfg
	@rm -f $(addsuffix .cfg, $(BINS))

ignore:
	echo $(BINS) '*.cfg *.info *.obj' | xargs -n 1 echo > .gitignore

pbc:; (cd .. && make)
