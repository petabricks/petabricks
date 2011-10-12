
BINS=$(basename $(wildcard *.pbcc */*.pbcc */*/*.pbcc))


all: $(BINS)

%: %.pbcc ../src/pbc
	@(../src/pbc $< 2>/dev/null >&2 && echo PASSED: $*) || echo FAILED: $*

clean:
	@echo removing binaries
	@rm -f $(BINS)
	@echo removing *.o 
	@rm -f $(addsuffix .cpp, $(BINS))
	@echo removing *.obj
	@rm -rf $(addsuffix .obj, $(BINS))
	@echo removing *.cpp
	@rm -f $(addsuffix .o, $(BINS))
	@echo removing *.info 
	@rm -f $(addsuffix .info, $(BINS))
	@echo removing *.heur
	@rm -f $(addsuffix .heur, $(BINS))
	@echo removing *.tmp
	@rm -rf $(addsuffix .tmp, $(BINS))
	@echo "removing backup (*.pbcc~) files"
	@rm -f $(addsuffix .pbcc~, $(BINS))

distclean: clean
	@echo removing *.cfg
	@rm -f $(addsuffix .cfg, $(BINS))

ignore:
	echo $(BINS) '*.cfg *.info *.obj' '-Makefile' | xargs -n 1 echo > .gitignore

pbc:; (cd .. && make)
