#!/usr/bin/make

all:
	@$(MAKE) -f makefile.befor64
	@$(MAKE) -f makefile.face
	@$(MAKE) -f makefile.finer
	@$(MAKE) -f makefile.fitter
	@$(MAKE) -f makefile.flap
	@$(MAKE) -f makefile.flow
	@$(MAKE) -f makefile.foodie
	@$(MAKE) -f makefile.forbear
	@$(MAKE) -f makefile.foreseer
	@$(MAKE) -f makefile.foxy
	@$(MAKE) -f makefile.penf
	@$(MAKE) -f makefile.stringifor
	@$(MAKE) -f makefile.vecfor
	@$(MAKE) -f makefile.vtkfortran
	@$(MAKE) -f makefile.wenoof

#phony auxiliary rules
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ)
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : clean
clean: cleanobj cleanmod
