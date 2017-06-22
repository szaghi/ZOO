#!/usr/bin/make

#defaults
LIB = zoo

#main building variables
DOBJ    = static/obj/
DMOD    = static/mod/
DEXE    = static/
MAKELIB = ar -rcs $(DEXE)lib$(LIB).a $(DOBJ)*.o ; ranlib $(DEXE)lib$(LIB).a
DSRC    = src
FC      = gfortran
OPTSC   = -cpp -c -frealloc-lhs -O3 -J static/mod
OPTSL   = -J static/mod
VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)

#auxiliary variables
COTEXT  = "Compiling $(<F)"
LITEXT  = "Assembling $@"

#building rule
$(LIB): $(MKDIRS) $(DOBJ)$(LIB).o
	@echo $(LITEXT)
	@$(MAKELIB)

#compiling rules
$(DOBJ)zoo.o: src/zoo.f90 \
	$(DEXE)libbefor64.a    \
	$(DEXE)libface.a       \
	$(DEXE)libfiner.a      \
	$(DEXE)libfitter.a     \
	$(DEXE)libflap.a       \
	$(DEXE)libflow.a       \
	$(DEXE)libfoodie.a     \
	$(DEXE)libforbear.a    \
	$(DEXE)libforeseer.a   \
	$(DEXE)libfoxy.a       \
	$(DEXE)libpenf.a       \
	$(DEXE)libstringifor.a \
	$(DEXE)libvecfor.a     \
	$(DEXE)libvtkfortran.a \
	$(DEXE)libwenoof.a
	@echo $(COTEXT)
	@$(FC) $(OPTSC) $< -o $@

.NOTPARALLEL: $(DEXE)libbefor64.a    \
              $(DEXE)libface.a       \
              $(DEXE)libfiner.a      \
              $(DEXE)libfitter.a     \
              $(DEXE)libflap.a       \
              $(DEXE)libflow.a       \
              $(DEXE)libfoodie.a     \
              $(DEXE)libforbear.a    \
              $(DEXE)libforeseer.a   \
              $(DEXE)libfoxy.a       \
              $(DEXE)libpenf.a       \
              $(DEXE)libstringifor.a \
              $(DEXE)libvecfor.a     \
              $(DEXE)libvtkfortran.a \
              $(DEXE)libwenoof.a

$(DEXE)libbefor64.a:
	@$(MAKE) -f makefile.befor64

$(DEXE)libface.a:
	@$(MAKE) -f makefile.face

$(DEXE)libfiner.a:
	@$(MAKE) -f makefile.finer

$(DEXE)libfitter.a:
	@$(MAKE) -f makefile.fitter

$(DEXE)libflap.a:
	@$(MAKE) -f makefile.flap

$(DEXE)libflow.a:
	@$(MAKE) -f makefile.flow

$(DEXE)libfoodie.a:
	@$(MAKE) -f makefile.foodie

$(DEXE)libforbear.a:
	@$(MAKE) -f makefile.forbear

$(DEXE)libforeseer.a:
	@$(MAKE) -f makefile.foreseer

$(DEXE)libfoxy.a:
	@$(MAKE) -f makefile.foxy

$(DEXE)libpenf.a:
	@$(MAKE) -f makefile.penf

$(DEXE)libstringifor.a:
	@$(MAKE) -f makefile.stringifor

$(DEXE)libvecfor.a:
	@$(MAKE) -f makefile.vecfor

$(DEXE)libvtkfortran.a:
	@$(MAKE) -f makefile.vtkfortran

$(DEXE)libwenoof.a:
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
