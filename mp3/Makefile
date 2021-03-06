MPVERSION=3
STUDENTSRC=mp3

OCAMLC=ocamlc
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc
GMAKE=make
RM=rm
CP=cp
LN=ln
MV=mv
TAR=tar
GZIP=gzip
MKDIR=mkdir

GRADER_NAME=grader

LIBRARY_GRADER=lib/grader.cma
MODULE_STUDENT=student
MODULE_SOLUTION=solution
MODULE_RUBRIC=rubric
MODULE_COMMON=mp3common

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME) 

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(STUDENTSRC)-skeleton.ml
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi

IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo


########################################################################
# if mpXcommon.mli exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).mli)" "$(MODULE_COMMON).mli" 
DISTFILES_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(MODULE_COMMON).cmi
IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo 
$(MODULE_COMMON).cmi: $(MODULE_COMMON).mli 
	$(OCAMLC) -c $(MODULE_COMMON).mli
endif

########################################################################
# if mpXcommon.cmo exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).cmi)" "$(MODULE_COMMON).cmi"
IMPLEMENTATIONS=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo
endif

DISTFILES_OTHER=README
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? util.o \
		$(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(MODULE_COMMON).cmi $(STUDENTSRC).ml 
	$(CP) $(STUDENTSRC).ml $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).ml)" "$(MODULE_SOLUTION).ml"
$(MODULE_SOLUTION).cmo: $(MODULE_COMMON).cmi $(MODULE_SOLUTION).ml
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: $(MODULE_COMMON).cmi pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "# 1 \"<built-in>\"" | grep -E -v "# 1 \"<command line>\"" \
                                      | grep -E -v "\"pre-rubric.c\"" | grep -E -v "# 1 \"tests\"" > $(MODULE_RUBRIC).ml
	$(OCAMLC) -c -I lib $(MODULE_RUBRIC).ml
	$(RM) -f $(MODULE_RUBRIC).ml

clean:
	$(GMAKE) -C lib clean
	$(RM) -f $(STUDENT_CLEAN)

##########################################################################
#these targets are used by staff
##########################################################################

TESTNAME=mp$(MPVERSION)grader

dist: $(DISTFILES)
	$(RM) -rf $(TESTNAME)
	$(MKDIR) $(TESTNAME)
	$(MKDIR) $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml lib/*.c lib/*.h $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) mp$(MPVERSION)grader* $(MODULE_STUDENT).ml $(MODULE_COMMON).cm?
