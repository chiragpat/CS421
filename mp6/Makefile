MPVERSION=6
SRCEXT=ml
STUDENTSRC=mp$(MPVERSION).$(SRCEXT)
SKELETONSRC=mp$(MPVERSION)-skeleton.$(SRCEXT)
TESTSRC=testing.ml

MPNAME=mp$(MPVERSION)
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
MODULE_COMMON=$(MPNAME)common
LEXER=minijavalex
PARSER=minijavaparse
AST=minijavaast

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME)

DISTFILES_SOURCE=pre-rubric.c tests Makefile $(MODULE_COMMON).ml $(LEXER).mll $(PARSER).mly $(AST).ml
DISTFILES_OBJECT=$(MODULE_COMMON).cmo $(MODULE_COMMON).cmi $(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi 

IMPLEMENTATIONS= $(MODULE_COMMON).cmo $(AST).cmo $(PARSER).cmo $(LEXER).cmo $(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo 

DISTFILES_OTHER=README $(SKELETONSRC) $(TESTSRC)
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_STUDENT).mli $(MODULE_STUDENT).ml $(MODULE_RUBRIC).cm? util.o $(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(STUDENTSRC) 
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################

$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml
	$(OCAMLC) -c $(MODULE_COMMON).ml

ifeq "$(wildcard $(MODULE_SOLUTION).ml)" "$(MODULE_SOLUTION).ml"
$(MODULE_SOLUTION).cmo: $(MODULE_SOLUTION).ml $(AST).cmo $(PARSER).cmo $(LEXER).cmo
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(AST).cmo: $(AST).ml
	$(OCAMLC) -c $(AST).ml

$(PARSER).cmo: $(PARSER).mly
	$(OCAMLYACC) $(PARSER).mly
	$(OCAMLC) -c $(PARSER).mli
	$(OCAMLC) -c $(PARSER).ml

$(LEXER).cmo: $(LEXER).mll
	$(OCAMLLEX) $(LEXER).mll
	$(OCAMLC) -c $(LEXER).ml

$(MODULE_RUBRIC).cmo: $(MODULE_COMMON).cmi pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
	gcc -E pre-$(MODULE_RUBRIC).c | grep -E -v "#" > $(MODULE_RUBRIC).ml
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
	$(CP) lib/Makefile lib/*.ml $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) mp$(MPVERSION)grader* $(MODULE_STUDENT).ml*
	$(RM) -f $(MODULE_COMMON).cm?
	$(RM) -f $(LEXER).ml $(LEXER).cm? $(PARSER).ml $(PARSER).cm? $(AST).cm? $(MODULE_SOLUTION).cm?
