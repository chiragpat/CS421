MPVERSION=2
STUDENTSRC=mp$(MPVERSION).ml

OCAMLC=ocamlc
OCAMLYACC=ocamlyacc
OCAMLLEX=ocamllex
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
MODULE_COMMON=mp$(MPVERSION)common
LIBRARY_FILES=str.cma
LANG_NAME=minijava

#######################################################################
# DISTFILES define what goes into mpNtest.tgz distributions
#######################################################################

all: $(GRADER_NAME) 

DISTFILES_SOURCE=pre-rubric.c tests Makefile mp$(MPVERSION)-skeleton.ml testing.ml
LEXER_OBJECT=$(LANG_NAME)lex.cmi $(LANG_NAME)lex.cmo
PARSER_OBJECT=$(LANG_NAME)parse.cmi $(LANG_NAME)parse.cmo
TEMP_DIST_OBJECT=$(MODULE_SOLUTION).cmo $(MODULE_SOLUTION).cmi $(LEXER_OBJECT) $(PARSER_OBJECT)
DISTFILES_OBJECT=$(TEMP_DIST_OBJECT)

TEMP_IMPL=$(MODULE_STUDENT).cmo $(MODULE_SOLUTION).cmo $(LANG_NAME)parse.cmo $(LANG_NAME)lex.cmo
IMPLEMENTATIONS=$(TEMP_IMPL)

########################################################################
# if mpXcommon.ml exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).ml)" "$(MODULE_COMMON).ml" 
DISTFILES_OBJECT=$(TEMP_DIST_OBJECT) $(MODULE_COMMON).cmo $(MODULE_COMMON).cmi
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(TEMP_IMPL) 
$(MODULE_COMMON).cmo: $(MODULE_COMMON).ml 
	$(OCAMLC) -c $(MODULE_COMMON).ml
endif

########################################################################
# if mpXcommon.cmo exists, add it
########################################################################
ifeq "$(wildcard $(MODULE_COMMON).cmo)" "$(MODULE_COMMON).cmo"
IMPLEMENTATIONS=$(MODULE_COMMON).cmo $(TEMP_IMPL) 
endif

########################################################################
# if minijavaparse.mly exists, compile it.  otherwise assume minijavaparse.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(LANG_NAME)parse.mly)" "$(LANG_NAME)parse.mly"
$(LANG_NAME)parse.cmo: $(LANG_NAME)parse.mly
	$(OCAMLYACC) $(LANG_NAME)parse.mly
	$(OCAMLC) -c $(LANG_NAME)parse.mli
	$(OCAMLC) -c $(LANG_NAME)parse.ml

$(LANG_NAME)lex.cmo: $(LANG_NAME)lex.mll
	$(OCAMLLEX) $(LANG_NAME)lex.mll
	$(OCAMLC) -c $(LANG_NAME)lex.ml
endif

DISTFILES_OTHER=README
DISTFILES=$(DISTFILES_SOURCE) $(DISTFILES_OBJECT) $(DISTFILES_OTHER)

OBJECTS=$(IMPLEMENTATIONS) $(MODULE_RUBRIC).cmo

#STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? util.o $(GRADER_NAME)
STUDENT_CLEAN=$(MODULE_STUDENT).cm? $(MODULE_RUBRIC).cm? $(GRADER_NAME)

$(GRADER_NAME): $(LIBRARY_GRADER) $(OBJECTS)
	$(OCAMLC) -o $(GRADER_NAME) $(LIBRARY_FILES) $(LIBRARY_GRADER) $(OBJECTS) 

$(LIBRARY_GRADER):
	$(GMAKE) -C lib
#	$(LN) -s lib/util.o .

$(MODULE_STUDENT).cmo: $(STUDENTSRC)
	$(CP) $(STUDENTSRC) $(MODULE_STUDENT).ml
	$(OCAMLC) -c $(MODULE_STUDENT).ml

########################################################################
# if solution.ml exists, compile it.  otherwise assume solution.cm{o,i}
# exist.
########################################################################
ifeq "$(wildcard $(MODULE_SOLUTION).ml)" "$(MODULE_SOLUTION).ml"
$(MODULE_SOLUTION).cmo: $(MODULE_SOLUTION).ml  $(LANG_NAME)parse.cmo
	$(OCAMLC) -c $(MODULE_SOLUTION).ml
endif

$(MODULE_RUBRIC).cmo: pre-$(MODULE_RUBRIC).c tests $(IMPLEMENTATIONS) $(LIBRARY_GRADER)
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
#	$(CP) lib/Makefile lib/*.ml lib/*.c $(TESTNAME)/lib
	$(CP) lib/Makefile lib/*.ml $(TESTNAME)/lib
	$(CP) $(DISTFILES) $(TESTNAME)
	$(TAR) cpf $(TESTNAME).tar $(TESTNAME)
	$(RM) -rf $(TESTNAME)
	$(GZIP) -9 $(TESTNAME).tar

#if you are a student, do not make dist-clean.  it will delete
#your copy of solution.cmo and you will need to download a new
#copy.
dist-clean: clean
	$(RM) -f $(DISTFILES_OBJECT) mp$(MPVERSION)grader* $(MODULE_STUDENT).ml $(MODULE_COMMON).cm? $(LANG_NAME)lex.ml $(LANG_NAME)parse.ml $(LANG_NAME)parse.mli
