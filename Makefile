# FuzzyCLIPS 6.42a -- Console and Static Library
#
# Portable GNU Make build file.
#
# Build configuration (compiler, flags, install paths) is produced by the
# ./configure script and written to config.mk, which is included below.
# Run ./configure once before building.  Typical workflow:
#
#     ./configure && make && make test && make install
#
# See ./configure --help for the available configuration options.

CONFIG := config.mk

# Every target except the cleanup ones requires a generated config.mk.
ifeq ($(wildcard $(CONFIG)),)
  ifeq ($(filter clean distclean,$(MAKECMDGOALS)),)
    $(error No $(CONFIG) found. Run ./configure first -- see ./configure --help)
  endif
else
  include $(CONFIG)
endif

# Fallback so the cleanup targets work even before ./configure has been run.
AR ?= ar

# Install helpers (override on the command line if your platform differs).
INSTALL         ?= install
INSTALL_PROGRAM ?= $(INSTALL) -m 755
INSTALL_DATA    ?= $(INSTALL) -m 644

BIN = fuzzyclips
LIB = libclips.a

# All C sources and headers live in src/.  VPATH lets make locate them while
# the object files are produced here in the top-level directory.
SRCDIR = src
VPATH  = $(SRCDIR)

OBJS = $(addprefix $(SRCDIR)/, \
	agenda.o analysis.o argacces.o bload.o bmathfun.o bsave.o \
 	classcom.o classexm.o classfun.o classinf.o classini.o \
 	classpsr.o clsltpsr.o commline.o conscomp.o constrct.o \
 	constrnt.o crstrtgy.o cstrcbin.o cstrccom.o cstrcpsr.o \
 	cstrnbin.o cstrnchk.o cstrncmp.o cstrnops.o cstrnpsr.o \
 	cstrnutl.o default.o defins.o developr.o dffctbin.o dffctbsc.o \
 	dffctcmp.o dffctdef.o dffctpsr.o dffnxbin.o dffnxcmp.o dffnxexe.o \
 	dffnxfun.o dffnxpsr.o dfinsbin.o dfinscmp.o drive.o emathfun.o \
 	engine.o envrnmnt.o envrnbld.o evaluatn.o expressn.o exprnbin.o \
 	exprnops.o exprnpsr.o extnfunc.o factbin.o factbld.o factcmp.o \
 	factcom.o factfun.o factgen.o facthsh.o factfile.o factlhs.o factmch.o \
 	factmngr.o factprt.o factqpsr.o factqury.o factrete.o factrhs.o \
 	filecom.o filertr.o fileutil.o generate.o genrcbin.o genrccmp.o \
 	genrccom.o genrcexe.o genrcfun.o genrcpsr.o globlbin.o globlbsc.o \
 	globlcmp.o globlcom.o globldef.o globlpsr.o immthpsr.o incrrset.o \
 	inherpsr.o inscom.o insfile.o insfun.o insmngr.o insmoddp.o \
 	insmult.o inspsr.o insquery.o insqypsr.o iofun.o lgcldpnd.o \
 	memalloc.o miscfun.o modulbin.o modulbsc.o modulcmp.o moduldef.o \
 	modulpsr.o modulutl.o msgcom.o msgfun.o msgpass.o msgpsr.o \
 	multifld.o multifun.o objbin.o objcmp.o objrtbin.o objrtbld.o \
 	objrtcmp.o objrtfnx.o objrtgen.o objrtmch.o parsefun.o pattern.o \
 	pprint.o prccode.o prcdrfun.o prcdrpsr.o prdctfun.o prntutil.o \
 	proflfun.o reorder.o reteutil.o retract.o router.o rulebin.o \
 	rulebld.o rulebsc.o rulecmp.o rulecom.o rulecstr.o ruledef.o \
 	ruledlt.o rulelhs.o rulepsr.o scanner.o sortfun.o strngfun.o \
 	strngrtr.o symblbin.o symblcmp.o symbol.o sysdep.o textpro.o \
 	tmpltbin.o tmpltbsc.o tmpltcmp.o tmpltdef.o tmpltfun.o tmpltlhs.o \
 	tmpltpsr.o tmpltrhs.o tmpltutl.o userdata.o userfunctions.o \
 	utility.o watch.o \
        cfdef.o fuzzycom.o fuzzydef.o fuzzylhs.o fuzzymod.o fuzzypsr.o \
        fuzzyrhs.o fuzzyutl.o)

.PHONY: all release debug test check install uninstall clean distclean help

all: $(BIN)

# 'release' and 'debug' are kept as aliases; the actual build type is chosen
# by ./configure (use ./configure --enable-debug for an unoptimized build).
release: $(BIN)
debug: $(BIN)

$(SRCDIR)/%.o : $(SRCDIR)/%.c
	$(CC) -c -D$(CLIPS_OS) -I$(SRCDIR) $(CPPFLAGS) $(CFLAGS) $(WARNINGS) $< -o $@

$(BIN): $(SRCDIR)/main.o $(LIB)
	$(CC) $(LDFLAGS) -o $(BIN) $(SRCDIR)/main.o -L. -lclips $(LDLIBS)

$(LIB): $(OBJS)
	rm -f $(LIB)
	$(AR) cq $(LIB) $(OBJS)

# Build (if necessary) and run the unit-test suite.
test check: $(BIN)
	@bash tests/run_all_tests.sh

# Install the binary, static library and public headers under the configured
# prefix.  DESTDIR is honoured for staged/packaged installs.
install: $(BIN) $(LIB)
	$(INSTALL) -d "$(DESTDIR)$(bindir)"
	$(INSTALL_PROGRAM) $(BIN) "$(DESTDIR)$(bindir)/$(BIN)"
	$(INSTALL) -d "$(DESTDIR)$(libdir)"
	$(INSTALL_DATA) $(LIB) "$(DESTDIR)$(libdir)/$(LIB)"
	$(INSTALL) -d "$(DESTDIR)$(includedir)"
	$(INSTALL_DATA) $(SRCDIR)/*.h "$(DESTDIR)$(includedir)/"
	@echo "Installed $(BIN) -> $(DESTDIR)$(bindir)/$(BIN)"

uninstall:
	-rm -f "$(DESTDIR)$(bindir)/$(BIN)"
	-rm -f "$(DESTDIR)$(libdir)/$(LIB)"
	-rm -rf "$(DESTDIR)$(includedir)"

clean:
	-rm -f $(SRCDIR)/main.o $(OBJS)
	-rm -f $(BIN) $(LIB)

# Also remove the generated configuration.
distclean: clean
	-rm -f $(CONFIG)

help:
	@echo "FuzzyCLIPS build -- run ./configure first, then:"
	@echo "  make            build $(BIN) and $(LIB)"
	@echo "  make test       build and run the unit-test suite"
	@echo "  make install    install under the configured prefix"
	@echo "  make clean      remove build products"
	@echo "  make distclean  also remove config.mk"

# Dependencies generated using "gcc -MM *.c"

$(SRCDIR)/agenda.o: agenda.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h engine.h lgcldpnd.h retract.h \
  memalloc.h modulutl.h scanner.h prntutil.h reteutil.h rulecom.h \
  router.h rulebsc.h strngrtr.h sysdep.h watch.h

$(SRCDIR)/analysis.o: analysis.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h cstrnchk.h constrnt.h evaluatn.h cstrnutl.h cstrnops.h \
  exprnpsr.h extnfunc.h expressn.h exprnops.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h multifld.h symbol.h match.h \
  network.h ruledef.h agenda.h crstrtgy.h conscomp.h symblcmp.h \
  cstrccom.h objrtmch.h scanner.h generate.h analysis.h reorder.h \
  pattern.h memalloc.h modulutl.h prntutil.h router.h rulecstr.h \
  rulepsr.h watch.h

$(SRCDIR)/argacces.o: argacces.c setup.h envrnmnt.h entities.h usrsetup.h \
  cstrnchk.h constrnt.h evaluatn.h constant.h extnfunc.h expressn.h \
  exprnops.h constrct.h userdata.h moduldef.h utility.h insfun.h \
  object.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h inscom.h prntutil.h router.h sysdep.h \
  argacces.h

$(SRCDIR)/bload.o: bload.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h bsave.h cstrnbin.h exprnpsr.h \
  scanner.h memalloc.h prntutil.h router.h bload.h exprnbin.h sysdep.h \
  symblbin.h

$(SRCDIR)/bmathfun.o: bmathfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h prntutil.h router.h bmathfun.h

$(SRCDIR)/bsave.o: bsave.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h bload.h exprnbin.h sysdep.h \
  symblbin.h cstrnbin.h exprnpsr.h scanner.h memalloc.h prntutil.h \
  router.h bsave.h

$(SRCDIR)/classcom.o: classcom.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h argacces.h classfun.h scanner.h classcom.h classini.h \
  modulutl.h msgcom.h msgpass.h prntutil.h router.h

$(SRCDIR)/classexm.o: classexm.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h classcom.h \
  classfun.h scanner.h classini.h memalloc.h msgcom.h msgpass.h msgfun.h \
  prntutil.h router.h strngrtr.h sysdep.h classexm.h

$(SRCDIR)/classfun.o: classfun.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h classcom.h classini.h cstrcpsr.h strngfun.h inscom.h \
  insmngr.h memalloc.h modulutl.h scanner.h msgfun.h msgpass.h \
  prntutil.h router.h classfun.h

$(SRCDIR)/classinf.o: classinf.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h classcom.h \
  classexm.h classfun.h scanner.h classini.h memalloc.h msgcom.h \
  msgpass.h msgfun.h prntutil.h classinf.h

$(SRCDIR)/classini.o: classini.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classexm.h \
  classfun.h scanner.h classinf.h classpsr.h cstrcpsr.h strngfun.h \
  inscom.h memalloc.h modulpsr.h modulutl.h msgcom.h msgpass.h watch.h \
  defins.h insquery.h bload.h exprnbin.h sysdep.h symblbin.h objbin.h \
  objcmp.h objrtbld.h objrtfnx.h classini.h

$(SRCDIR)/classpsr.o: classpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h classcom.h classfun.h scanner.h clsltpsr.h cstrcpsr.h \
  strngfun.h inherpsr.h memalloc.h modulpsr.h modulutl.h msgpsr.h \
  pprint.h prntutil.h router.h classpsr.h

$(SRCDIR)/clsltpsr.o: clsltpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h cstrnchk.h cstrnpsr.h cstrnutl.h default.h memalloc.h \
  pprint.h prntutil.h router.h clsltpsr.h

$(SRCDIR)/commline.o: commline.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h argacces.h expressn.h exprnops.h constrct.h userdata.h \
  moduldef.h utility.h evaluatn.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h exprnpsr.h scanner.h fileutil.h memalloc.h pprint.h \
  prcdrfun.h prcdrpsr.h prntutil.h router.h strngrtr.h sysdep.h \
  commline.h

$(SRCDIR)/conscomp.o: conscomp.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrncmp.h \
  exprnpsr.h scanner.h memalloc.h modulcmp.h prntutil.h router.h \
  sysdep.h dffnxcmp.h dffnxfun.h tmpltcmp.h tmpltdef.h factbld.h \
  globlcmp.h globldef.h genrccmp.h genrcfun.h objcmp.h

$(SRCDIR)/constrct.o: constrct.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h commline.h \
  cstrcpsr.h strngfun.h exprnpsr.h scanner.h memalloc.h miscfun.h \
  modulutl.h prcdrfun.h prcdrpsr.h prntutil.h router.h sysdep.h watch.h

$(SRCDIR)/constrnt.o: constrnt.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h memalloc.h \
  router.h scanner.h

$(SRCDIR)/crstrtgy.o: crstrtgy.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h argacces.h memalloc.h pattern.h \
  scanner.h reorder.h reteutil.h rulecom.h

$(SRCDIR)/cstrcbin.o: cstrcbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrcbin.h

$(SRCDIR)/cstrccom.o: cstrccom.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h extnfunc.h evaluatn.h expressn.h exprnops.h constrct.h \
  userdata.h moduldef.h utility.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h symblcmp.h cstrccom.h objrtmch.h memalloc.h argacces.h \
  modulutl.h scanner.h prntutil.h router.h commline.h sysdep.h bload.h \
  exprnbin.h symblbin.h cstrcpsr.h strngfun.h

$(SRCDIR)/cstrcpsr.o: cstrcpsr.c setup.h envrnmnt.h entities.h usrsetup.h router.h \
  watch.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h prcdrpsr.h \
  exprnpsr.h scanner.h memalloc.h modulutl.h modulpsr.h pprint.h \
  prntutil.h strngrtr.h sysdep.h cstrcpsr.h strngfun.h

$(SRCDIR)/cstrnbin.o: cstrnbin.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h memalloc.h prntutil.h router.h bload.h utility.h evaluatn.h \
  moduldef.h userdata.h insfun.h object.h constrct.h constrnt.h \
  expressn.h exprnops.h multifld.h symbol.h match.h network.h ruledef.h \
  agenda.h crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h \
  objrtmch.h exprnbin.h sysdep.h symblbin.h bsave.h cstrnbin.h

$(SRCDIR)/cstrnchk.o: cstrnchk.c setup.h envrnmnt.h entities.h usrsetup.h \
  cstrnutl.h constrnt.h evaluatn.h constant.h extnfunc.h expressn.h \
  exprnops.h constrct.h userdata.h moduldef.h utility.h insfun.h \
  object.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h prntutil.h \
  router.h classcom.h classexm.h inscom.h cstrnchk.h

$(SRCDIR)/cstrncmp.o: cstrncmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h conscomp.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h memalloc.h prntutil.h \
  router.h sysdep.h cstrncmp.h

$(SRCDIR)/cstrnops.o: cstrnops.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h constrnt.h evaluatn.h cstrnchk.h cstrnutl.h extnfunc.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  insfun.h object.h multifld.h symbol.h match.h network.h ruledef.h \
  agenda.h crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h \
  memalloc.h router.h scanner.h cstrnops.h

$(SRCDIR)/cstrnpsr.o: cstrnpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h cstrnchk.h constrnt.h evaluatn.h cstrnutl.h expressn.h \
  exprnops.h constrct.h userdata.h moduldef.h utility.h insfun.h \
  object.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h \
  memalloc.h pprint.h prntutil.h router.h scanner.h sysdep.h cstrnpsr.h

$(SRCDIR)/cstrnutl.o: cstrnutl.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h memalloc.h \
  router.h scanner.h cstrnutl.h

$(SRCDIR)/default.o: default.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  constrnt.h evaluatn.h cstrnchk.h cstrnutl.h exprnpsr.h extnfunc.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  insfun.h object.h multifld.h symbol.h match.h network.h ruledef.h \
  agenda.h crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h \
  scanner.h factmngr.h tmpltdef.h factbld.h facthsh.h inscom.h pprint.h \
  prntutil.h router.h default.h

$(SRCDIR)/defins.o: defins.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h dfinsbin.h defins.h dfinscmp.h argacces.h classcom.h \
  classfun.h scanner.h cstrcpsr.h strngfun.h inspsr.h memalloc.h \
  modulpsr.h modulutl.h pprint.h prntutil.h router.h

$(SRCDIR)/developr.o: developr.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h inscom.h modulutl.h scanner.h \
  prntutil.h router.h classcom.h classfun.h developr.h

$(SRCDIR)/dffctbin.o: dffctbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h dffctdef.h memalloc.h dffctbin.h cstrcbin.h \
  modulbin.h

$(SRCDIR)/dffctbsc.o: dffctbsc.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h dffctdef.h dffctpsr.h factrhs.h factmngr.h tmpltdef.h \
  factbld.h facthsh.h scanner.h memalloc.h router.h dffctbin.h \
  cstrcbin.h modulbin.h dffctcmp.h dffctbsc.h

$(SRCDIR)/dffctcmp.o: dffctcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h dffctdef.h dffctcmp.h

$(SRCDIR)/dffctdef.o: dffctdef.c setup.h envrnmnt.h entities.h usrsetup.h \
  dffctbsc.h dffctdef.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h \
  dffctpsr.h memalloc.h bload.h exprnbin.h sysdep.h symblbin.h \
  dffctbin.h cstrcbin.h modulbin.h dffctcmp.h

$(SRCDIR)/dffctpsr.o: dffctpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h cstrcpsr.h strngfun.h dffctbsc.h dffctdef.h factrhs.h \
  factmngr.h tmpltdef.h factbld.h facthsh.h scanner.h memalloc.h \
  modulutl.h pprint.h prntutil.h router.h dffctpsr.h

$(SRCDIR)/dffnxbin.o: dffnxbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrcbin.h memalloc.h modulbin.h dffnxbin.h \
  dffnxfun.h

$(SRCDIR)/dffnxcmp.o: dffnxcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h dffnxcmp.h dffnxfun.h

$(SRCDIR)/dffnxexe.o: dffnxexe.c setup.h envrnmnt.h entities.h usrsetup.h \
  constrct.h userdata.h moduldef.h utility.h evaluatn.h constant.h \
  insfun.h object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h prcdrfun.h prccode.h scanner.h \
  prntutil.h proflfun.h router.h watch.h dffnxexe.h dffnxfun.h

$(SRCDIR)/dffnxfun.o: dffnxfun.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h dffnxbin.h dffnxfun.h dffnxcmp.h cstrcpsr.h strngfun.h \
  dffnxpsr.h modulpsr.h scanner.h dffnxexe.h watch.h argacces.h \
  memalloc.h modulutl.h prntutil.h router.h

$(SRCDIR)/dffnxpsr.o: dffnxpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h genrccom.h genrcfun.h cstrcpsr.h strngfun.h dffnxfun.h \
  exprnpsr.h scanner.h memalloc.h modulutl.h pprint.h prccode.h \
  prntutil.h router.h dffnxpsr.h

$(SRCDIR)/dfinsbin.o: dfinsbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrcbin.h defins.h memalloc.h modulbin.h \
  dfinsbin.h

$(SRCDIR)/dfinscmp.o: dfinscmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h defins.h dfinscmp.h

$(SRCDIR)/drive.o: drive.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h engine.h lgcldpnd.h retract.h \
  incrrset.h memalloc.h prntutil.h reteutil.h rulecom.h router.h drive.h

$(SRCDIR)/emathfun.o: emathfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h miscfun.h \
  prntutil.h router.h emathfun.h

$(SRCDIR)/engine.o: engine.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h argacces.h commline.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h inscom.h memalloc.h modulutl.h \
  scanner.h prccode.h prcdrfun.h prntutil.h proflfun.h reteutil.h \
  rulecom.h retract.h router.h ruledlt.h sysdep.h watch.h engine.h \
  lgcldpnd.h

$(SRCDIR)/envrnbld.o: envrnbld.c setup.h envrnmnt.h entities.h usrsetup.h \
  bmathfun.h evaluatn.h constant.h commline.h emathfun.h engine.h \
  lgcldpnd.h match.h network.h ruledef.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h retract.h filecom.h \
  iofun.h memalloc.h miscfun.h multifun.h parsefun.h pprint.h prccode.h \
  scanner.h prcdrfun.h prdctfun.h prntutil.h proflfun.h router.h \
  sortfun.h strngfun.h sysdep.h textpro.h watch.h dffctdef.h genrccom.h \
  genrcfun.h dffnxfun.h globldef.h tmpltdef.h factbld.h classini.h \
  envrnbld.h

$(SRCDIR)/envrnmnt.o: envrnmnt.c setup.h envrnmnt.h entities.h usrsetup.h \
  bmathfun.h evaluatn.h constant.h commline.h emathfun.h engine.h \
  lgcldpnd.h match.h network.h ruledef.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h retract.h filecom.h \
  iofun.h memalloc.h miscfun.h multifun.h parsefun.h prccode.h scanner.h \
  prcdrfun.h prdctfun.h prntutil.h proflfun.h router.h sortfun.h \
  strngfun.h sysdep.h textpro.h watch.h dffctdef.h genrccom.h genrcfun.h \
  dffnxfun.h globldef.h tmpltdef.h factbld.h classini.h

$(SRCDIR)/evaluatn.o: evaluatn.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h commline.h \
  factmngr.h tmpltdef.h factbld.h facthsh.h memalloc.h modulutl.h \
  scanner.h router.h prcdrfun.h prntutil.h exprnpsr.h proflfun.h \
  sysdep.h dffnxfun.h genrccom.h genrcfun.h inscom.h

$(SRCDIR)/expressn.o: expressn.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h memalloc.h prntutil.h router.h

$(SRCDIR)/exprnbin.o: exprnbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h dffctdef.h memalloc.h genrcbin.h genrcfun.h \
  dffnxbin.h dffnxfun.h factmngr.h tmpltdef.h factbld.h facthsh.h \
  tmpltbin.h cstrcbin.h modulbin.h globlbin.h globldef.h objbin.h \
  inscom.h

$(SRCDIR)/exprnops.o: exprnops.c setup.h envrnmnt.h entities.h usrsetup.h \
  cstrnchk.h constrnt.h evaluatn.h constant.h cstrnops.h cstrnutl.h \
  extnfunc.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h insfun.h object.h multifld.h symbol.h match.h network.h \
  ruledef.h agenda.h crstrtgy.h conscomp.h symblcmp.h cstrccom.h \
  objrtmch.h memalloc.h prntutil.h router.h

$(SRCDIR)/exprnpsr.o: exprnpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrnchk.h \
  memalloc.h modulutl.h scanner.h pprint.h prcdrfun.h prntutil.h \
  router.h strngrtr.h genrccom.h genrcfun.h dffnxfun.h exprnpsr.h

$(SRCDIR)/extnfunc.o: extnfunc.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h factmngr.h tmpltdef.h factbld.h facthsh.h memalloc.h \
  router.h inscom.h

$(SRCDIR)/factbin.o: factbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h factmngr.h tmpltdef.h factbld.h facthsh.h \
  memalloc.h pattern.h scanner.h reorder.h reteutil.h rulecom.h \
  rulebin.h cstrcbin.h modulbin.h factbin.h

$(SRCDIR)/factbld.o: factbld.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h factcmp.h pattern.h scanner.h \
  reorder.h factgen.h factlhs.h factmch.h factbld.h factmngr.h \
  tmpltdef.h facthsh.h memalloc.h modulutl.h reteutil.h rulecom.h \
  router.h

$(SRCDIR)/factcmp.o: factcmp.c setup.h envrnmnt.h entities.h usrsetup.h factbld.h \
  network.h match.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h factmngr.h tmpltdef.h \
  facthsh.h factcmp.h pattern.h scanner.h reorder.h

$(SRCDIR)/factcom.o: factcom.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h factmngr.h tmpltdef.h factbld.h \
  facthsh.h factrhs.h scanner.h pprint.h prntutil.h router.h sysdep.h \
  tmpltutl.h factcom.h

$(SRCDIR)/factfile.o: factfile.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h bload.h \
  exprnbin.h sysdep.h symblbin.h cstrcpsr.h strngfun.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h factrhs.h scanner.h insmngr.h inscom.h \
  memalloc.h modulpsr.h modulutl.h prntutil.h router.h strngrtr.h \
  tmpltutl.h factfile.h

$(SRCDIR)/factfun.o: factfun.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h prntutil.h router.h sysdep.h \
  tmpltutl.h factmngr.h tmpltdef.h factbld.h facthsh.h factfun.h

$(SRCDIR)/factgen.o: factgen.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  constrct.h userdata.h moduldef.h utility.h evaluatn.h insfun.h \
  object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h match.h \
  network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h exprnpsr.h scanner.h factmch.h \
  factbld.h factmngr.h tmpltdef.h facthsh.h factprt.h factrete.h \
  memalloc.h pattern.h reorder.h prcdrpsr.h reteutil.h rulecom.h \
  router.h sysdep.h tmpltfun.h tmpltlhs.h tmpltutl.h factgen.h

$(SRCDIR)/facthsh.o: facthsh.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  factmngr.h conscomp.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h tmpltdef.h factbld.h \
  facthsh.h memalloc.h router.h sysdep.h lgcldpnd.h

$(SRCDIR)/factlhs.o: factlhs.c setup.h envrnmnt.h entities.h usrsetup.h cstrcpsr.h \
  strngfun.h modulpsr.h evaluatn.h constant.h moduldef.h userdata.h \
  symbol.h scanner.h modulutl.h pattern.h expressn.h exprnops.h \
  constrct.h utility.h insfun.h object.h constrnt.h multifld.h match.h \
  network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h reorder.h pprint.h prntutil.h \
  router.h tmpltdef.h factbld.h tmpltlhs.h tmpltpsr.h tmpltutl.h \
  factmngr.h facthsh.h factlhs.h

$(SRCDIR)/factmch.o: factmch.c setup.h envrnmnt.h entities.h usrsetup.h drive.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h engine.h lgcldpnd.h retract.h \
  factgen.h reorder.h pattern.h scanner.h factrete.h incrrset.h \
  memalloc.h prntutil.h reteutil.h rulecom.h router.h sysdep.h \
  tmpltdef.h factbld.h factmch.h factmngr.h facthsh.h

$(SRCDIR)/factmngr.o: factmngr.c setup.h envrnmnt.h entities.h usrsetup.h \
  commline.h default.h constrnt.h evaluatn.h constant.h engine.h \
  lgcldpnd.h match.h network.h ruledef.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h expressn.h exprnops.h \
  multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h retract.h factbin.h factbld.h \
  factcmp.h pattern.h scanner.h reorder.h factcom.h factfile.h factfun.h \
  factmngr.h tmpltdef.h facthsh.h factmch.h factqury.h factrhs.h \
  memalloc.h prntutil.h router.h strngrtr.h sysdep.h tmpltbsc.h \
  tmpltfun.h tmpltutl.h watch.h cstrnchk.h

$(SRCDIR)/factprt.o: factprt.c setup.h envrnmnt.h entities.h usrsetup.h factgen.h \
  reorder.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h pattern.h \
  scanner.h prntutil.h router.h factprt.h

$(SRCDIR)/factqpsr.o: factqpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  exprnpsr.h extnfunc.h evaluatn.h constant.h expressn.h exprnops.h \
  constrct.h userdata.h moduldef.h utility.h insfun.h object.h \
  constrnt.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h scanner.h \
  factqury.h factmngr.h tmpltdef.h factbld.h facthsh.h modulutl.h \
  prcdrpsr.h pprint.h prntutil.h router.h strngrtr.h factqpsr.h

$(SRCDIR)/factqury.o: factqury.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h memalloc.h \
  exprnpsr.h scanner.h modulutl.h tmpltutl.h factmngr.h tmpltdef.h \
  factbld.h facthsh.h factqpsr.h prcdrfun.h prntutil.h router.h \
  factqury.h

$(SRCDIR)/factrete.o: factrete.c setup.h envrnmnt.h entities.h usrsetup.h drive.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h engine.h lgcldpnd.h retract.h \
  factgen.h reorder.h pattern.h scanner.h factmch.h factbld.h factmngr.h \
  tmpltdef.h facthsh.h incrrset.h memalloc.h reteutil.h rulecom.h \
  router.h factrete.h

$(SRCDIR)/factrhs.o: factrhs.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h cstrcpsr.h strngfun.h exprnpsr.h scanner.h modulutl.h \
  modulpsr.h pattern.h reorder.h pprint.h prntutil.h router.h strngrtr.h \
  tmpltpsr.h tmpltdef.h factbld.h tmpltrhs.h tmpltutl.h factmngr.h \
  facthsh.h factrhs.h

$(SRCDIR)/filecom.o: filecom.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h bload.h exprnbin.h sysdep.h \
  symblbin.h bsave.h commline.h cstrcpsr.h strngfun.h fileutil.h \
  memalloc.h router.h filecom.h

$(SRCDIR)/filertr.o: filertr.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  memalloc.h router.h sysdep.h filertr.h

$(SRCDIR)/fileutil.o: fileutil.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h commline.h \
  cstrcpsr.h strngfun.h memalloc.h prcdrfun.h pprint.h prntutil.h \
  router.h scanner.h strngrtr.h sysdep.h filecom.h fileutil.h

$(SRCDIR)/generate.o: generate.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h globlpsr.h memalloc.h pattern.h reorder.h prntutil.h \
  router.h generate.h analysis.h

$(SRCDIR)/genrcbin.o: genrcbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrcbin.h genrccom.h genrcfun.h memalloc.h \
  modulbin.h objbin.h router.h genrcbin.h

$(SRCDIR)/genrccmp.o: genrccmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h genrccom.h genrcfun.h \
  objcmp.h genrccmp.h

$(SRCDIR)/genrccom.o: genrccom.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h bload.h \
  exprnbin.h sysdep.h symblbin.h classcom.h inscom.h cstrcpsr.h \
  strngfun.h genrcbin.h genrcfun.h genrccmp.h genrcexe.h genrcpsr.h \
  memalloc.h modulpsr.h scanner.h modulutl.h router.h strngrtr.h watch.h \
  prntutil.h genrccom.h

$(SRCDIR)/genrcexe.o: genrcexe.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h argacces.h genrccom.h genrcfun.h prcdrfun.h prccode.h \
  prntutil.h proflfun.h router.h genrcexe.h

$(SRCDIR)/genrcfun.o: genrcfun.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h classcom.h classfun.h scanner.h argacces.h cstrcpsr.h \
  strngfun.h genrccom.h genrcfun.h genrcexe.h memalloc.h modulutl.h \
  prccode.h prntutil.h router.h

$(SRCDIR)/genrcpsr.o: genrcpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h dffnxfun.h classfun.h scanner.h classcom.h cstrcpsr.h \
  strngfun.h exprnpsr.h genrccom.h genrcfun.h immthpsr.h memalloc.h \
  modulutl.h pprint.h prcdrpsr.h prccode.h prntutil.h router.h \
  genrcpsr.h

$(SRCDIR)/globlbin.o: globlbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h globlbsc.h globldef.h memalloc.h globlbin.h \
  modulbin.h cstrcbin.h

$(SRCDIR)/globlbsc.o: globlbsc.c setup.h envrnmnt.h entities.h usrsetup.h \
  constrct.h userdata.h moduldef.h utility.h evaluatn.h constant.h \
  insfun.h object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h globlbin.h modulbin.h cstrcbin.h \
  globldef.h globlcmp.h globlcom.h watch.h globlbsc.h

$(SRCDIR)/globlcmp.o: globlcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h globldef.h globlcmp.h

$(SRCDIR)/globlcom.o: globlcom.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h globldef.h \
  prntutil.h router.h globlcom.h

$(SRCDIR)/globldef.o: globldef.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h globlbin.h modulbin.h cstrcbin.h globldef.h commline.h \
  globlbsc.h globlcmp.h globlcom.h globlpsr.h memalloc.h modulpsr.h \
  scanner.h modulutl.h prntutil.h router.h strngrtr.h

$(SRCDIR)/globlpsr.o: globlpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h cstrcpsr.h strngfun.h exprnpsr.h scanner.h globlbsc.h \
  globldef.h memalloc.h modulpsr.h modulutl.h pprint.h prntutil.h \
  router.h watch.h globlpsr.h

$(SRCDIR)/immthpsr.o: immthpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h cstrnutl.h exprnpsr.h genrcpsr.h genrcfun.h memalloc.h \
  prccode.h immthpsr.h

$(SRCDIR)/incrrset.o: incrrset.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h argacces.h drive.h engine.h \
  lgcldpnd.h retract.h pattern.h scanner.h reorder.h router.h reteutil.h \
  rulecom.h incrrset.h

$(SRCDIR)/inherpsr.o: inherpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h memalloc.h modulutl.h pprint.h prntutil.h router.h \
  inherpsr.h

$(SRCDIR)/inscom.o: inscom.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h classcom.h classfun.h scanner.h \
  classinf.h commline.h exprnpsr.h insfile.h insmngr.h inscom.h \
  insmoddp.h insmult.h inspsr.h lgcldpnd.h memalloc.h msgcom.h msgpass.h \
  msgfun.h prntutil.h router.h strngrtr.h sysdep.h

$(SRCDIR)/insfile.o: insfile.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h classcom.h classfun.h scanner.h \
  memalloc.h factmngr.h tmpltdef.h factbld.h facthsh.h inscom.h \
  insmngr.h inspsr.h prntutil.h router.h strngrtr.h symblbin.h sysdep.h \
  insfile.h

$(SRCDIR)/insfun.o: insfun.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h classcom.h classfun.h scanner.h \
  cstrnchk.h drive.h engine.h lgcldpnd.h retract.h inscom.h insmngr.h \
  memalloc.h modulutl.h msgcom.h msgpass.h msgfun.h prccode.h prntutil.h \
  router.h

$(SRCDIR)/insmngr.o: insmngr.c setup.h envrnmnt.h entities.h usrsetup.h network.h \
  match.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h drive.h lgcldpnd.h \
  objrtfnx.h classcom.h classfun.h scanner.h cstrnchk.h engine.h \
  retract.h memalloc.h miscfun.h modulutl.h msgcom.h msgpass.h msgfun.h \
  prccode.h prntutil.h router.h sysdep.h insmngr.h inscom.h watch.h

$(SRCDIR)/insmoddp.o: insmoddp.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h inscom.h \
  insmngr.h inspsr.h memalloc.h miscfun.h msgcom.h msgpass.h msgfun.h \
  prccode.h scanner.h prntutil.h router.h insmoddp.h

$(SRCDIR)/insmult.o: insmult.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h msgfun.h msgpass.h multifun.h \
  prntutil.h router.h insmult.h

$(SRCDIR)/inspsr.o: inspsr.c setup.h envrnmnt.h entities.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h userdata.h constrct.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h scanner.h \
  classinf.h exprnpsr.h pprint.h prntutil.h router.h inspsr.h

$(SRCDIR)/insquery.o: insquery.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h classcom.h \
  classfun.h scanner.h exprnpsr.h insmngr.h inscom.h insqypsr.h \
  memalloc.h prcdrfun.h prntutil.h router.h insquery.h

$(SRCDIR)/insqypsr.o: insqypsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h exprnpsr.h \
  scanner.h insquery.h prcdrpsr.h pprint.h prntutil.h router.h \
  strngrtr.h insqypsr.h

$(SRCDIR)/iofun.o: iofun.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h commline.h exprnpsr.h scanner.h \
  filertr.h memalloc.h miscfun.h pprint.h prcdrfun.h prntutil.h router.h \
  strngrtr.h sysdep.h iofun.h

$(SRCDIR)/lgcldpnd.o: lgcldpnd.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h engine.h \
  lgcldpnd.h retract.h factmngr.h tmpltdef.h factbld.h facthsh.h \
  memalloc.h pattern.h scanner.h reorder.h reteutil.h rulecom.h router.h

$(SRCDIR)/main.o: main.c clips.h setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h memalloc.h \
  cstrcpsr.h strngfun.h fileutil.h envrnbld.h commline.h prntutil.h \
  router.h filertr.h strngrtr.h iofun.h sysdep.h bmathfun.h exprnpsr.h \
  scanner.h miscfun.h watch.h modulbsc.h bload.h exprnbin.h symblbin.h \
  bsave.h rulebsc.h engine.h lgcldpnd.h retract.h drive.h incrrset.h \
  rulecom.h dffctdef.h dffctbsc.h tmpltdef.h factbld.h tmpltbsc.h \
  tmpltfun.h factmngr.h facthsh.h factcom.h factfile.h factfun.h \
  globldef.h globlbsc.h globlcom.h dffnxfun.h genrccom.h genrcfun.h \
  classcom.h classexm.h classfun.h classinf.h classini.h classpsr.h \
  defins.h inscom.h insfile.h insmngr.h msgcom.h msgpass.h

$(SRCDIR)/memalloc.o: memalloc.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h memalloc.h prntutil.h router.h utility.h evaluatn.h \
  moduldef.h userdata.h insfun.h object.h constrct.h constrnt.h \
  expressn.h exprnops.h multifld.h symbol.h match.h network.h ruledef.h \
  agenda.h crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h \
  objrtmch.h

$(SRCDIR)/miscfun.o: miscfun.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h exprnpsr.h scanner.h memalloc.h \
  prntutil.h router.h sysdep.h dffnxfun.h factfun.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h tmpltutl.h miscfun.h

$(SRCDIR)/modulbin.o: modulbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrcbin.h memalloc.h modulbin.h

$(SRCDIR)/modulbsc.o: modulbsc.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h bload.h \
  exprnbin.h sysdep.h symblbin.h modulbin.h cstrcbin.h modulcmp.h \
  prntutil.h router.h modulbsc.h

$(SRCDIR)/modulcmp.o: modulcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h sysdep.h modulcmp.h

$(SRCDIR)/moduldef.o: moduldef.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h bload.h \
  exprnbin.h sysdep.h symblbin.h modulbin.h cstrcbin.h memalloc.h \
  modulbsc.h modulcmp.h modulpsr.h scanner.h prntutil.h router.h

$(SRCDIR)/modulpsr.o: modulpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h memalloc.h modulutl.h scanner.h pprint.h prntutil.h \
  router.h bload.h exprnbin.h sysdep.h symblbin.h modulpsr.h

$(SRCDIR)/modulutl.o: modulutl.c setup.h envrnmnt.h entities.h usrsetup.h \
  cstrcpsr.h strngfun.h memalloc.h modulpsr.h evaluatn.h constant.h \
  moduldef.h userdata.h symbol.h scanner.h pprint.h prntutil.h router.h \
  sysdep.h watch.h expressn.h exprnops.h constrct.h utility.h insfun.h \
  object.h constrnt.h multifld.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h \
  modulutl.h

$(SRCDIR)/msgcom.o: msgcom.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h bload.h exprnbin.h sysdep.h \
  symblbin.h classcom.h classfun.h scanner.h classinf.h msgpsr.h \
  insmoddp.h msgfun.h msgpass.h memalloc.h prccode.h prntutil.h router.h \
  watch.h msgcom.h

$(SRCDIR)/msgfun.o: msgfun.c setup.h envrnmnt.h entities.h usrsetup.h classcom.h \
  cstrccom.h moduldef.h userdata.h constrct.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h scanner.h \
  inscom.h memalloc.h msgcom.h msgpass.h prccode.h prntutil.h router.h \
  msgfun.h

$(SRCDIR)/msgpass.o: msgpass.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h classcom.h classfun.h scanner.h \
  commline.h exprnpsr.h inscom.h memalloc.h msgcom.h msgpass.h msgfun.h \
  prccode.h prcdrfun.h prntutil.h proflfun.h router.h strngfun.h

$(SRCDIR)/msgpsr.o: msgpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h classcom.h classfun.h scanner.h cstrcpsr.h strngfun.h \
  cstrnchk.h exprnpsr.h memalloc.h modulutl.h msgcom.h msgpass.h \
  msgfun.h pprint.h prccode.h prntutil.h router.h strngrtr.h msgpsr.h

$(SRCDIR)/multifld.o: multifld.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h evaluatn.h exprnops.h expressn.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h memalloc.h scanner.h prntutil.h \
  router.h strngrtr.h

$(SRCDIR)/multifun.o: multifun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h memalloc.h multifun.h pprint.h prcdrpsr.h prcdrfun.h \
  prntutil.h router.h

$(SRCDIR)/objbin.o: objbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h classcom.h classfun.h scanner.h classini.h \
  cstrcbin.h cstrnbin.h memalloc.h modulbin.h msgcom.h msgpass.h \
  msgfun.h prntutil.h router.h objrtbin.h objbin.h

$(SRCDIR)/objcmp.o: objcmp.c setup.h envrnmnt.h entities.h usrsetup.h conscomp.h \
  constrct.h userdata.h moduldef.h utility.h evaluatn.h constant.h \
  insfun.h object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h cstrccom.h objrtmch.h \
  extnfunc.h symblcmp.h classcom.h classfun.h scanner.h classini.h \
  cstrncmp.h objrtfnx.h sysdep.h objrtcmp.h objcmp.h

$(SRCDIR)/objrtbin.o: objrtbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h classfun.h scanner.h classcom.h memalloc.h \
  pattern.h reorder.h reteutil.h rulecom.h rulebin.h cstrcbin.h \
  modulbin.h objrtbin.h

$(SRCDIR)/objrtbld.o: objrtbld.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h cstrnutl.h cstrnchk.h cstrnops.h drive.h inscom.h insmngr.h \
  memalloc.h pattern.h reorder.h prntutil.h reteutil.h rulecom.h \
  rulepsr.h exprnpsr.h objrtgen.h objrtfnx.h pprint.h router.h \
  objrtbin.h objrtcmp.h objrtbld.h

$(SRCDIR)/objrtcmp.o: objrtcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h classcom.h objrtfnx.h \
  pattern.h scanner.h reorder.h sysdep.h objrtcmp.h

$(SRCDIR)/objrtfnx.o: objrtfnx.c setup.h envrnmnt.h entities.h usrsetup.h \
  classcom.h cstrccom.h moduldef.h userdata.h constrct.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h objrtmch.h classfun.h \
  scanner.h bload.h exprnbin.h sysdep.h symblbin.h drive.h engine.h \
  lgcldpnd.h retract.h memalloc.h prntutil.h reteutil.h rulecom.h \
  router.h objrtfnx.h

$(SRCDIR)/objrtgen.o: objrtgen.c setup.h envrnmnt.h entities.h usrsetup.h \
  classfun.h object.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h scanner.h \
  classcom.h objrtfnx.h objrtgen.h reorder.h pattern.h

$(SRCDIR)/objrtmch.o: objrtmch.c setup.h envrnmnt.h entities.h usrsetup.h \
  classfun.h object.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h scanner.h \
  classcom.h memalloc.h drive.h engine.h lgcldpnd.h retract.h incrrset.h \
  objrtfnx.h prntutil.h reteutil.h rulecom.h ruledlt.h reorder.h \
  pattern.h router.h insmngr.h inscom.h

$(SRCDIR)/parsefun.o: parsefun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h exprnpsr.h scanner.h memalloc.h pprint.h prcdrpsr.h \
  prntutil.h router.h strngrtr.h parsefun.h

$(SRCDIR)/pattern.o: pattern.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  constrnt.h evaluatn.h cstrnchk.h cstrnutl.h exprnpsr.h extnfunc.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  insfun.h object.h multifld.h symbol.h match.h network.h ruledef.h \
  agenda.h crstrtgy.h conscomp.h symblcmp.h cstrccom.h objrtmch.h \
  scanner.h memalloc.h pprint.h prntutil.h reteutil.h rulecom.h router.h \
  rulecmp.h pattern.h reorder.h

$(SRCDIR)/pprint.o: pprint.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  memalloc.h sysdep.h utility.h evaluatn.h moduldef.h userdata.h \
  insfun.h object.h constrct.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h pprint.h

$(SRCDIR)/prccode.o: prccode.c setup.h envrnmnt.h entities.h usrsetup.h memalloc.h \
  constant.h globlpsr.h expressn.h exprnops.h constrct.h userdata.h \
  moduldef.h utility.h evaluatn.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h pprint.h prcdrpsr.h prntutil.h router.h prccode.h

$(SRCDIR)/prcdrfun.o: prcdrfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrnchk.h \
  cstrnops.h exprnpsr.h scanner.h memalloc.h prcdrpsr.h router.h \
  prcdrfun.h globldef.h

$(SRCDIR)/prcdrpsr.o: prcdrpsr.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrnchk.h \
  cstrnops.h cstrnutl.h exprnpsr.h scanner.h memalloc.h modulutl.h \
  pprint.h prntutil.h router.h prcdrpsr.h globldef.h globlpsr.h

$(SRCDIR)/prdctfun.o: prdctfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnpsr.h \
  scanner.h router.h prdctfun.h

$(SRCDIR)/prntutil.o: prntutil.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h factmngr.h tmpltdef.h factbld.h facthsh.h inscom.h \
  insmngr.h memalloc.h multifun.h router.h scanner.h strngrtr.h sysdep.h \
  prntutil.h

$(SRCDIR)/proflfun.o: proflfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h classcom.h \
  dffnxfun.h genrccom.h genrcfun.h memalloc.h msgcom.h msgpass.h \
  router.h sysdep.h proflfun.h

$(SRCDIR)/reorder.o: reorder.c setup.h envrnmnt.h entities.h usrsetup.h cstrnutl.h \
  constrnt.h evaluatn.h constant.h extnfunc.h expressn.h exprnops.h \
  constrct.h userdata.h moduldef.h utility.h insfun.h object.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h symblcmp.h cstrccom.h objrtmch.h memalloc.h pattern.h \
  scanner.h reorder.h prntutil.h router.h rulelhs.h

$(SRCDIR)/reteutil.o: reteutil.c setup.h envrnmnt.h entities.h usrsetup.h drive.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h engine.h lgcldpnd.h retract.h \
  incrrset.h memalloc.h pattern.h scanner.h reorder.h prntutil.h \
  router.h rulecom.h reteutil.h

$(SRCDIR)/retract.o: retract.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h argacces.h drive.h engine.h \
  lgcldpnd.h retract.h memalloc.h prntutil.h reteutil.h rulecom.h \
  router.h

$(SRCDIR)/router.o: router.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h filertr.h memalloc.h prntutil.h \
  scanner.h strngrtr.h sysdep.h router.h

$(SRCDIR)/rulebin.o: rulebin.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h bload.h exprnbin.h sysdep.h \
  symblbin.h bsave.h engine.h lgcldpnd.h retract.h memalloc.h pattern.h \
  scanner.h reorder.h reteutil.h rulecom.h rulebsc.h rulebin.h \
  cstrcbin.h modulbin.h

$(SRCDIR)/rulebld.o: rulebld.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  constrct.h userdata.h moduldef.h utility.h evaluatn.h insfun.h \
  object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h match.h \
  network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h drive.h incrrset.h memalloc.h \
  pattern.h scanner.h reorder.h prntutil.h reteutil.h rulecom.h router.h \
  rulebld.h rulepsr.h watch.h

$(SRCDIR)/rulebsc.o: rulebsc.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h drive.h engine.h lgcldpnd.h retract.h \
  reteutil.h rulecom.h router.h watch.h rulebin.h cstrcbin.h modulbin.h \
  rulecmp.h rulebsc.h

$(SRCDIR)/rulecmp.o: rulecmp.c setup.h envrnmnt.h entities.h usrsetup.h factbld.h \
  network.h match.h ruledef.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h objrtmch.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h pattern.h scanner.h \
  reorder.h reteutil.h rulecom.h rulecmp.h

$(SRCDIR)/rulecom.o: rulecom.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h engine.h lgcldpnd.h retract.h \
  incrrset.h memalloc.h pattern.h scanner.h reorder.h prntutil.h \
  reteutil.h rulecom.h router.h ruledlt.h sysdep.h watch.h rulebin.h \
  cstrcbin.h modulbin.h

$(SRCDIR)/rulecstr.o: rulecstr.c setup.h envrnmnt.h entities.h usrsetup.h \
  analysis.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h reorder.h \
  pattern.h scanner.h cstrnchk.h cstrnops.h cstrnutl.h prcdrpsr.h \
  prntutil.h router.h rulepsr.h rulecstr.h

$(SRCDIR)/ruledef.o: ruledef.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h drive.h engine.h lgcldpnd.h retract.h \
  memalloc.h pattern.h scanner.h reorder.h reteutil.h rulecom.h \
  rulebsc.h rulepsr.h ruledlt.h bload.h exprnbin.h sysdep.h symblbin.h \
  rulebin.h cstrcbin.h modulbin.h rulecmp.h

$(SRCDIR)/ruledlt.o: ruledlt.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h bload.h exprnbin.h sysdep.h \
  symblbin.h drive.h engine.h lgcldpnd.h retract.h memalloc.h pattern.h \
  scanner.h reorder.h reteutil.h rulecom.h ruledlt.h

$(SRCDIR)/rulelhs.o: rulelhs.c setup.h envrnmnt.h entities.h usrsetup.h agenda.h \
  ruledef.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h objrtmch.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h crstrtgy.h argacces.h cstrnchk.h exprnpsr.h \
  scanner.h memalloc.h pattern.h reorder.h pprint.h prntutil.h router.h \
  rulelhs.h

$(SRCDIR)/rulepsr.o: rulepsr.c setup.h envrnmnt.h entities.h usrsetup.h analysis.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h reorder.h pattern.h scanner.h \
  cstrcpsr.h strngfun.h cstrnchk.h cstrnops.h engine.h lgcldpnd.h \
  retract.h exprnpsr.h incrrset.h memalloc.h modulutl.h prccode.h \
  prcdrpsr.h pprint.h prntutil.h router.h rulebld.h rulebsc.h rulecstr.h \
  ruledlt.h rulelhs.h watch.h tmpltfun.h factmngr.h tmpltdef.h factbld.h \
  facthsh.h bload.h exprnbin.h sysdep.h symblbin.h rulepsr.h

$(SRCDIR)/scanner.o: scanner.c setup.h envrnmnt.h entities.h usrsetup.h constant.h \
  memalloc.h pprint.h prntutil.h router.h symbol.h sysdep.h utility.h \
  evaluatn.h moduldef.h userdata.h insfun.h object.h constrct.h \
  constrnt.h expressn.h exprnops.h multifld.h match.h network.h \
  ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h symblcmp.h \
  cstrccom.h objrtmch.h scanner.h

$(SRCDIR)/sortfun.o: sortfun.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h dffnxfun.h memalloc.h sysdep.h \
  sortfun.h

$(SRCDIR)/strngfun.o: strngfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h commline.h \
  cstrcpsr.h strngfun.h engine.h lgcldpnd.h retract.h exprnpsr.h \
  scanner.h memalloc.h miscfun.h prcdrpsr.h pprint.h prntutil.h router.h \
  strngrtr.h sysdep.h drive.h

$(SRCDIR)/strngrtr.o: strngrtr.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h memalloc.h prntutil.h router.h sysdep.h strngrtr.h \
  utility.h evaluatn.h moduldef.h userdata.h insfun.h object.h \
  constrct.h constrnt.h expressn.h exprnops.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h

$(SRCDIR)/symblbin.o: symblbin.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h bload.h \
  exprnbin.h sysdep.h symblbin.h bsave.h cstrnbin.h exprnpsr.h scanner.h \
  memalloc.h router.h

$(SRCDIR)/symblcmp.o: symblcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrncmp.h \
  exprnpsr.h scanner.h memalloc.h prntutil.h router.h sysdep.h

$(SRCDIR)/symbol.o: symbol.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h memalloc.h prntutil.h router.h \
  sysdep.h

$(SRCDIR)/sysdep.o: sysdep.c setup.h envrnmnt.h entities.h usrsetup.h memalloc.h \
  sysdep.h

$(SRCDIR)/textpro.o: textpro.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h commline.h memalloc.h prntutil.h \
  router.h sysdep.h textpro.h

$(SRCDIR)/tmpltbin.o: tmpltbin.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h bsave.h cstrnbin.h factbin.h factbld.h factmngr.h \
  tmpltdef.h facthsh.h memalloc.h tmpltpsr.h tmpltutl.h tmpltbin.h \
  cstrcbin.h modulbin.h

$(SRCDIR)/tmpltbsc.o: tmpltbsc.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrcpsr.h \
  strngfun.h factrhs.h factmngr.h tmpltdef.h factbld.h facthsh.h \
  scanner.h memalloc.h router.h tmpltbin.h cstrcbin.h modulbin.h \
  tmpltcmp.h tmpltpsr.h tmpltutl.h tmpltbsc.h

$(SRCDIR)/tmpltcmp.o: tmpltcmp.c setup.h envrnmnt.h entities.h usrsetup.h \
  conscomp.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  cstrccom.h objrtmch.h extnfunc.h symblcmp.h cstrncmp.h factcmp.h \
  pattern.h scanner.h reorder.h tmpltdef.h factbld.h tmpltcmp.h

$(SRCDIR)/tmpltdef.o: tmpltdef.c setup.h envrnmnt.h entities.h usrsetup.h \
  cstrccom.h moduldef.h userdata.h constrct.h utility.h evaluatn.h \
  constant.h insfun.h object.h constrnt.h expressn.h exprnops.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h objrtmch.h cstrnchk.h memalloc.h \
  modulpsr.h scanner.h modulutl.h pattern.h reorder.h router.h \
  tmpltbsc.h tmpltdef.h factbld.h tmpltfun.h factmngr.h facthsh.h \
  tmpltpsr.h tmpltutl.h bload.h exprnbin.h sysdep.h symblbin.h \
  tmpltbin.h cstrcbin.h modulbin.h tmpltcmp.h

$(SRCDIR)/tmpltfun.o: tmpltfun.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h commline.h \
  cstrnchk.h default.h exprnpsr.h scanner.h factmngr.h tmpltdef.h \
  factbld.h facthsh.h factrhs.h memalloc.h modulutl.h pprint.h \
  prcdrpsr.h prntutil.h reorder.h pattern.h router.h sysdep.h tmpltlhs.h \
  tmpltrhs.h tmpltutl.h tmpltfun.h

$(SRCDIR)/tmpltlhs.o: tmpltlhs.c setup.h envrnmnt.h entities.h usrsetup.h \
  constant.h constrct.h userdata.h moduldef.h utility.h evaluatn.h \
  insfun.h object.h constrnt.h expressn.h exprnops.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h exprnpsr.h scanner.h factrhs.h \
  factmngr.h tmpltdef.h factbld.h facthsh.h memalloc.h modulutl.h \
  pattern.h reorder.h pprint.h prntutil.h router.h tmpltutl.h tmpltlhs.h

$(SRCDIR)/tmpltpsr.o: tmpltpsr.c setup.h envrnmnt.h entities.h usrsetup.h bload.h \
  utility.h evaluatn.h constant.h moduldef.h userdata.h insfun.h \
  object.h constrct.h constrnt.h expressn.h exprnops.h multifld.h \
  symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h \
  extnfunc.h symblcmp.h cstrccom.h objrtmch.h exprnbin.h sysdep.h \
  symblbin.h cstrcpsr.h strngfun.h cstrnchk.h cstrnpsr.h cstrnutl.h \
  default.h exprnpsr.h scanner.h factmngr.h tmpltdef.h factbld.h \
  facthsh.h memalloc.h modulutl.h pattern.h reorder.h pprint.h \
  prntutil.h router.h tmpltbsc.h watch.h tmpltpsr.h

$(SRCDIR)/tmpltrhs.o: tmpltrhs.c setup.h envrnmnt.h entities.h usrsetup.h default.h \
  constrnt.h evaluatn.h constant.h extnfunc.h expressn.h exprnops.h \
  constrct.h userdata.h moduldef.h utility.h insfun.h object.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h symblcmp.h cstrccom.h objrtmch.h factrhs.h factmngr.h \
  tmpltdef.h factbld.h facthsh.h scanner.h memalloc.h modulutl.h \
  pprint.h prntutil.h router.h tmpltfun.h tmpltlhs.h tmpltutl.h \
  tmpltrhs.h

$(SRCDIR)/tmpltutl.o: tmpltutl.c setup.h envrnmnt.h entities.h usrsetup.h \
  argacces.h expressn.h exprnops.h constrct.h userdata.h moduldef.h \
  utility.h evaluatn.h constant.h insfun.h object.h constrnt.h \
  multifld.h symbol.h match.h network.h ruledef.h agenda.h crstrtgy.h \
  conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h cstrnchk.h \
  memalloc.h modulutl.h scanner.h prntutil.h router.h sysdep.h \
  tmpltbsc.h tmpltdef.h factbld.h tmpltfun.h factmngr.h facthsh.h \
  tmpltpsr.h watch.h tmpltutl.h

$(SRCDIR)/userdata.o: userdata.c setup.h envrnmnt.h entities.h usrsetup.h \
  userdata.h

$(SRCDIR)/userfunctions.o: userfunctions.c clips.h setup.h envrnmnt.h entities.h \
  usrsetup.h argacces.h expressn.h exprnops.h constrct.h userdata.h \
  moduldef.h utility.h evaluatn.h constant.h insfun.h object.h \
  constrnt.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h conscomp.h extnfunc.h symblcmp.h cstrccom.h objrtmch.h \
  memalloc.h cstrcpsr.h strngfun.h fileutil.h envrnbld.h commline.h \
  prntutil.h router.h filertr.h strngrtr.h iofun.h sysdep.h bmathfun.h \
  exprnpsr.h scanner.h miscfun.h watch.h modulbsc.h bload.h exprnbin.h \
  symblbin.h bsave.h rulebsc.h engine.h lgcldpnd.h retract.h drive.h \
  incrrset.h rulecom.h dffctdef.h dffctbsc.h tmpltdef.h factbld.h \
  tmpltbsc.h tmpltfun.h factmngr.h facthsh.h factcom.h factfile.h \
  factfun.h globldef.h globlbsc.h globlcom.h dffnxfun.h genrccom.h \
  genrcfun.h classcom.h classexm.h classfun.h classinf.h classini.h \
  classpsr.h defins.h inscom.h insfile.h insmngr.h msgcom.h msgpass.h

$(SRCDIR)/utility.o: utility.c setup.h envrnmnt.h entities.h usrsetup.h commline.h \
  evaluatn.h constant.h factmngr.h conscomp.h constrct.h userdata.h \
  moduldef.h utility.h insfun.h object.h constrnt.h expressn.h \
  exprnops.h multifld.h symbol.h match.h network.h ruledef.h agenda.h \
  crstrtgy.h cstrccom.h objrtmch.h extnfunc.h symblcmp.h tmpltdef.h \
  factbld.h facthsh.h memalloc.h prntutil.h router.h sysdep.h

$(SRCDIR)/watch.o: watch.c setup.h envrnmnt.h entities.h usrsetup.h argacces.h \
  expressn.h exprnops.h constrct.h userdata.h moduldef.h utility.h \
  evaluatn.h constant.h insfun.h object.h constrnt.h multifld.h symbol.h \
  match.h network.h ruledef.h agenda.h crstrtgy.h conscomp.h extnfunc.h \
  symblcmp.h cstrccom.h objrtmch.h memalloc.h router.h watch.h
