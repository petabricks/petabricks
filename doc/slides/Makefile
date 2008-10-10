#!/usr/bin/make -f
#
# Name:     Generic latex makefile
# Author:   Jason Ansel 
# Modified: Mar 14 2008
#
# Features:
#  - Compiles %.tex files to %.pdf
#  - Automatically detects all .tex files in working directory
#  - Automatically detects dependencies
#  - Runs latex/bibtex multiple times to make sure citations are generated correctly
#  - Uses either pdflatex or latex/dvips/ps2pdf (depending on config)
#  - Supports gnuplot, graphviz, and xfig based figures
#
# Example usages:
#   make         
#     Compiles all modified figures and tex files
#
#   make figures
#   	Compiles only the figures
#   
#   make {gvim,emacs,...}
#     Opens the corresponding editor with all tex files
#    
#   make {acroread,xpdf,evince,...}
#   	Compiles then opens the corresponding viewer
#
#   make monitor 
#     Runs `fileschanged`	to monitor the source files and automatically 
#     recompile when they are modified.
#
#   make workspace
#   	Opens the default viewer and editor, then runs `fileschanged` to 
#   	automatically compile when sources are modified.
#
#		make reload
#			Reloads the instance of xpdf spawned by `make xpdf`
#		
#

### Begin Configuration ####################################

#method to use, default is pdflatex.  Uncomment for latex/dvips
USE_DVI_PS_LATEX=1

# uncomment to delete figures on `make clean`
CLEAN_FIGURES=1

#the default source editor
DEFAULT_EDITOR=gvim

#the default pdf viewer
DEFAULT_VIEWER=evince


#flags passed to latex or pdflatex
LATEX_FLAGS = -file-line-error -halt-on-error -recorder

#flags passes to dot, neato, etc
GRAPHVIZ_FLAGS = -Grankdir=LR

#flags passed to ps2pdf in the normal case
PS2PDF_FLAGS = -dEPSCrop=true

#flags passed to ps2pdf to embed all fonts
PS2PDF_EMBED_FLAGS = $(PS2PDF_FLAGS) -dPDFSETTINGS=/prepress -dEmbedAllFonts=true -dSubsetFonts=true

#terminal used by gnuplot
GNUPLOT_TERMINAL=postscript eps enhanced color
#GNUPLOT_TERMINAL=postscript eps enhanced

### End Configuration ####################################

all: figures pdf dvi

#build up some files lists
BASE         :=$(basename $(wildcard *.tex))
FILES_TEX    :=$(addsuffix .tex, $(BASE))
FILES_PDF    :=$(addsuffix .pdf, $(BASE))
FILES_PS     :=$(addsuffix .ps,  $(BASE))
FILES_LOG    :=$(addsuffix .log, $(BASE))
FILES_AUX    :=$(addsuffix .aux, $(BASE))
FILES_BBL    :=$(addsuffix .bbl, $(BASE))
FILES_BLG    :=$(addsuffix .blg, $(BASE))
FILES_FLS    :=$(addsuffix .fls, $(BASE))
FILES_DEPS   :=$(addsuffix .deps, $(BASE))
FILES_BIB    :=$(wildcard *.bib)

FIGURES_SRC  :=$(wildcard *.dot     figures/*.dot     \
								*.neato   figures/*.neato   \
								*.twopi   figures/*.twopi   \
								*.circo   figures/*.circo   \
								*.fdp     figures/*.fdp     \
								*.fig     figures/*.fig     \
								*.gnuplot plots/*.gnuplot)
FIGURES_EPS  :=$(addsuffix .eps, $(basename $(FIGURES_SRC)))
FIGURES_PDF  :=$(addsuffix .pdf, $(basename $(FIGURES_SRC)))

#tweak file lists based on LATEX_ENGINE
ifndef USE_DVI_PS_LATEX
FIGURES      := $(FIGURES_PDF)
FILES_DVI    :=
GRAPHVIZ_FLAGS += -Tpdf
GRAPHVIZ_EXT = pdf
else
FIGURES      := $(FIGURES_EPS)
FILES_DVI    :=$(addsuffix .dvi, $(BASE))
GRAPHVIZ_FLAGS += -Tps2 
GRAPHVIZ_EXT = eps
endif

SOURCES :=$(FILES_TEX) $(FIGURES_SRC) $(FILES_BIB)
TARGETS :=$(FILES_PDF) $(FILES_PS) $(FILES_DVI) $(FILES_BBL) $(FILES_AUX) $(FILES_LOG) $(FILES_BLG) $(FILES_FLS) $(FILES_DEPS)

DIRNAME = $(shell basename `pwd`)
XPDF_REMOTE_SESSION = $(shell pwd | tr '/ ' _)

### some simple targets
pdf: $(FILES_PDF)
ps: $(FILES_PS)
dvi: $(FILES_DVI) 
plots figures: $(FIGURES)
bibs: $(FILES_BIB) 

.PHONY: plots figures bibs pdf ps dvi

### to work with make monitor (fileschanged)
A: reload
M: reload

### misc gui's/tools
acroread: $(FILES_PDF)
	acroread $(FILES_PDF)

ggv: $(FILES_PDF)
	ggv $<

kpdf: $(FILES_PDF)
	kpdf $<

xpdf: $(FILES_PDF)
	xpdf -remote $(XPDF_REMOTE_SESSION) $<

viewer: $(DEFAULT_VIEWER)

reload: $(FILES_PDF)
	case "$(DEFAULT_VIEWER)" in \
		xpdf)   xpdf -remote $(XPDF_REMOTE_SESSION) -reload ;; \
		evince) evince $< ;; \
		kpdf)   ;; \
	esac &

evince: $(FILES_PDF)
	evince $<

ifdef FILES_DVI
xdvi: $(FILES_DVI)
	xdvi $(FILES_DVI)
endif

editor: $(DEFAULT_EDITOR)

gvim:
	gvim $(FILES_TEX)

vi:
	vi $(FILES_TEX)

emacs:
	emacs $(FILES_TEX)

gedit:
	gedit $(FILES_TEX)

kate:
	kate $(FILES_TEX)

monitor: reload
	 fileschanged -x make $(FILES_TEX) $(FILES_BIB) $(FIGURES_SRC)
	
workspace:
	(make editor&)
	(make viewer&)
	make monitor

texmaker:
	texmaker $(TEX_FILES)

### depends

override __include_deps := $(wildcard *.deps)
include $(__include_deps) 

override define __depends_code
	test -f $*.fls && cat $*.fls \
		| grep '^INPUT' 					 \
	  | cut -d ' ' -f 2 				 \
		| grep -v '^$*.aux$$' 		 \
		| sort | uniq 						 \
		| xargs -n 8 echo '$@: '   \
		> $*.deps
endef

### the actual latex rules

override define __latex_code
	unset TEXINPUTS; export TEXINPUTS;
	#first passes for bbl/aux
	$(LATEX) $(LATEX_FLAGS) $< < /dev/null > /dev/null 2>&1 || true 
	$(BIBTEX) $* </dev/null || true
	#and again; to get the citation order right
	$(LATEX) $(LATEX_FLAGS) $< < /dev/null > /dev/null 2>&1 || true 
	#and again; to make final version
	$(LATEX) $(LATEX_FLAGS) $< < /dev/null || (echo "LATEX CALL FAILED!!!"; exit 1)
	$(__depends_code)
endef

ifndef USE_DVI_PS_LATEX

LATEX=pdflatex
BIBTEX=bibtex
%.pdf: %.tex $(FIGURES) $(FILES_BIB) 
	$(__latex_code)

else
	
LATEX=latex
BIBTEX=bibtex
%.dvi %.ps: %.tex $(FIGURES) $(FILES_BIB)
	$(__latex_code)
	#convert to postscript
	dvips -tletter -Ppdf -G0 -o $*.ps $*.dvi

endif


### pdf/ps conversions
%.pdf: %.ps
	(cd $(dir $<) && ps2pdf $(PS2PDF_FLAGS) $(notdir $<))

%.pdf: %.eps
	(cd $(dir $<) && ps2pdf $(PS2PDF_FLAGS) $(notdir $<))

### gnuplot rules
%.eps: %.gnuplot 
	echo 'set terminal $(GNUPLOT_TERMINAL);' | cat - $< \
	| (cd $(dir $<) && gnuplot) > $@

### graphviz rules
%.$(GRAPHVIZ_EXT): %.dot
	dot $(GRAPHVIZ_FLAGS) < $< > $@

%.$(GRAPHVIZ_EXT): %.neato
	neato $(GRAPHVIZ_FLAGS) < $< > $@

%.$(GRAPHVIZ_EXT): %.twopi
	twopi $(GRAPHVIZ_FLAGS) < $< > $@

%.$(GRAPHVIZ_EXT): %.circo
	circo $(GRAPHVIZ_FLAGS) < $< > $@

%.$(GRAPHVIZ_EXT): %.fdp
	fdp $(GRAPHVIZ_FLAGS) < $< > $@

%.pdf: %.tdot
	dot2tex -tmath < $< > $*-tmp
	pdflatex $*-tmp </dev/null
	pdfcrop $*-tmp.pdf
	mv $*-tmp-crop.pdf $@
	rm -f $*-tmp{,.*}

%.itex: %.tdot
	dot2tex -tmath --figonly < $< > $@

### xfig rules

%.eps: %.fig
	fig2dev -Leps $< $@

#%.pdf: %.fig
#	fig2dev -Lpdf $< $@

### clean/dist etc
clean:
	rm -f $(TARGETS) *.out
ifdef CLEAN_FIGURES
	rm -f $(FIGURES_EPS) $(FIGURES_PDF)
endif

distclean: clean
	rm -f *~ texput.log

dist: distclean
	(cd ..;tar -zcvf $(DIRNAME).tar.gz $(DIRNAME))
	@ls -lh ../$(DIRNAME).tar.gz 2>/dev/null || ls -l ../$(DIRNAME).tar.gz

