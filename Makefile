LANGS = ParseIta ParseAPI

WORDNETS = $(patsubst Parse%,WordNet%.gf,$(LANGS))

UD_BUL_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Bulgarian-BTB/*.conllu)

UD_ENG_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_English-EWT/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_English-GUM/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_English-LinES/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_English-ParTUT/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_English-PUD/*.conllu)

UD_FIN_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Finnish-FTB/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Finnish-PUD/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Finnish-TDT/*.conllu)

UD_ITA_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Italian-ISDT/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Italian-ParTUT/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Italian-PoSTWITA/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Italian-PUD/*.conllu)

UD_POR_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Portuguese-PUD/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Portuguese-GSD/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Portuguese-Bosque/*.conllu)

UD_SLV_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Slovenian-SST/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Slovenian-SSJ/*.conllu)

UD_SWE_TREEBANKS = \
	$(wildcard data/ud-treebanks-v2.3/UD_Swedish-Talbanken/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Swedish-LinES/*.conllu) \
	$(wildcard data/ud-treebanks-v2.3/UD_Swedish-PUD/*.conllu)

ifeq ($(USE_WIKIPEDIA),YES)
	UD_ENG_TREEBANKS += $(wildcard data/English/en-wikipedia-00[01].conllu.xz)
	UD_BUL_TREEBANKS += $(wildcard data/Bulgarian/bg-wikipedia-00[01].conllu.xz)
	UD_SWE_TREEBANKS += $(wildcard data/Swedish/sv-wikipedia-00[01].conllu.xz)
	UD_FIN_TREEBANKS += $(wildcard data/Finnish/fi-wikipedia-00[01].conllu.xz)
endif

SHARED_PATH = $(shell gf --version | tail -1 | cut -c 16-)
DOC_PATH = $(SHARED_PATH)/www
INSTALL_PATH = $(SHARED_PATH)/lib

all: build_dirs Parse.pgf semantics.db 

Parse.pgf: $(patsubst %, build/%.pgf, $(LANGS)) Parse.probs
	gf --make --probs=Parse.probs --boot -name=Parse $(patsubst %, build/%.pgf, $(LANGS))

build/gfo/WordNet.gfo:

build/gfo/WordNet%.gfo: WordNet%.gf WordNet.gf
	gf --batch --gfo-dir=build/gfo --no-pmcfg $<

build/Parse%.pgf: Parse%.gf Parse.gf build/gfo/WordNet%.gfo build/gfo/WordNet.gfo
	gf --make -name=$(basename $(@F)) --gfo-dir=build/gfo --output-dir=build $<

Parse.probs Parse.uncond.probs: train/statistics.hs examples.txt build/ParseAPI.pgf
	stack exec .stack-work/install/x86_64-linux-tinfo6/47a1b31dee0b505c8511f68a8cd436e58546b2f29f552654729a634ebe8d86a5/8.10.7/bin/gf-wn-statistics $^

#build/udsenser: train/udsenser.hs train/GF2UED.hs build/train/EM.hs build/train/Matching.hs build/train/em_core.o build/train/em_data_stream.o
#	ghc --make -odir build/train -hidir build/train -O2 $^ -o $@ -lpgf -lgu -lm -llzma -lpthread

semantics.db: build/glosses WordNet.gf $(patsubst Parse%, WordNet%.gf, $(LANGS)) examples.txt Parse.uncond.probs
	stak exec .stack-work/install/x86_64-linux-tinfo6/47a1b31dee0b505c8511f68a8cd436e58546b2f29f552654729a634ebe8d86a5/8.10.7/bin/gf-wn-glosses 

build/glosses: www-services/glosses.hs www-services/SenseSchema.hs www-services/Interval.hs
	stack build 
	
.SECONDARY:

.PHONY: build_dirs

build_dirs:
	mkdir -p build
	mkdir -p build/gfo
	mkdir -p build/train
	mkdir -p build/www-services
