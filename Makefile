GF_SOURCES = \
	Parse.gf ParseEng.gf ParsePor.gf ParseSwe.gf ParseBul.gf ParseFin.gf ParseAPI.gf \
	ParseExtend.gf ParseExtendEng.gf ParseExtendPor.gf ParseExtendSwe.gf ParseExtendBul.gf ParseExtendFin.gf \
	WordNet.gf WordNetEng.gf WordNetSwe.gf WordNetPor.gf WordNetBul.gf WordNetFin.gf WordNetAPI.gf \

UD_ENG_TREEBANKS = \
	$(wildcard ud-treebanks-v2.3/UD_English-EWT/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_English-GUM/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_English-LinES/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_English-ParTUT/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_English-PUD/*.conllu)

UD_BUL_TREEBANKS = \
	$(wildcard ud-treebanks-v2.3/UD_Bulgarian-BTB/*.conllu)

UD_SWE_TREEBANKS = \
	$(wildcard ud-treebanks-v2.3/UD_Swedish-Talbanken/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_Swedish-LinES/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_Swedish-PUD/*.conllu)

UD_FIN_TREEBANKS = \
	$(wildcard ud-treebanks-v2.3/UD_Finnish-FTB/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_Finnish-PUD/*.conllu) \
	$(wildcard ud-treebanks-v2.3/UD_Finnish-TDT/*.conllu)

ifeq ($(USE_WIKIPEDIA),YES)
	UD_ENG_TREEBANKS += $(wildcard English/en-wikipedia-00[01].conllu.xz)
	UD_BUL_TREEBANKS += $(wildcard Bulgarian/bg-wikipedia-00[01].conllu.xz)
	UD_SWE_TREEBANKS += $(wildcard Swedish/sv-wikipedia-00[01].conllu.xz)
	UD_FIN_TREEBANKS += $(wildcard Finnish/fi-wikipedia-00[01].conllu.xz)
endif


all: build_dirs Parse.pgf semantics.db build/SenseService

Parse.pgf: $(GF_SOURCES) Parse.probs
	gf --make --probs=Parse.probs --gfo-dir=build/gfo ParseBul.gf ParseEng.gf ParseFin.gf ParsePor.gf ParseSwe.gf ParseAPI.gf
	scp Parse.pgf www.grammaticalframework.org:/usr/local/www/GF-demos/www/robust/Parse.pgf

build/Parse.noprobs.pgf: $(GF_SOURCES)
	gf --make -name=Parse.noprobs --gfo-dir=build/gfo --output-dir=build ParseBul.gf ParseEng.gf ParseFin.gf ParseSwe.gf

Parse.probs Parse.bigram.probs: build/udsenser build/Parse.noprobs.pgf examples.txt $(UD_BUL_TREEBANKS) $(UD_ENG_TREEBANKS) $(UD_FIN_TREEBANKS) $(UD_SWE_TREEBANKS)
	build/udsenser build/Parse.noprobs.pgf train abstract examples.txt , ParseBul $(UD_BUL_TREEBANKS) , ParseEng $(UD_ENG_TREEBANKS) , ParseFin $(UD_FIN_TREEBANKS) , ParseSwe $(UD_SWE_TREEBANKS)

embedding.txt: Parse.bigram.probs
	python3 train/sense_embedding.py

build/udsenser: train/udsenser.hs train/GF2UED.hs build/train/EM.hs build/train/Matching.hs build/train/em_core.o
	ghc --make -odir build/train -hidir build/train -O2 $^ -o $@ -lpgf -lgu -lm -llzma -lpthread

build/train/em_core.o: train/em_core.c train/em_core.h
	gcc -O2 -std=c99 -Itrain -c $< -o $@

build/train/EM.hs: train/EM.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

build/train/Matching.hs: train/Matching.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

semantics.db: sense-service/glosses.hs WordNet.gf examples.txt embedding.txt
	runghc -isense-service sense-service/glosses.hs
	scp semantics.db www.grammaticalframework.org:/home/krasimir/www/semantics.db

build/SenseService: sense-service/SenseService.hs sense-service/SenseSchema.hs
	ghc --make -odir build/sense-service -hidir build/sense-service -O2 -optl-static -optl-pthread $^ -o $@
	scp build/SenseService www.grammaticalframework.org:/home/krasimir/www/SenseService
	ssh -t www.grammaticalframework.org "mv www/SenseService www/SenseService.fcgi; sudo pkill -e SenseService.*"

WordNet.gf: FORCE build/SenseService
	runghc check.hs - $(shell ssh www.grammaticalframework.org ./www/SenseService.fcgi report)

FORCE:

.PHONY: build_dirs

build_dirs:
	mkdir -p build
	mkdir -p build/gfo
	mkdir -p build/train
	mkdir -p build/sense-service
