GF_SOURCES = \
	Parse.gf ParseEng.gf ParsePor.gf ParseSwe.gf ParseBul.gf ParseFin.gf ParseAPI.gf \
	ParseExtend.gf ParseExtendEng.gf ParseExtendPor.gf ParseExtendSwe.gf ParseExtendBul.gf ParseExtendFin.gf \
	WordNet.gf WordNetEng.gf WordNetSwe.gf WordNetPor.gf WordNetBul.gf WordNetFin.gf WordNetAPI.gf \

UD_ENG_TREEBANKS = \
	ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-train.conllu \
	ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-test.conllu \
	ud-treebanks-v2.2/UD_English-EWT/en_ewt-ud-dev.conllu \
	ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-train.conllu \
	ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-test.conllu \
	ud-treebanks-v2.2/UD_English-GUM/en_gum-ud-dev.conllu \
	ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-train.conllu \
	ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-test.conllu \
	ud-treebanks-v2.2/UD_English-LinES/en_lines-ud-dev.conllu \
	ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-train.conllu \
	ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-test.conllu \
	ud-treebanks-v2.2/UD_English-ParTUT/en_partut-ud-dev.conllu \
	ud-treebanks-v2.2/UD_English-PUD/en_pud-ud-test.conllu

UD_BUL_TREEBANKS = \
	ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-train.conllu \
	ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-test.conllu \
	ud-treebanks-v2.2/UD_Bulgarian-BTB/bg_btb-ud-dev.conllu

UD_SWE_TREEBANKS = \
	ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-train.conllu \
	ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-test.conllu \
	ud-treebanks-v2.2/UD_Swedish-Talbanken/sv_talbanken-ud-dev.conllu \
	ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-train.conllu \
	ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-test.conllu \
	ud-treebanks-v2.2/UD_Swedish-LinES/sv_lines-ud-dev.conllu \
	ud-treebanks-v2.2/UD_Swedish-PUD/sv_pud-ud-test.conllu

UD_FIN_TREEBANKS = \
	ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-dev.conllu \
	ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-test.conllu \
	ud-treebanks-v2.2/UD_Finnish-FTB/fi_ftb-ud-train.conllu \
	ud-treebanks-v2.2/UD_Finnish-PUD/fi_pud-ud-test.conllu \
	ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-dev.conllu \
	ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-test.conllu \
	ud-treebanks-v2.2/UD_Finnish-TDT/fi_tdt-ud-train.conllu


all: build_dirs Parse.pgf semantics.db build/SenseService
	@echo ""
	@echo "********************************************************************************"
	@echo "* Now kill the SenseService.fcgi process"
	@echo "********************************************************************************"
	@ssh www.grammaticalframework.org

Parse.pgf: $(GF_SOURCES) Parse.probs
	gf --make --probs=Parse.probs --gfo-dir=build/gfo ParseBul.gf ParseEng.gf ParseFin.gf ParsePor.gf ParseSwe.gf ParseAPI.gf
	scp Parse.pgf www.grammaticalframework.org:/usr/local/www/GF-demos/www/robust/Parse.pgf

build/Parse.noprobs.pgf: $(GF_SOURCES)
	gf --make -name=Parse.noprobs --gfo-dir=build/gfo --output-dir=build ParseBul.gf ParseEng.gf ParseFin.gf ParsePor.gf ParseSwe.gf

Parse.probs Parse.bigram.probs: build/udsenser build/Parse.noprobs.pgf examples.txt $(UD_BUL_TREEBANKS) $(UD_ENG_TREEBANKS) $(UD_FIN_TREEBANKS) $(UD_SWE_TREEBANKS)
	build/udsenser build/Parse.noprobs.pgf train abstract examples.txt , ParseBul $(UD_BUL_TREEBANKS) , ParseEng $(UD_ENG_TREEBANKS) , ParseFin $(UD_FIN_TREEBANKS) , ParseSwe $(UD_SWE_TREEBANKS)

build/udsenser: train/udsenser.hs train/GF2UED.hs build/train/EM.hs build/train/Matching.hs build/train/em_core.o
	ghc --make -odir build/train -hidir build/train -O2 $^ -o $@ -lpgf -lgu -lm

build/train/em_core.o: train/em_core.c train/em_core.h
	gcc -O2 -std=c99 -Itrain -c $< -o $@

build/train/EM.hs: train/EM.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

build/train/Matching.hs: train/Matching.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

semantics.db: sense-service/glosses.hs WordNet.gf examples.txt Parse.bigram.probs
	runghc -isense-service sense-service/glosses.hs
	scp semantics.db www.grammaticalframework.org:/home/krasimir/www/semantics.db

build/SenseService: sense-service/SenseService.hs sense-service/SenseSchema.hs
	ghc --make -odir build/sense-service -hidir build/sense-service -O2 $^ -o $@
	scp build/SenseService www.grammaticalframework.org:/home/krasimir/www/SenseService
	ssh www.grammaticalframework.org mv www/SenseService www/SenseService.fcgi

WordNet.gf: FORCE build/SenseService
	runghc check.hs - $(shell ssh www.grammaticalframework.org ./www/SenseService.fcgi report)

FORCE:

.PHONY: build_dirs

build_dirs:
	mkdir -p build
	mkdir -p build/gfo
	mkdir -p build/train
	mkdir -p build/sense-service
