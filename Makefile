LANGS = ParseAfr ParseBul ParseCat ParseChi ParseDut ParseEng ParseEst ParseFin ParseFre ParseGer ParseIta ParseKor ParseMlt ParsePol ParsePor ParseSlv ParseSom ParseSpa ParseSwa ParseSwe ParseTha ParseTur ParseAPI

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

SERVER_PATH = /usr/local/www/gf-wordnet

ifndef GF_LIB_PATH
INSTALL_PATH=$(shell cat ../gf-core/DATA_DIR)/lib
else
INSTALL_PATH=$(GF_LIB_PATH)/lib
endif

all: build_dirs Parse.pgf semantics.db build/SenseService build/ContentService build/gfshell
ifneq ($(SERVER), NO)
	ssh -t www.grammaticalframework.org "sudo pkill -e SenseService.*; sudo pkill -e ContentService.*"
endif

Parse.pgf: $(patsubst %, build/%.pgf, $(LANGS)) Parse.probs
	gf --make --probs=Parse.probs $(patsubst %, build/%.pgf, $(LANGS))
ifneq ($(SERVER), NO)
	scp Parse.pgf www.grammaticalframework.org:/usr/local/www/GF-demos/www/robust/Parse.pgf
	scp -p build/gfo/WordNet*.gfo www.grammaticalframework.org:/usr/local/www/gf-wordnet
	ssh -t www.grammaticalframework.org sudo mv /usr/local/www/gf-wordnet/WordNet*.gfo /usr/share/x86_64-linux-ghc-7.10.3/gf-3.10.4/lib
endif

build/gfo/WordNet.gfo:

build/gfo/WordNet%.gfo: WordNet%.gf WordNet.gf
	gf --batch --gfo-dir=build/gfo --no-pmcfg $<

build/Parse%.pgf: Parse%.gf Parse.gf build/gfo/WordNet%.gfo build/gfo/WordNet.gfo
	gf --make -name=$(basename $(@F)) --gfo-dir=build/gfo --output-dir=build $<

Parse.probs Parse.uncond.probs: train/statistics.hs examples.txt build/ParseAPI.pgf
	runghc $^

build/udsenser: train/udsenser.hs train/GF2UED.hs build/train/EM.hs build/train/Matching.hs build/train/em_core.o build/train/em_data_stream.o
	ghc --make -odir build/train -hidir build/train -O2 $^ -o $@ -lpgf -lgu -lm -llzma -lpthread

build/train/em_core.o: train/em_core.c train/em_core.h train/em_data_stream.h
	gcc -O2 -std=c99 -Itrain -c $< -o $@

build/train/em_data_stream.o: train/em_data_stream.c train/em_data_stream.h
	gcc -O2 -std=c99 -Itrain -c $< -o $@

build/train/EM.hs: train/EM.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

build/train/Matching.hs: train/Matching.hsc train/em_core.h
	hsc2hs --cflag="-std=c99" -Itrain $< -o $@

semantics.db: build/glosses WordNet.gf $(patsubst Parse%, WordNet%.gf, $(LANGS)) examples.txt Parse.uncond.probs images.txt
	build/glosses
ifneq ($(SERVER), NO)
	scp semantics.db www.grammaticalframework.org:$(SERVER_PATH)
	scp build/status.svg www.grammaticalframework.org:$(SERVER_PATH)/www
endif

build/glosses: www-services/glosses.hs www-services/SenseSchema.hs www-services/Interval.hs
	ghc --make -odir build/www-services -hidir build/www-services -O2 -iwww-services $^ -o $@

build/SenseService: www-services/SenseService.hs www-services/SenseSchema.hs www-services/URLEncoding.hs www-services/PatternMatching.hs www-services/Interval.hs
	ghc --make -odir build/www-services -hidir build/www-services -DSERVER_PATH="\"$(SERVER_PATH)\"" -O2 -optl-pthread $^ -o $@
ifneq ($(SERVER), NO)
	rm -f $(SERVER_PATH)/www/SenseService.fcgi
	cp build/SenseService $(SERVER_PATH)/www/SenseService.fcgi
endif

build/ContentService: www-services/ContentService.hs www-services/SenseSchema.hs www-services/ContentSchema.hs www-services/URLEncoding.hs www-services/Interval.hs
	ghc --make -odir build/www-services -hidir build/www-services -DSERVER_PATH="\"$(SERVER_PATH)\"" -O2 -optl-pthread $^ -o $@
ifneq ($(SERVER), NO)
	rm -f $(SERVER_PATH)/www/ContentService
	cp build/ContentService $(SERVER_PATH)/www/ContentService
endif

build/gfshell: $(WORDNETS)
ifneq ($(SERVER), NO)
	ssh -t www.grammaticalframework.org \
	    "$(foreach WORDNET,$(WORDNETS),sudo mkdir -p /usr/local/www/GF-overlay/src/www/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET));\
	                                   echo \"$(shell sed '1s/concrete WordNet\(...\) of WordNet = Cat... \*\* open\(.*\){/resource morpho = open Documentation\1,\2{}/;1q' <$(WORDNET))\" | \
	                                   sudo tee /usr/local/www/GF-overlay/src/www/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET))/morpho.gf;\
	                                   sudo chown gf-cloud /usr/local/www/GF-overlay/src/www/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET)) \
	                                                       /usr/local/www/GF-overlay/src/www/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET))/morpho.gf;)"
	touch $@
endif

.SECONDARY:

.PHONY: build_dirs

build_dirs:
	mkdir -p build
	mkdir -p build/gfo
	mkdir -p build/train
	mkdir -p build/www-services


install: $(patsubst %.gf,build/gfo/%.gfo,$(WORDNETS))
	mkdir -p $(INSTALL_PATH)
	install build/gfo/WordNet*.gfo     $(INSTALL_PATH)
