LANGS = ParseAar ParseAbk ParseAce ParseAdy ParseAfr ParseAlt ParseAmh \
		ParseAng ParseAPI ParseAra ParseArc ParseArg ParseAry ParseArz \
		ParseAsm ParseAst ParseAva ParseBak ParseBam ParseBan ParseBar \
		ParseBcl ParseBel ParseBen ParseBod ParseBos ParseBre ParseBul \
		ParseCat ParseCeb ParseChe ParseChi ParseCho ParseChr ParseChu \
		ParseChv ParseCkb ParseCor ParseCos ParseCrh ParseCsb ParseCym \
		ParseCze ParseDan ParseDiv ParseDsb ParseDut ParseEng ParseEpo \
		ParseEst ParseEus ParseEwe ParseFao ParseFij ParseFre ParseFin \
		ParseFrp ParseFrr ParseFry ParseFur ParseGer ParseGla ParseGle \
		ParseGlg ParseGlv ParseGot ParseGrc ParseGre ParseGsw ParseGuj \
		ParseHat ParseHau ParseHaw ParseHeb ParseHin ParseHrv ParseHsb \
		ParseHun ParseHye ParseIce ParseIdo ParseIii ParseIku ParseIlo \
		ParseIna ParseInd ParseInh ParseIta ParseJam ParseJav ParseJbo \
		ParseJpn ParseKal ParseKan ParseKat ParseKaz ParseKbd ParseKhm \
		ParseKik ParseKin ParseKir ParseKor ParseKpv ParseKur ParseLad \
		ParseLao ParseLat ParseLav ParseLij ParseLim ParseLin ParseLit \
		ParseLld ParseLmo ParseLtg ParseLtz ParseLzz ParseMah ParseMal \
		ParseMar ParseMdf ParseMhr ParseMkd ParseMlg ParseMlt ParseMnw \
		ParseMon ParseMwl ParseMya ParseMyv ParseNap ParseNav ParseNds \
		ParseNep ParseNno ParseNor ParseNya ParseOci ParseOri ParseOss \
		ParsePam ParsePap ParsePes ParsePli ParsePms ParsePnb ParsePol \
		ParsePor ParsePus ParseQue ParseRoh ParseRon ParseRup ParseRus \
		ParseSah ParseSan ParseScn ParseSco ParseShn ParseSin ParseSlo \
		ParseSlv ParseSma ParseSme ParseSmn ParseSmo ParseSms ParseSna \
		ParseSnd ParseSom ParseSot ParseSpa ParseSqi ParseSrd ParseSrn \
		ParseSrp ParseStq ParseSun ParseSwa ParseSwe ParseSzl ParseTam \
		ParseTat ParseTel ParseTet ParseTgk ParseTgl ParseTha ParseTir \
		ParseTpi ParseTuk ParseTur ParseTyv ParseUdm ParseUig ParseUkr \
		ParseUrd ParseUzb ParseVec ParseVep ParseVie ParseVol ParseVro \
		ParseWar ParseWln ParseWol ParseXal ParseXho ParseXmf ParseYid \
		ParseYor ParseYue ParseZsm ParseZul


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

all: build_dirs Parse.pgf semantics.db build/www-services/WordNetServer

Parse.pgf: $(patsubst %, build/%.pgf, $(LANGS)) Parse.probs
	gf --make --probs=Parse.probs --boot -name=Parse $(patsubst %, build/%.pgf, $(LANGS))
	# mv Parse.ngf $(DOC_PATH)

build/Parse%.pgf: Parse%.gf Parse.gf WordNet%.gf WordNet.gf
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

semantics.db: build/glosses WordNet.gf $(patsubst Parse%, WordNet%.gf, $(LANGS)) examples.txt Parse.uncond.probs
	build/glosses $(LANGS)
	# mv semantics.db $(DOC_PATH)

build/glosses: www-services/glosses.hs www-services/SenseSchema.hs www-services/Interval.hs
	ghc --make -threaded -odir build/www-services -hidir build/www-services -O2 -iwww-services $^ -o $@

build/www-services/WordNetServer: www-services/Server.hs www-services/SenseService.hs www-services/SenseSchema.hs www-services/ContentService.hs www-services/ContentSchema.hs www-services/FunctionsService.hs www-services/PatternMatching.hs www-services/Interval.hs
	ghc --make -threaded -hidir build/www-services -O2 -optl-pthread $^ -o $@

morpho:
	$(foreach WORDNET,$(WORDNETS),mkdir -p $(DOC_PATH)/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET));\
	                              echo "$(shell sed '1s/concrete WordNet\(...\) of WordNet = Cat\(...\|Zero\) \*\* open\(.*\){/resource morpho = open Documentation\1,\3{}/;1q' <$(WORDNET))" | \
	                              tee $(DOC_PATH)/tmp/$(patsubst WordNet%.gf,morpho-%,$(WORDNET))/morpho.gf;)

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
