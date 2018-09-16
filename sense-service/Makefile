all: semantics.db ../Parse.pgf SenseService
	rsync ../Parse.pgf www.grammaticalframework.org:/usr/local/www/GF-demos/www/robust/Parse.pgf
	@echo ""
	@echo "********************************************************************************"
	@echo "* Now kill the SenseService.fcgi process"
	@echo "********************************************************************************"
	@ssh www.grammaticalframework.org

../WordNet.gf: FORCE SenseService
	(cd ..; runghc check.hs - $(shell ssh www.grammaticalframework.org ./www/SenseService.fcgi report))

../Parse.pgf: ../Parse.gf ../ParseEng.gf ../ParseSwe.gf ../ParseBul.gf ../ParseExtend.gf ../ParseExtendEng.gf ../ParseExtendSwe.gf ../ParseExtendBul.gf ../WordNet.gf ../WordNetEng.gf ../WordNetSwe.gf ../WordNetBul.gf
	(cd ..; gf --make ParseEng.gf ParseSwe.gf ParseBul.gf)

semantics.db: ../WordNet.gf ../examples.txt
	runghc glosses.hs
	rsync semantics.db www.grammaticalframework.org:/home/krasimir/www/semantics.db

SenseService: SenseService.hs SenseSchema.hs
	ghc --make -O2 SenseService.hs
	rsync SenseService www.grammaticalframework.org:/home/krasimir/www/SenseService
	ssh www.grammaticalframework.org mv www/SenseService www/SenseService.fcgi

FORCE:
	
