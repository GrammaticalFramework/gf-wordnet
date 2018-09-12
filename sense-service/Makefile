all: semantics.db SenseService
	rsync semantics.db www.grammaticalframework.org:/home/krasimir/www/semantics.db
	rsync ../Parse.pgf www.grammaticalframework.org:/usr/local/www/GF-demos/www/robust/Parse.pgf
	rsync SenseService www.grammaticalframework.org:/home/krasimir/www/SenseService
	ssh www.grammaticalframework.org mv www/SenseService www/SenseService.fcgi
	@echo ""
	@echo "********************************************************************************"
	@echo "* Now kill the SenseService.fcgi process"
	@echo "********************************************************************************"
	@ssh www.grammaticalframework.org

semantics.db: ../WordNet.gf
	runghc glosses.hs

SenseService: SenseService.hs SenseSchema.hs
	ghc --make -O2 SenseService.hs
