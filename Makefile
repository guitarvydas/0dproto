here=`pwd`

all: repos js

install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify

js:
	./fab/fab - PseudoCode 0d.ohm js0d.fmt --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
	~/node_modules/js-beautify/js/bin/js-beautify.js container.js

dev:
	(cd fab ; make dev)
	./fab/fab - PseudoCode 0d.ohm js0d.fmt --support='./jssupport.js' <test0.0d | sed -e '/^$$/d' >test0.js
	~/node_modules/js-beautify/js/bin/js-beautify.js test0.js


clean:
	rm *~
