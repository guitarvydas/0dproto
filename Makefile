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
	./fab/fab - PseudoCode pc.ohm jspc.fmt --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
	~/node_modules/js-beautify/js/bin/js-beautify.js container.js

dev:
	./fab/fab - PseudoCode pc.ohm jspc.fmt --support='./jssupport.js' <test0.0d | sed -e '/^$$/d' >test0.js
	~/node_modules/js-beautify/js/bin/js-beautify.js test0.js


clean:
	rm *~
