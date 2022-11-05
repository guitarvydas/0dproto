here=`pwd`
#support=/Users/tarvydas/quicklisp/local-projects/0d/jssupport.js
#support=$(here)/jssupport.js
support=jssupport.js

all: repos js

install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify

js:
	./fab/fab - PseudoCode pc.ohm jspc.fmt --support=/Users/tarvydas/quicklisp/local-projects/0d/jssupport.js <container.0d | sed -e '/^$$/d' >container.js
	~/node_modules/js-beautify/js/bin/js-beautify.js container.js

dev:
	./fab/fab - PseudoCode pc.ohm jspc.fmt --support='./jssupport.js' <container.0d

clean:
	rm *~
