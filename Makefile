here=`pwd`

all: repos js

install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify

js: container.0d
	./fab/fab - ZeroD 0d.ohm js0d.fmt --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
	~/node_modules/js-beautify/js/bin/js-beautify.js container.js

dev:
	touch container.u0d
	(make)

container.0d : container.u0d
	bred/bred.bash message.bred container.u0d >container.0d

clean:
	rm *~
	rm container.0d
