here=`pwd`

all: repos container.js

install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify

container.js: container.0d
	./fab/fab - ZeroD 0d.ohm js0d.fmt --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
	~/node_modules/js-beautify/js/bin/js-beautify.js container.js

dev:
	touch test.u0d
	(make test.0d)

container.0d : container.u0d
	bred/bred.bash message.bred container.u0d >/tmp/container.out
	bred/bred.bash outputport.bred /tmp/container.out >container.0d

leaf.0d : leaf.u0d
	bred/bred.bash message.bred leaf.u0d >/tmp/leaf.out
	bred/bred.bash outputport.bred /tmp/leaf.out >leaf.0d

test.0d : test.u0d
	bred/bred.bash connection.bred test.u0d >/tmp/test.out0
	bred/bred.bash outputport.bred /tmp/test.out0 >/tmp/test.out1
	bred/bred.bash inputport.bred /tmp/test.out1 >/tmp/test.out2
	bred/bred.bash direction.bred /tmp/test.out2 >/tmp/test.out3
	bred/bred.bash message.bred /tmp/test.out3 >test.0d

clean:
	rm *~
	rm container.0d
