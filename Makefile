here=`pwd`

all: install repos container.js

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

dev: devtest

dev0:
	(make install)
	bred/bred.bash message.bred container.u0d bred

devcontainer:
	(make install)
	touch container.u0d
	(make container.0d)
	cat container.0d

devtest:
	touch test.u0d
	(make test.0d)
	cat test.0d

container.0d : container.u0d
	bred/bred.bash message.bred container.u0d bred >/tmp/container.out
	bred/bred.bash outputport.bred /tmp/container.out bred >container.0d

leaf.0d : leaf.u0d
	bred/bred.bash message.bred leaf.u0d bred >/tmp/leaf.out
	bred/bred.bash outputport.bred /tmp/leaf.out bred >leaf.0d

identity:
	@echo
	python3 repl_connection.py <test.u0d >/tmp/test.outA
	bred/bred0.bash downdirection.bred /tmp/test.outA bred
	@echo

pattern:
	@echo
	python3 repl_connection.py <test.u0d >/tmp/test.outA
	bred/bred1.bash downdirection.bred /tmp/test.outA bred
	@echo

fabricator:
	@echo
	python3 repl_connection.py <test.u0d >/tmp/test.outA
	bred/bred2.bash downdirection.bred /tmp/test.outA bred
	@echo

test.0d : test.u0d
	@echo
	python3 repl_connection.py <test.u0d >/tmp/test.outA
	bred/bred.bash downdirection.bred /tmp/test.outA bred >/tmp/test.outAa
	# bred/bred.bash updirection.bred /tmp/test.outAa bred >/tmp/test.outAb
	cp /tmp/test.outAa test.0d
	@echo

Junk:
	# bred/bred.bash senderreceiver.bred test.u0d bred >/tmp/test.outA
	# bred/bred.bash connection.bred /tmp/test.outA bred >/tmp/test.out0
	# bred/bred.bash outputport.bred /tmp/test.out0 bred >/tmp/test.out1
	# bred/bred.bash inputport.bred /tmp/test.out1 bred >/tmp/test.out2
	# bred/bred.bash direction.bred /tmp/test.out2 bred >/tmp/test.out3
	# bred/bred.bash message.bred /tmp/test.out3 bred >test.0d

clean:
	rm *~
	rm container.0d
