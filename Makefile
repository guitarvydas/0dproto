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
	bred/bred.bash message.bred container.u0d bred >container.out
	bred/bred.bash outputport.bred container.out bred >container.0d

leaf.0d : leaf.u0d
	bred/bred-transpile.bash message.bred bred <leaf.u0d >leaf.0d

identity:
	@echo
	python3 repl_connection.py <test.u0d >test.identity
	bred/bred0.bash downdirection.bred test.outA bred
	@echo

pattern:
	@echo
	python3 repl_connection.py <test.u0d >test.pattern
	bred/bred1.bash downdirection.bred test.outA bred
	@echo

fabricator:
	@echo
	python3 repl_connection.py <test.u0d >test.fabricator
	bred/bred2.bash downdirection.bred test.outA bred
	@echo

test.0dA : test.u0d
	@echo
	python3 repl_connection.py <test.u0d >test.0dA

test.0dB : test.0dA
	@echo
	bred/bred-transpile.bash downdirection.bred bred <test.0dA >test.0dB
	cp test.0dB test.0d
	@echo

test.0dC : test.0dB
	@echo
	bred/bred-transpile.bash updirection.bred bred <test.0dB >test.0dC
	cp test.0dC test.0d
	@echo

test.0dD : test.0dC
	@echo
	bred/bred-transpile.bash passthrough.bred bred <test.0dC >test.0dD
	cp test.0dD test.0d
	@echo

test.0dE : test.0dD
	@echo
	bred/bred-transpile.bash shortmessage.bred bred <test.0dD >test.0dE
	cp test.0dE test.0d
	@echo

test.0dF : test.0dE
	@echo
	bred/bred-transpile.bash message.bred bred <test.0dE >test.0dF
	cp test.0dF test.0d
	@echo

test.0d0 : test.0dF

test.0d : test.u0d
	python3 repl_connection.py <test.u0d >test.0dA
	bred/bred-transpile.bash downdirection.bred bred <test.0dA >test.0dB
	bred/bred-transpile.bash updirection.bred bred <test.0dB >test.0dC
	bred/bred-transpile.bash passthrough.bred bred <test.0dC >test.0dD
	bred/bred-transpile.bash shortmessage.bred bred <test.0dD >test.0dE
	bred/bred-transpile.bash message.bred bred <test.0dE >test.0dF
	bred/bred-transpile.bash message.bred bred <test.0dE >test.0dF
	bred/bred-transpile.bash outputport.bred bred <test.0dF >test.0dG
	bred/bred-transpile.bash inputport.bred bred <test.0dG >test.0dH
	cp test.0dH test.0d
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
