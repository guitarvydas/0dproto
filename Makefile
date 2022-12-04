here=`pwd`

all: container.js leaf.0d test.0d

dev:
	rm -f smalltest.lisp
	(make smalltest.lisp)

full: install repos container.0d leaf.0d test.0d

install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako
	npm install cli
	npm install js-beautify

test.0d : test.u0d
	./transpile.bash test

smalltest.0d : smalltest.u0d
	./transpile.bash smalltest

leaf.0d : leaf.u0d
	./transpile.bash leaf

container.0d : container.u0d
	./transpile.bash container


clean:
	rm -f *~
	rm -f container.0d
	rm -f *.0dA *.0dB *.0dC *.0dD1 *.0dD *.0dE *.0dF *.0dG *.0dH
	rm -f pattern.ohm pattern.fab

identity:
	./fab/fab - ZeroD 0d.ohm identity.fab --support='./jssupport.js' <container.0d | sed -e '/^$$/d'

container.js: container.0d
	./fab/fab - ZeroD 0d.ohm js.fab --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
	cat container.js

leaf.js: leaf.0d
	./fab/fab - jsZeroD 0d.ohm js.fab --support='./jssupport.js' <leaf.0d >temp.0d
	python3 repl_cleanup.py <temp.0d >leaf.js
	cat leaf.js

leaf.lisp: leaf.0d
	echo >temp.lisp
	#./fab/fab - clZeroD 0d.ohm clclasses.fab --support='./jssupport.js' <leaf.0d >>temp.lisp
	./fab/fab - clZeroD 0d.ohm cl.fab --support='./jssupport.js' <leaf.0d >>temp.lisp
	python3 repl_cleanup.py <temp.lisp >leaf.lisp
	cat leaf.lisp

smalltest.lisp: smalltest.0d
	echo >temp.lisp
	#./fab/fab - clZeroD 0d.ohm clclasses.fab --support='./jssupport.js' <smalltest.0d >>temp.lisp
	./fab/fab - clZeroD 0d.ohm cl.fab --support='./jssupport.js' <smalltest.0d >>temp.lisp
	python3 repl_cleanup.py <temp.lisp >smalltest.lisp
	cat smalltest.lisp

# container.js: container.0d
# 	./fab/fab - ZeroD 0d.ohm js0d.fmt --support='./jssupport.js' <container.0d | sed -e '/^$$/d' >container.js
# 	~/node_modules/js-beautify/js/bin/js-beautify.js container.js
