here=`pwd`

all: container.0d leaf.0d test.0d

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

leaf.0d : leaf.u0d
	./transpile.bash leaf

container.0d : container.u0d
	./transpile.bash container


clean:
	rm -f *~
	rm -f container.0d
	rm -f *.0dA *.0dB *.0dC *.0dD1 *.0dD *.0dE *.0dF *.0dG *.0dH
