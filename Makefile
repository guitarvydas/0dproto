all: repos js


install: repos npmstuff

repos:
	multigit -r

npmstuff:
	npm install ohm-js yargs atob pako

js:
	./fab/fab - PseudoCode pc.ohm jspc.fmt <container.0d

clean:
	rm *~
