ODIN_FLAGS ?= -debug -o:none
DAS2JSON=syntax/*.odin

all: das2json
	@echo 'running...'
	./das2json ../src/d2jexample.drawio

das2json: *.odin $(DAS2JSON)
	@echo 'building...'
	odin build . $(ODIN_FLAGS)

clean:
	rm -rf das2json das2json.bin das2json.bin.dSYM /tmp/*.drawio.json

