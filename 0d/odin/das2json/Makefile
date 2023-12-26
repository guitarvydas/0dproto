ODIN_FLAGS ?= -debug -o:none
SYNTAX=syntax/*.odin

das2json: *.odin $(SYNTAX)
	@echo 'building...'
	odin build . $(ODIN_FLAGS)

clean:
	rm -rf das2json *.bin *.dSYM *.drawio.json

