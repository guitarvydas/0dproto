.PHONY: run check build vsh

SRC=src
ODIN_FLAGS ?= -debug -o:none
0D=0d/odin/0d/*.odin 0d/odin/std/*.odin
D2JDIR=0d/odin/das2json
D2J=$(D2JDIR)/das2json
DEMO=demo

run: basics drawio vsh dev0d hello_world agency abcjs arithmetic

basics:  $(D2J)
	odin build $(DEMO)/demo_basics $(ODIN_FLAGS)
	./demo_basics

hello_world: $(D2J) $(SRC)/demo_hello_world.drawio
	$(D2J) $(SRC)/demo_hello_world.drawio
	odin build $(DEMO)/demo_hello_world $(ODIN_FLAGS)
	./demo_hello_world $(SRC)/demo_hello_world.drawio

drawio: $(D2J) $(SRC)/demo_drawio.drawio
	odin build  $(DEMO)/demo_drawio $(ODIN_FLAGS)
	$(D2J) $(SRC)/demo_drawio.drawio
	./demo_drawio $(SRC)/demo_drawio.drawio

vsh: $(D2J) $(SRC)/demo_vsh.drawio
	$(D2J) $(SRC)/demo_vsh.drawio
	odin build  $(DEMO)/demo_vsh $(ODIN_FLAGS)
	./demo_vsh $(SRC)/demo_vsh.drawio

dev0d: $(D2J) $(SRC)/demo_dev0d.drawio
	$(D2J) $(SRC)/demo_dev0d.drawio
	odin build  $(DEMO)/demo_dev0d $(ODIN_FLAGS)
	./demo_dev0d $(SRC)/demo_dev0d.drawio

agency: $(D2J) $(SRC)/demo_agency.drawio ../llm/agency/main
	$(D2J) $(SRC)/demo_agency.drawio
	odin build  $(DEMO)/demo_agency $(ODIN_FLAGS)
	./demo_agency $(SRC)/demo_agency.drawio

abcjs: $(D2J) $(SRC)/demo_abcjs.drawio
	$(D2J) $(SRC)/demo_abcjs.drawio
	odin build  $(DEMO)/demo_abcjs $(ODIN_FLAGS)
	./demo_abcjs $(SRC)/demo_abcjs.drawio

arithmetic: $(D2J) $(SRC)/demo_arith.drawio
	$(D2J) $(SRC)/demo_arith.drawio
	odin build  $(DEMO)/demo_arith $(ODIN_FLAGS)
	./demo_arith $(SRC)/demo_arith.drawio


$(D2J):
	@(cd $(D2JDIR) ; make -s)

../llm/agency/main:
	@(cd llm/agency ; make)

clean:
	rm -rf *.bin demo_*
	(cd $(D2JDIR) ; make clean)

