.PHONY: run check build vsh

SRC=src
ODIN_FLAGS ?= -debug -o:none
0D=0d/odin/0d/*.odin 0d/odin/std/*.odin
D2JDIR=0d/odin/das2json
D2J=$(D2JDIR)/das2json
DEMO=demo

run: hello_world basics drawio vsh dev0d agency abcjs

basics: demo_basics
	@echo 'running...'
	./demo_basics

hello_world: demo_hello_world $(D2J) $(SRC)/demo_hello_world.drawio
	$(D2J) $(SRC)/demo_hello_world.drawio
	odin build $(DEMO)/demo_hello_world $(ODIN_FLAGS)
	./demo_hello_world $(SRC)/demo_hello_world.drawio

drawio: demo_drawio $(D2J) $(SRC)/demo_drawio.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_drawio.drawio
	./demo_drawio $(SRC)/demo_drawio.drawio

vsh: demo_vsh $(D2J) $(SRC)/demo_vsh.drawio
	@echo '*** Compiling to JSON...'
	$(D2J) $(SRC)/demo_vsh.drawio
	@echo '*** running...'
	./demo_vsh $(SRC)/demo_vsh.drawio

dev0d: demo_dev0d $(D2J) $(SRC)/demo_dev0d.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_dev0d.drawio
	./demo_dev0d $(SRC)/demo_dev0d.drawio

agency: demo_agency $(D2J) $(SRC)/demo_agency.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_agency.drawio
	./demo_agency $(SRC)/demo_agency.drawio

abcjs: demo_abcjs $(D2J) $(SRC)/demo_abcjs.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_abcjs.drawio
	./demo_abcjs $(SRC)/demo_abcjs.drawio


$(D2J):
	(cd $(D2JDIR) ; make -s)


demo_hello_world: $(DEMO)/demo_hello_world/*.odin $(0D)
	@echo 'building...'
	odin build $(DEMO)/demo_hello_world $(ODIN_FLAGS)

demo_basics: $(DEMO)/demo_basics/*.odin $(0D)
	@echo 'building...'
	odin build $(DEMO)/demo_basics $(ODIN_FLAGS)

demo_drawio: #(DEMO)/demo_drawio/*.odin $(0D)
	@echo 'building...'
	odin build  $(DEMO)/demo_drawio $(ODIN_FLAGS)

demo_vsh: $(DEMO)/demo_vsh/*.odin $(0D)
	@echo 'building...'
	odin build  $(DEMO)/demo_vsh $(ODIN_FLAGS)

demo_dev0d: $(DEMO)/demo_dev0d/*.odin $(0D)
	@echo 'building...'
	odin build  $(DEMO)/demo_dev0d $(ODIN_FLAGS)

demo_agency: $(DEMO)/demo_agency/*.odin $(0D) ../llm/agency/main
	@echo 'building...'
	odin build  $(DEMO)/demo_agency $(ODIN_FLAGS)

demo_abcjs: $(DEMO)/demo_abcjs/*.odin $(0D)
	@echo 'building...'
	odin build  $(DEMO)/demo_abcjs $(ODIN_FLAGS)

../llm/agency/main:
	(cd llm/agency ; make)

clean:
	rm -rf *.bin demo_*
	(cd $(D2JDIR) ; make clean)

