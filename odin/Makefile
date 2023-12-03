.PHONY: run check build vsh

SRC=../src
ODIN_FLAGS ?= -debug -o:none
0D=process/*.odin 0d/*.odin leaf0d/*.odin debug/*.odin process/*.odin
D2J=../das2json/das2json

run: basics drawio vsh dev0d agency

basics: demo_basics.bin
	@echo 'running...'
	./demo_basics.bin

drawio: demo_drawio.bin $(D2J) $(SRC)/demo_drawio.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_drawio.drawio
	./demo_drawio.bin $(SRC)/demo_drawio.drawio

vsh: demo_vsh.bin $(D2J) $(SRC)/demo_vsh.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_vsh.drawio
	./demo_vsh.bin $(SRC)/demo_vsh.drawio

agency: demo_agency.bin $(D2J) $(SRC)/demo_agency.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_agency.drawio
	./demo_agency.bin $(SRC)/demo_agency.drawio

dev0d: demo_dev0d.bin $(D2J) $(SRC)/demo_dev0d.drawio
	@echo 'running...'
	$(D2J) $(SRC)/demo_dev0d.drawio
	./demo_dev0d.bin $(SRC)/demo_dev0d.drawio

das2json: ../das2json/das2json
	@echo 'building...'
	(cd ../das2json ; make)

demo_basics.bin: demo_basics/*.odin $(0D)
	@echo 'building...'
	odin build demo_basics $(ODIN_FLAGS)

demo_drawio.bin: demo_drawio/*.odin $(0D)
	@echo 'building...'
	odin build demo_drawio $(ODIN_FLAGS)

demo_vsh.bin: demo_vsh/*.odin $(0D)
	@echo 'building...'
	odin build demo_vsh $(ODIN_FLAGS)

demo_agency.bin: demo_agency/*.odin $(0D) ../llm/agency/main
	@echo 'building...'
	odin build demo_agency $(ODIN_FLAGS)

demo_dev0d.bin: demo_dev0d/*.odin $(0D)
	@echo 'building...'
	odin build demo_dev0d $(ODIN_FLAGS)

../llm/agency/main:
	(cd ../llm/agency/main ; make)

clean:
	rm -f *.bin
	rm -rf *.dSYM
