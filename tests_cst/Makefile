SRC=00.c 01.c 02.c 03.c 

NEWSPEAK=../newspeak-1.7/bin/c2newspeak
SIMPLEAI=../newspeak-1.7/bin/simpleai

all: $(SRC:.c=.npk)

%.npk:%.c
	$(NEWSPEAK) $< -o $@

%:%.npk
	cd ../newspeak-1.7 && $(MAKE)
	$(SIMPLEAI) $<

clean:
	rm *.npk *.no