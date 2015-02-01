SRC=00.c 01.c 02.c 03.c 04.c 05.c 06.c 07.c 08.c 10.c  12.c  14.c  16.c  18.c  09.c  11.c  13.c  15.c  17.c  19.c

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