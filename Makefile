all: afollari

afollari: afollari.gpr afollari.adb
	gprbuild -Pafollari.gpr

clean:
	rm -f *.o afollari
