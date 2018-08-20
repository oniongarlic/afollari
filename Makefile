all: afollari

afollari: afollari.gpr afollari.adb
	gnatmake -Pafollari.gpr

clean:
	rm -f *.o afollari
