all:
	$(MAKE) check_LF
	$(MAKE) clean

check_LF:
	coqc -Q LF/ LF LF/*.v

clean:
	rm LF/*.vo
	rm LF/*.glob

