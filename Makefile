all:
	$(MAKE) check
	$(MAKE) clean

check:
	coqc V1-LF/*.v

clean:
	rm V1-LF/*.vo
	rm V1-LF/*.glob

