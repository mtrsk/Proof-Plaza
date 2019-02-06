all:
	$(MAKE) check_LF
	$(MAKE) clean

check_LF:
	./scripts/lf.sh

.ONESHELL:

clean:
	rm LF/*.vo
	rm LF/*.glob

