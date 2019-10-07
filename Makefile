all:
	$(MAKE) check_LF
	$(MAKE) check_QC
	$(MAKE) clean

check_LF:
	./scripts/lf.sh

check_QC:
	./scripts/qc.sh

clean:
	rm LF/*.vo
	rm LF/*.glob
	rm QC/*.vo
	rm QC/*.glob

