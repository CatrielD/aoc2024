.PHONY: all

.PHONY: clean
.PHONY: make-clean

# all: the_solver.o day7.o
all: day7

%: %.m
	mmc -E -O0 --make $@
#	mmc -E --make $@  #<---- this one fails for part2 !!!
#	mmc -O6 --intermodule-optimization --make $@
#	mmc -E --make $@  #<---- this one fails for part2 !!!
#                      default optimization O2 ¿UB?
clean:
	rm day7.c day7.c_date day7.d day7.mh day7.o day7_init.c day7_init.o day7.err output || true

clean-all: clean make-clean
	rm day7 day7.err day7.mh || true

make-clean:
	mmc --make day.realclean
	/bin/rm -rf Mercury || true

