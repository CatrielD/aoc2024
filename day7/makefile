.PHONY: all

# all: the_solver.o day7.o
all: day7

%: %.m
	mmc -E --make $@
#	mmc -O4 --intermodule-optimization $<

# clean:
#	rm day7.c day7.c_date day7.d day7.mh day7.o day7_init.c day7_init.o day7.err

clean-all: clean
	rm day7 day7.err day7.mh

.PHONY: clean
clean:
	mmc --make day.realclean
	/bin/rm -rf Mercury

