make_opts ::= -E -O6 \
	--no-fully-strict --enable-term --inline-linear-tail-rec-sccs --constraint-propagation --optimize-duplicate-calls --introduce-accumulators --deforestation --everything-in-one-c-function \
	--runtime-flags "--nondetstack-size 640"

#--stack-segments -> necesita
#mercury_compile: error: the Mercury standard library cannot be found in grade
#`asm_fast.gc.stseg'.

.PHONY: all

.PHONY: clean
.PHONY: make-clean

all: day7

%: %.m makefile
	mmc $(make_opts) --make $@

clean:
	rm day7.c day7.c_date day7.d day7.mh day7.o day7_init.c day7_init.o day7.err output || true

clean-all: clean make-clean
	rm day7 day7.err day7.mh || true

make-clean:
	mmc --make day.realclean
	/bin/rm -rf Mercury || true

