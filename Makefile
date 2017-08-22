.PHONY: swipl
swipl:;swipl -O --goal=main --stand_alone=true -o juicy -c juicy_swipl.pl

.PHONY: gprolog
gprolog:;gplc juicy_gprolog.pl -o juicy
