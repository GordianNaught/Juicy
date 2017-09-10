.PHONY: swipl
swipl:;swipl -O --goal=main --stand_alone=true -o juicy -c juicy_swipl.pl && ./runTests.plx

.PHONY: gprolog
gprolog:;gplc juicy_gprolog.pl -o juicy && ./runTests.plx
