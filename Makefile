GANTT_CHART=stack exec gantt-chart --
all: test

build:
	stack build

test: build
	$(GANTT_CHART) monthly.gantt
	$(GANTT_CHART) -o daily.pdf daily.gantt
	$(GANTT_CHART) -o monthly.pdf monthly.gantt


%.tex: %.gantt templates/gantt.st
	$(GANTT_CHART) $< > $@

test-%: build
	$(GANTT_CHART) -v -o $(*).pdf $(*).gantt
	evince $(*).pdf


clean:
	rm -f *.hi *.o *.pdf

gantt: Parse.hs
