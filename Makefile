
alga-bench.ss : build-report.ss *.json
	echo "(main)" | scheme -q $<

