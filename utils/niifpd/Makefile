all:
	python /usr/share/doc/python2.7/examples/Tools/freeze/freeze.py -X pydoc -o build niifpd.py > /dev/null
	make -C build all
	echo "Built at build/niifpd"

