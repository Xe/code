#!/bin/bash

USERAGENT=`bash make-useragent.sh`

function parseImage {
	if wget -U "$USERAGENT" `python get-url.py $1.json` -nc -P images
	then
		sleep 5
	fi
}

if [[ -d images ]]
then
	mkdir images
fi

for i in {75311..406928}
do

	wget -U "$USERAGENT" "http://derpiboo.ru/$i.json?nocomments=true&nofav=true?key=p88QprcPpyw3qzu7uHup" -O $i.json
	parseImage $i
	rm "$i.json"
	sleep 0.5
done

