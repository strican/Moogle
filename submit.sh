#!/bin/bash -e

if [ x$1 = x ] 
then
    echo "Need to specify assignment number"
    exit
fi

if test -f /usr/local/bin/submit; then
	rm -rf moogle-submit
	mkdir moogle-submit
	cp *.ml moogle-submit
	cp *.pdf moogle-submit
	cp *.html moogle-submit
	/usr/local/bin/submit lib51 $1 `pwd`/moogle-submit
	echo "Done!"
else
    echo "You must run this on one of the nice.fas machines"
fi
