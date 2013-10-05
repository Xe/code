DIR=~/backup

if [ -d ~/bup ]; then
    DIR=~/bup
fi

rsync -avz -e ssh znc@volantis.yolo-swag.com:/home/znc/.znc/users/shadowh511/moddata/log $DIR

