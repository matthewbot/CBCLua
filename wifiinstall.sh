#!/bin/sh

echo -n "CBC IP Address: "
read IP

tar -czf - cbclua/ --exclude=".*" --exclude="*~" -h -p | ssh root@${IP} "rm -rf /mnt/user/code/cbclua; tar -C /mnt/user/code -zxf - -p"

