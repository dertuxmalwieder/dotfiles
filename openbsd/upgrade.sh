#!/bin/sh
HOST=ftp.eu.openbsd.org
USER=anonymous
PASS=whatever
PLATFORM=i386
DOWNLOADDIR=/home/tux/upgrade


# create the download directory if it doesn't exist yet
if [ ! -d $DOWNLOADDIR ]; then
  mkdir -p $DOWNLOADDIR
fi

# cd into the download directory
cd $DOWNLOADDIR

# clean up
rm *

# download all new packages
ftp -n -V $HOST << EOT
user $USER $PASS
prompt
cd pub/OpenBSD/snapshots/$PLATFORM
mget SHA*
mget bsd*
mget *.tgz
bye
EOT

# replace the kernel
rm /obsd ; ln /bsd /obsd && cp bsd.mp /nbsd && mv /nbsd /bsd
cp bsd.rd /
cp bsd /bsd.sp

# extract all packages
tar -C / -xzphf xserv*
tar -C / -xzphf xfont*
tar -C / -xzphf xshare*
tar -C / -xzphf xbase*
tar -C / -xzphf game*
tar -C / -xzphf comp*
tar -C / -xzphf man*
# base needs to be last
tar -C / -xzphf base*

# reboot
reboot
