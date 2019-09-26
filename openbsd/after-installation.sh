#!/bin/csh
set DOWNLOADDIR=/home/tux/upgrade

if ( -d $DOWNLOADDIR ) then

cd $DOWNLOADDIR

# upgrade config
sysmerge

# update all packages
pkg_add -u

# cleanup
pkg_delete -a

else

echo "FAILED TO FIND $DOWNLOADDIR"

endif
