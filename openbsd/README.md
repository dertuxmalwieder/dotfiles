# OpenBSD scripts

A number of scripts I use to keep my local installation of [OpenBSD](http://www.openbsd.org) up and running.

**Note that they'll mostly require root privileges!**

## Think_securely.png

My default OpenBSD wallpaper. Use it if you like it. Or don't.

## upgrade.sh

My sysupdate script as a "live" alternative to the recommended process via `bsd.rd`: Fetches all new packages, replaces them (and the kernel) and reboots, so the steps needed to update my OpenBSD -CURRENT are as follows:

1. `./upgrade.sh`
2. (reboot)
3. execute `after-installation.sh`

## after-installation.sh

To be run immediately after the reboot. This script uses `sysmerge` to merge your configuration files, then updates your packages and cleans up old 
dependencies.

## auto-wifi.sh

Some kind of a WiFi auto-connect script for a better connection to open/public/alien WiFi networks. Taken from [here](http://lounge.se/wiki2/show/AutoWifi).

## ksh93rc

My `.kshrc` for the `ksh93` (might collide with OpenBSD's default `/etc/ksh.kshrc`, you'd probably edit or remove that file).
