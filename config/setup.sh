#!/bin/bash


# For some reason, sysrq pops up on every shift, control or command action.
echo "sysctl -w kernel.sysrq=0" >> ~/.bashrc
source ~/.bashrc

rm /etc/apt/sources.list.d/bluemap.list

apt-get update

apt-get -y remove network-manager
apt-get -y install bridge-utils
apt-get -y install vim

update-rc.d -f dnsmasq remove
update-rc.d -f ip-routing remove
update-rc.d -f mobile-internet remove
update-rc.d -f ebtables remove
update-rc.d -f ssh-phone-home remove
update-rc.d -f nfs-kernel-server remove
update-rc.d -f nfs-common remove
update-rc.d -f portmap remove
update-rc.d -f mountnfs.sh remove
update-rc.d -f umountnfs.sh remove
rm /etc/exports
