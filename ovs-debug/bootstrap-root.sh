#!/usr/bin/env bash

TOOLS_DIR=/vagrant
MININET_DIR=${TOOLS_DIR}/mininet
OVS_SRC_DIR=${TOOLS_DIR}/ovs

#https://github.com/openvswitch/ovs.git

# Install basic dependencies
apt-get update
apt-get -y install build-essential git gdb strace htop fakeroot debhelper dh-autoreconf libssl-dev python emacs24-nox vim multitail

# Copy ./home to vagrant's home (vbox mounts it as vagrant:vagrant)
cp -pRv ${TOOLS_DIR}/home/. /home/vagrant/

# Mount /vagrant in vagrant's home for easy access
mkdir -pv /home/vagrant/vagrant
mount -o bind /vagrant /home/vagrant/vagrant

# Add local modifications to /etc (adds /vagrant to PATH, etc.)
install -v -o root -g root -m 640 ${TOOLS_DIR}/etc/sudoers.d/ovs-path /etc/sudoers.d/ovs-path
install -v -o root -g root -m 644 ${TOOLS_DIR}/etc/profile.d/vagrant.sh /etc/profile.d/vagrant.sh







