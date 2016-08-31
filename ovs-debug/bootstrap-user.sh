#!/usr/bin/env bash

TOOLS_DIR=/vagrant
MININET_DIR=${TOOLS_DIR}/mininet
OVS_SRC_DIR=${TOOLS_DIR}/ovs
OVS_PREFIX=/usr/local

# Prepare OVS for configuration
cd "${OVS_SRC_DIR}"
./boot.sh
make distclean

# Install ovs from source dir (building in a separate dir so we don't pollute /vagrant)
OVS_BUILD_DIR=/tmp/ovs-build
mkdir -pv ${OVS_BUILD_DIR}
cd ${OVS_BUILD_DIR}
${TOOLS_DIR}/ovs/configure CFLAGS="-g" --prefix=${OVS_PREFIX} --with-debug
make
sudo make install

# Initialize ovs
sudo ${TOOLS_DIR}/bin/ovs-init "${OVS_SRC_DIR}" "${OVS_PREFIX}"

# Start OVS daemons
sudo ${TOOLS_DIR}/bin/ovs-start "${OVS_PREFIX}"

# "Install" pox
git clone https://github.com/noxrepo/pox /home/vagrant/pox

# Clone and install mininet (also installs lots of dependencies)
git clone https://github.com/mininet/mininet.git /home/vagrant/mininet
sudo /home/vagrant/mininet/util/install.sh -n -w
