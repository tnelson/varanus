# OVS Development VM

This document describes how to set up and use a Vagrant VM to debug OVS.  

## Requirements

To perform the setup, you'll need the following installed on your system:
 - Vagrant
 - A VM platform of your choice (tested on virtualbox)

## Initial setup

 - Clone this repository and enter this directory
 - Clone the openvswitch source into this directory
`git clone https://github.com/openvswitch/ovs.git ovs`
*NOTE*: I'm assuming we'll have a better way of doing this soon (like
 a separate repo forked from ovs)
 - Initialize the VM:  `vagrant up`

This tells Vagrant to install the VM and provision it as defined in
`bootstrap.sh`.  This includes installing dependencies, building
openvswitch from source, and installing mininet and POX.  OVS should
be started automatically.

After the VM has been provisioned, this step should simply start the
VM using its current configuration.

 - Connect to the VM and get a shell:  `vagrant ssh`

## Starting mininet / Using our debug build of OVS

Our custom build of OVS does not install the openvswitch kernel
module.  Instead, we are using a version of OVS's datapath that exists
entirely in userspace, which should allow us to debug it more easily
(for now).

To tell mininet to use OVS in this configuration, we can start it as follows:
`sudo mn --switch=ovsk,datapath=user --controller=remote,ip=127.0.0.1,port=6633`

Be sure to update the `--controller` argument for your needs.  

## Examples for manipulating openvswitch

We need a custom build of openvswitch, so we don't have or need any of
the fancy init scripts that come with a package install.

- To start OVS:
`sudo ovs-start`

- To initialize the OVS database (only needed on a clean install, and performed automatically by `bootstrap.sh`):
`sudo ovs-init <path_to_ovs_source>`

## Debugging OVS

We will be debugging the virtual switch component of OVS, so our
target binary is `ovs-vswitchd`.  To start the debugger, we need to
start gdb and attach it to the running `ovs-vswitchd` binary.  To do
this, run: `sudo ovs-debug`

Alternatively, you can run the curses-based debugger from emacs as follows:  
 1. Start emacs and run `M-x gdb` 
 2. Change the start command to `sudo gdb -i=mi /usr/local/sbin/ovs-vswitchd`
 3. Find the PID of `ovs-vswitchd` with `cat /usr/local/var/run/openvswitch/ovs-vswitchd.pid`
 4. In the emacs gdb shell buffer, enter `attach <PID>`, where <PID>
 is the PID for ovs-vswitchd

## Additional notes:  files and paths

 - Once the VM is started, Vagrant automatically shares the directory
   containing the Vagrantfile with the VM in `/vagrant`.  Our
   provisioning script also mounts this to `/home/vagrant/vagrant`

 - Our provisioning script installs all file pertaining to OVS in the
   prefix `/usr/local`.  Thus, the `ovs-vswitchd` binary is located in
   `/usr/local/sbin/ovs-vswitchd` and logs can be found in
   `/usr/local/var/log/openvswitch`

 - Sources for mininet and pox are located in `/home/vagrant/mininet`
   and /home/vagrant/pox', respectively.
 
 - Start scripts `ovs-init`, `ovs-start` and the like are located in
   `/vagrant/bin`.  Feel free to modify/extend them as necessary.

