Technical Design
================

### Admiral, the fleetmaster

Goal
----

Project Admiral will be an implementation of a management interface for 
organizations wanting to use CoreOS in production. Admiral will fill the 
following roles:

- Bootstrap utility for new CoreOS clusters
- Lifecycle management for existing CoreOS clusters
- Centralized role-based configuration
- Update management

Bootstrap
---------

Tools to select machines to be in parts of a cluster and generate the needed 
server side etcd discovery information as well as the cloudinit files loaded 
on boot.

Lifecycle
---------

This will be defined as an API that existing lifecycle management tools can 
interact with.

Configuration
-------------

Admiral will serve the cloudinit files used by CoreOS for provisioning and 
activating services on the cluster.

Update Management
-----------------

This will middleman with an omaha server to allow administrators to 
selectively upgrade CoreOS machines to different versions as well as provide 
the legwork for making custom image creation easier.

---


