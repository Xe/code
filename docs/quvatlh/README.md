Qu'vatlh
========

A scheduler for hundreds of tasks across a diverse multi-tenant cloud-aware 
dynamically scaling ecosystem.

Design
------

Qu'vatlh will use a master->slave approach to clustering.

Terms
-----

 - Event: the result of new Tasks being added or deleted
 - Task: a unclaimed item of work to be done
 - Worker: claims a task and produces an action from that task
 - Action: the result of a task being run
 - Policy: a function that checks if a task can be claimed by a worker

Events
------

The mechanism for getting events to workers is via a bunch of HTTP requests to 
an external server that will also manage state. This may be internal or 
external depending on your deployed configuration.

### `ADD`

Adds tasks to the pool for workers to bid on if the policy function returns 
true.

### `DELETE`

Removes tasks from the pool and kills off any extra actions.

### `BALANCE`

Balances a group of tasks to better fit load across the cluster.

Task
----

A task has:

 - `Group`: what logical group the task is in. This could be the application 
 name or a better optimized name for the application the task will run
 - `Balancable`: If the task can be rebalanced automatically, this will be set 
 true. The default value is true. Set this to false for things that involve 
 long-lived connections to users or other services such as database servers.
 - `Policy`: a Lua function to see if the task may run on the local machine.
 - `Name`: the policy's name
 - `Limit`: the hard limit on the number of identical instances that can exist.
 - `Announce`: a Lua function to be run as part of the announcement that the 
 action is started.
 - `Work`: a yaml description of what commands to run to start the action.
 - `Tags`: the task will only run on workers with these tags. Glob-matched.

When a worker bids on a task it will run its policy function with the system 
state and the task as arguments.

Worker
------

A worker keeps a list of its active actions and the tasks that spawned them. It 
will listen for events and bid on new tasks if it is allowed to run it by its 
policy.

A worker has:
 - `Name`: The unique name of the worker, will be automatically generated if it 
 is not set in the configuration for the system.
 - `Tags`: tags that match events it will attempt to bid on.

Action
------

The work running on a server.

Policy
------

```lua
PolicyFuncBase = function(state, task) -- returns boolean
end
```
