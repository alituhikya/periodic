# Periodic
Periodic - Erlang library to make it easy to create to run tasks periodically and do set up deferred tasks that can expire after a given time.

- Perform a task at certain intervals
- Different interval functions eg exponential, linear, normal
- Wait for a certain time (timeout) to receive a signal before performing a certain task, or else expiring

## Contents
- [Introduction](#introduction)
- [Documentation overview](#documentation-overview)
- [Building](#building)
  - [Dependencies](#dependencies)
- [Contributors](#contributors)

## Introduction 
The periodic application provides functions to to run tasks periodically and do set up deferred tasks that can expire after a given time.
It will create a supervision tree that will supervise two types of processes depending on the needs of the application.
These supervisors are started once the app is started.

The periodic tasks can be set to occur at:
- fixed linear intervals
- Exponentially increasing intervals
- or intervals



## Documentation overview
A [use cases document](doc/use_cases.md) gives an overview of the
application by describing the 3 main use cases:

1.
1.
1.

For more details there are additional documents:
-

## Building
rebar is used to build the software. 

```
rebar get-deps
rebar compile
```

## Testing
rebar can also be used to run the tests.

```
rebar -C test.config eunit

```


## Contributors
This application was developed by James Alituhikya.  The development of this library was sponsored by [chapchap ltd](https://www.chapchap.co).