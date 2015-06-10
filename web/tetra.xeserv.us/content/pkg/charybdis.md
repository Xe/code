+++
title = "charybdis"
+++

```go
import "github.com/Xe/Tetra/bot/script/charybdis"
```

Package charybdis contains wrapped functions taken directly from the charybdis
source tree.

## Usage

#### func  CloakHost

```go
func CloakHost(host string) (result string)
```
CloakHost will apply the charybdis cloaking function to a given string and
return its result. The limit of the string given in is 100.

#### func  CloakIP

```go
func CloakIP(host string) (result string)
```
CloakIP will apply the charybdis ip address cloaking function to a given string
and return its result. This should be an IP address.
