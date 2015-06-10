+++
title = "crypto"
+++

```go
import "github.com/Xe/Tetra/bot/script/crypto"
```

Package crypto is for cryptographic or similar kinds of functions.

## Usage

#### func  Fnv

```go
func Fnv(data string) string
```
Fnv is a non-cryptographic hashing function based on non-cryptographic hash
functions created by Glenn Fowler, Landon Curt Noll, and Phong Vo. This wraps
the standard library FNV hash function by representing it as a 32 bit integer.
Then it takes that number and "hashes" it by adding the rune's unicode value (as
a uint 32) and then takes the modulus of 26, letting the number be represented
as a letter of the alphabet. This is not a cryptographically secure operation,
it is purely to replace numbers with a human-readable string to satisfy the
requirement that any vhost with a "/" in it cannot end in a number (to avoid
someone obtaining a vhost that is a cidr mask, it can cause issues).

#### func  Hash

```go
func Hash(data string, salt string) string
```
Hash is a simple wrapper around the MD5 algorithm implementation in the Go
standard library. It takes in data and a salt and returns the hashed
representation.
