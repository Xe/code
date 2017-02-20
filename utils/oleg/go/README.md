# oleg
--
    import "github.com/Xe/oleg/go"


## Usage

```go
var (
	// Key not found in OlegDB
	ErrNoSuchKey = errors.New("No such key")
)
```
Our errors

#### type Database

```go
type Database struct {
}
```

Database is an OlegDB reference. This is good for data storage I think.

#### func  Purchase

```go
func Purchase(host, port string) *Database
```
Purchase creates a new Database by getting it from the store.

The hostname and port arguments are strings because laziness is next to
godliness.

#### func (*Database) Jar

```go
func (d *Database) Jar(table, key, value string) error
```
Jar puts key into table in OlegDB, returning an error if the operation failed.

There's plenty of reasons this could fail, but none of them would ever need to
come up.

#### func (*Database) Scoop

```go
func (d *Database) Scoop(table, key string) (err error)
```
Scoop removes key from table. Get that crap out of the mayo jar.

#### func (*Database) Unjar

```go
func (d *Database) Unjar(table, key string) (value string, err error)
```
Unjar gets key from table out of OlegDB, returning an error if the operation
failed.
