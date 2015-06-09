.PHONY: all
all: go
	@echo "Done"

.PHONY: go
go:
	@protoc --go_out comm comm.proto
	@protoc --go_out register register.proto
