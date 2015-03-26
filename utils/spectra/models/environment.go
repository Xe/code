package models

// Struct Environment is a parent structure for the group of
// Nodes that make up the desited cluster.
type Environment struct {
	Environments []*Node `yaml:"environments"` // Each element of the environment
	Override     *Node   `yaml:"override"`     // Global settings that overrule the local nodes
	EtcdURL      string  `yaml:"etcd"`         // Etcd discovery token
	Suffix       string  `yaml:"suffix"`       // Domain name suffix
}

// Struct Node defines an individual cloudformation subcluster.
type Node struct {
	Name        string   `yaml:"name"`        // Node or group name
	Description string   `yaml:"description"` // Node or group description
	Children    []*Node  `yaml:"nodes"`       // Sub-nodes if applicable
	Tags        string   `yaml:"tags"`        // Node tags for fleet
	Wants       string   `yaml:"wants"`       // Fleet services this will bake in
	Kind        string   `yaml:"kind"`        // AWS node kind
	Root        string   `yaml:"root"`        // Root disk size
	VPCId       string   `yaml:"vpc"`         // VPC ID for the cloudformation
	VPCSubnets  []string `yaml:"subnet"`      // Subnets to use for VPC
	VPCZones    []string `yaml:"zone"`        // VPC zones
	AvailZone   string   `yaml:"az"`          // AWS zone
}
