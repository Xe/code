package models

// Struct CloudFormation is used for spectra seed to define
// mappings inside the output cloudformation.
type CloudFormation struct {
	Version     string                 `json:"AWSTemplateFormatVersion"`
	Parameters  map[string]interface{} `json:"Parameters"`
	Resources   map[string]interface{} `json:"Resources"`
	Mappings    map[string]interface{} `json:"Mappings"`
	Description string                 `json:"Description"`
}

// Type Paramater is shorthand for having to do typecasting.
type Parameter struct {
	Key   string `json:"ParameterKey"`
	Value string `json:"ParameterValue"`
}

type ActiveStack struct {
	Name         string      `json:"StackName"`
	Description  string      `json:"Description"`
	Id           string      `json:"StackID"`
	Parameters   []Parameter `json:"Parameters"`
	Tags         []string    `json:"Tags"`
	CreationTime string      `json:"CreationTime"`
	StackStatus  string      `json:"StackStatus"`
}
