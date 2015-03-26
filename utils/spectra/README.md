# spectra
--
Command spectra is a tool for creating a group of Amazon CloudFormation files
based on an environment manifest, base cloudformation template, and a base
CoreOS Userdata file.

Usage summary:

    > Usage of Spectra:
      spectra [options] [command]

      Commands:
        run  - runs the cloudformation files
        seed - seeds the cloudformation files
        help - show this message

      Flags:
        -dir="output": output directory to use
        -env="environments.yaml": list of environments to create configs for
        -template="cftemplate.json": CloudFormation template to use
        -userdata="user-data": base user-data to extend from

Example environments.yaml:

    # This file must have everything inside the `environments` group
    environments:
      - name: test
        description: >
          A test environment made as a one-off
        tags: test-alliance=please-ignore
        kind: t2.micro
        subnet:
          - subnet-deadb00b
        zone:
          - us-east-1a
        az: us-east-1a
        root: 15
      - name: deis
        description: >
          A parent with children
        nodes:
          - name: child
            description: >
              Child subcluster 1
            tags: test-alliance=please-ignore
            kind: t2.micro
            subnet:
              - subnet-deadb00b
            zone:
              - us-east-1a
            az: us-east-1a
            root: 15

    override:
      etcd: https://discovery.etcd.io/b8baedc2e87105ed9ea622ee2c0095a8
      vpc: vpc-fabecabe

See the example directory for examples on the cloudformation template and CoreOS
userdata.

As this tool was designed to automate creation of cloudformations for a deploy
of Deis, the examples and some things of that ilk will reference Deis. This tool
is distributed publicly in the hope that it will be useful but the authors of
this tool assume no responsibility for any resources it creates.

Using:

Create a folder with all your files in it. Run spectra seed and then spectra
run. Assuming you have the aws tool correctly set up it should work.
