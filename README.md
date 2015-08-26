cirrus
------

A tool for generation and deployment of opinionated AWS CloudFormation stacks.

This is still undergoing initial development, and while it is functional should
not be considered for serious production use yet.

## Usage

### parse

`cirrus parse <config-file>`

Outputs a CloudFormation template for the given configuration file to stdout.

## Example Configuration

```yaml
vpc: vpc-183bcd7c

instanceType: t2.micro

ami: ami-0d4cfd66

capacity:
  min: 2
  max: 3

healthCheck:
  target: "http:80/"
  timeout: 2
  interval: 10
  healthyThreshold: 3
  unhealthyThreshold: 5

listeners:
  - port: 80
    protocol: "HTTP"

userData: |
  yum update -y
  yum install -y docker
  service docker start
  docker run -d -p 80:80 tutum/hello-world

subnets:
  loadBalancer:
    - subnet-12
    - subnet-34
  instance:
    - subnet-56
    - subnet-78
```
