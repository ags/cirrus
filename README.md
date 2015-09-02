cirrus
------

A tool for generation and deployment of opinionated AWS CloudFormation stacks.

This is still undergoing initial development, and while it is functional should
not be considered for serious production use yet.

## Usage

### parse

`cirrus parse <config-file>`

Outputs a CloudFormation template for the given configuration file to stdout.

### deploy

`cirrus deploy <stack-name> <config-file>`

Creates or updates a CloudFormation stack with the given name using the given
configuration file.

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

Deploying this configuration results in:

* A Security Group.

Configured for the given VPC, allows TCP traffic from port 80 to port 80.

* An Elastic Load Balancer.

A cross-zone ELB in the configured security group and subnets '12' and '34',
which listens on port 80 for HTTP traffic. The ELB uses a connection draining
policy and health checks `/` on port 80 of each instance.

* An Auto Scaling Group.

An ASG associated with the generated Elastic Load Balancer, utilizing the ELB
health check. The termination policy terminates the oldest instance first.  ASG
instances are tagged with the name of the CloudFormation stack, and belongs to
subnets '56' and '78'. The creation policy requires all instances to be healthy
to succeed, and updates are done by replacing one instance at a time, ensuring
one is always in service.

* A Launch Configuration.

This configuration results in a t2.micro instance based on ami-0d4cfd66.  It
adds the given instance to the defined Security Group in order to allow HTTP
traffic. The provided user data is used to bootstrap the instance.  cirrus
prepends data to forward the user data output to `/dev/console` so it is
available in the system log of the instance in the AWS EC2 web console, and
also appends data to signal the Auto Scaling Group if the provided user data
completes successfully.

See [tests/fixture.json](https://raw.githubusercontent.com/ags/cirrus/master/tests/fixture.json) for an example of the generated CloudFormation template.
