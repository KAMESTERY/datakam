////////////////////////
//// Role for EKSKam
////////////////////////
//
//// Scheduler
//resource "aws_iam_role" "iam_for_ekskam" {
//  name = "iam_for_ekskam"
//
//  assume_role_policy = <<POLICY
//{
//  "Version": "2012-10-17",
//  "Statement": [
//    {
//      "Effect": "Allow",
//      "Principal": {
//        "Service": "eks.amazonaws.com"
//      },
//      "Action": "sts:AssumeRole"
//    }
//  ]
//}
//POLICY
//}
//
//resource "aws_iam_role_policy_attachment" "ekskam-AmazonEKSClusterPolicy" {
//  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
//  role       = "${aws_iam_role.iam_for_ekskam.name}"
//}
//
//resource "aws_iam_role_policy_attachment" "ekskam-AmazonEKSServicePolicy" {
//  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSServicePolicy"
//  role       = "${aws_iam_role.iam_for_ekskam.name}"
//}
//
//// Workers
//resource "aws_iam_role" "iam_for_worker_ekskam" {
//  name = "iam_for_worker_ekskam"
//
//  assume_role_policy = <<POLICY
//{
//  "Version": "2012-10-17",
//  "Statement": [
//    {
//      "Effect": "Allow",
//      "Principal": {
//        "Service": "ec2.amazonaws.com"
//      },
//      "Action": "sts:AssumeRole"
//    }
//  ]
//}
//POLICY
//}
//
//resource "aws_iam_role_policy_attachment" "worker_ekskam-AmazonEKSWorkerNodePolicy" {
//  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
//  role       = "${aws_iam_role.iam_for_worker_ekskam.name}"
//}
//
//resource "aws_iam_role_policy_attachment" "worker_ekskam-AmazonEKS_CNI_Policy" {
//  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
//  role       = "${aws_iam_role.iam_for_worker_ekskam.name}"
//}
//
//resource "aws_iam_role_policy_attachment" "worker_ekskam-AmazonEC2ContainerRegistryReadOnly" {
//  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
//  role       = "${aws_iam_role.iam_for_worker_ekskam.name}"
//}
//
//resource "aws_iam_instance_profile" "worker_ekskam" {
//  name = "ekskam-workers"
//  role = "${aws_iam_role.iam_for_worker_ekskam.name}"
//}
//
////////////////////////
//// EKSKam Cluster
////////////////////////
//
//resource "aws_eks_cluster" "ekskam-cluster" {
//  name     = "ekskam-cluster"
//  role_arn = "${aws_iam_role.iam_for_ekskam.arn}"
//
//  "vpc_config" {
//    security_group_ids = ["${aws_security_group.ekskam-cluster.id}"]
//    subnet_ids         = ["${module.ekskam-vpc.public_subnets}"]
//  }
//
//  depends_on = [
//    "aws_iam_role_policy_attachment.ekskam-AmazonEKSClusterPolicy",
//    "aws_iam_role_policy_attachment.ekskam-AmazonEKSServicePolicy",
//  ]
//}
//
////////////////////////////
//// EKSKam Security Group
////////////////////////////
//
//resource "aws_security_group" "ekskam-cluster" {
//  name        = "ekskam-cluster"
//  description = "Cluster communication with worker nodes"
//  vpc_id      = "${module.ekskam-vpc.vpc_id}"
//
//  egress {
//    from_port   = 0
//    to_port     = 0
//    protocol    = "-1"
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  tags {
//    Name = "ekskam-cluster"
//  }
//}
//
//resource "aws_security_group" "ekskam-workers" {
//  name        = "ekskam-workers"
//  description = "Security group for all nodes in the cluster"
//  vpc_id      = "${module.ekskam-vpc.vpc_id}"
//
//  egress {
//    from_port   = 0
//    to_port     = 0
//    protocol    = "-1"
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  tags = "${
//    map(
//     "Name", "ekskam-worker-node",
//     "kubernetes.io/cluster/${aws_eks_cluster.ekskam-cluster.name}", "owned",
//    )
//  }"
//}
//
//resource "aws_security_group_rule" "ekskam-workers-ingress-self" {
//  description              = "Allow node to communicate with each other"
//  from_port                = 0
//  protocol                 = "-1"
//  security_group_id        = "${aws_security_group.ekskam-workers.id}"
//  source_security_group_id = "${aws_security_group.ekskam-workers.id}"
//  to_port                  = 65535
//  type                     = "ingress"
//}
//
//resource "aws_security_group_rule" "ekskam-workers-ingress-cluster" {
//  description              = "Allow worker Kubelets and pods to receive communication from the cluster control plane"
//  from_port                = 1025
//  protocol                 = "tcp"
//  security_group_id        = "${aws_security_group.ekskam-workers.id}"
//  source_security_group_id = "${aws_security_group.ekskam-cluster.id}"
//  to_port                  = 65535
//  type                     = "ingress"
//}
//
//resource "aws_security_group_rule" "ekskam-cluster-ingress-node-https" {
//  description              = "Allow pods to communicate with the cluster API Server"
//  from_port                = 443
//  protocol                 = "tcp"
//  security_group_id        = "${aws_security_group.ekskam-cluster.id}"
//  source_security_group_id = "${aws_security_group.ekskam-workers.id}"
//  to_port                  = 443
//  type                     = "ingress"
//}
//
////////////////////////
//// EKSKam SubNets
////////////////////////
//
//resource "aws_eip" "ekskam-nat" {
//  count = 3
//
//  vpc = true
//}
//
//module "ekskam-vpc" {
//  source = "terraform-aws-modules/vpc/aws"
//
//  name                  = "ekskam-vpc"
//  cidr                  = "10.0.0.0/16"
//  secondary_cidr_blocks = ["10.1.0.0/16", "10.2.0.0/16"]
//
//  azs             = ["us-east-1a", "us-east-1b", "us-east-1c"]
//  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
//  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
//
//  assign_generated_ipv6_cidr_block = true
//  enable_vpn_gateway               = false
//  enable_dns_hostnames             = true
//  enable_nat_gateway               = true
//  single_nat_gateway               = false
//  reuse_nat_ips                    = true                           # &lt;= Skip creation of EIPs for the NAT Gateways
//  external_nat_ip_ids              = ["${aws_eip.ekskam-nat.*.id}"] # &lt;= IPs specified here as input to the module
//
//  tags = {
//    Terraform   = "true"
//    Environment = "dev"
//  }
//}
//
////////////////////////////////
//// Worker Nodes for EKSKam
////////////////////////////////
//
//data "aws_ami" "ekskam-workers-ami" {
//  filter {
//    name   = "name"
//    values = ["amazon-eks-node-v*"]
//  }
//
//  most_recent = true
//  owners      = ["602401143452"] # Amazon EKS AMI Account ID
//}
//
//# EKS currently documents this required userdata for EKS worker nodes to
//# properly configure Kubernetes applications on the EC2 instance.
//# We utilize a Terraform local here to simplify Base64 encoding this
//# information into the AutoScaling Launch Configuration.
//# More information: https://docs.aws.amazon.com/eks/latest/userguide/launch-workers.html
//locals {
//  ekskam-workers-userdata = <<USERDATA
//#!/bin/bash
//set -o xtrace
///etc/eks/bootstrap.sh --apiserver-endpoint '${aws_eks_cluster.ekskam-cluster.endpoint}' --b64-cluster-ca '${aws_eks_cluster.ekskam-cluster.certificate_authority.0.data}' '${aws_eks_cluster.ekskam-cluster.name}'
//USERDATA
//}
//
//resource "aws_launch_configuration" "ekskam-workers-launch-configuration" {
//  associate_public_ip_address = true
//  iam_instance_profile        = "${aws_iam_instance_profile.worker_ekskam.name}"
//  image_id                    = "${data.aws_ami.ekskam-workers-ami.id}"
//  instance_type               = "t2.nano"
//  name_prefix                 = "ekskam-workers"
//  security_groups             = ["${aws_security_group.ekskam-workers.id}"]
//  user_data_base64            = "${base64encode(local.ekskam-workers-userdata)}"
//
//  lifecycle {
//    create_before_destroy = true
//  }
//}
//
//resource "aws_autoscaling_group" "ekskam-workers-auto-scaling-group" {
//  desired_capacity     = 2
//  launch_configuration = "${aws_launch_configuration.ekskam-workers-launch-configuration.id}"
//  max_size             = 8
//  min_size             = 1
//  name                 = "ekskam-workers"
//  vpc_zone_identifier  = ["${module.ekskam-vpc.public_subnets}"]
//
//  tag {
//    key                 = "Name"
//    value               = "ekskam-workers"
//    propagate_at_launch = true
//  }
//
//  tag {
//    key                 = "kubernetes.io/cluster/${aws_eks_cluster.ekskam-cluster.name}"
//    value               = "owned"
//    propagate_at_launch = true
//  }
//}
//
////////////////////////
//// EKSKam KubeConfig
////////////////////////
//
//locals {
//  kubeconfig = <<KUBECONFIG
//
//
//apiVersion: v1
//clusters:
//- cluster:
//    server: ${aws_eks_cluster.ekskam-cluster.endpoint}
//    certificate-authority-data: ${aws_eks_cluster.ekskam-cluster.certificate_authority.0.data}
//  name: kubernetes
//contexts:
//- context:
//    cluster: kubernetes
//    user: aws
//  name: aws
//current-context: aws
//kind: Config
//preferences: {}
//users:
//- name: aws
//  user:
//    exec:
//      apiVersion: client.authentication.k8s.io/v1alpha1
//      command: aws-iam-authenticator
//      args:
//        - "token"
//        - "-i"
//        - "${aws_eks_cluster.ekskam-cluster.name}"
//KUBECONFIG
//
//  config_map_aws_auth = <<CONFIGMAPAWSAUTH
//
//
//apiVersion: v1
//kind: ConfigMap
//metadata:
//  name: aws-auth
//  namespace: kube-system
//data:
//  mapRoles: |
//    - rolearn: ${aws_iam_role.iam_for_worker_ekskam.arn}
//      username: system:node:{{EC2PrivateDNSName}}
//      groups:
//        - system:bootstrappers
//        - system:nodes
//CONFIGMAPAWSAUTH
//}
//
//output "ekskam-kubeconfig" {
//  value = "${local.kubeconfig}"
//}
//
////echo $(terraform output ekskam-kubeconfig) > infrastructure/kube/ekskam-kubeconfig.yml
////KUBECONFIG=infrastructure/kube/ekskam-kubeconfig.yml kubectl get componentstatus
////KUBECONFIG=infrastructure/kube/ekskam-kubeconfig.yml kubectl get nodes
//
//output "ekskam-config_map_aws_auth" {
//  value = "${local.config_map_aws_auth}"
//}
//
////echo $(terraform output ekskam-config_map_aws_auth) > infrastructure/kube/ekskam-config_map_aws_auth.yml
////KUBECONFIG=infrastructure/kube/ekskam-kubeconfig.yml kubectl apply -f infrastructure/kube/ekskam-config_map_aws_auth.yml
////KUBECONFIG=infrastructure/kube/ekskam-kubeconfig.yml kubectl get nodes --watch
//

