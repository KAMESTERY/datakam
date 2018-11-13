////////////////////////
//// Role for EKSKam
////////////////////////
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
////////////////////////
//// EKSKam Cluster
////////////////////////
//
//resource "aws_eks_cluster" "ekskam-cluster" {
//  name     = "ekskam-cluster"
//  role_arn = "${aws_iam_role.iam_for_ekskam.arn}"
//
//  "vpc_config" {
//    subnet_ids = ["${aws_subnet.ekskam-subnet.*.id}"]
//  }
//
//  depends_on = [
//    "aws_iam_role_policy_attachment.ekskam-AmazonEKSClusterPolicy",
//    "aws_iam_role_policy_attachment.ekskam-AmazonEKSServicePolicy",
//  ]
//}
//
////////////////////////
//// EKSKam SubNets
////////////////////////
//
//resource "aws_subnet" "ekskam-subnet" {
//  cidr_block = "${module.ekskam-vpc.vpc_cidr_block}"
//  vpc_id     = "${module.ekskam-vpc.vpc_id}"
//}
//
//module "ekskam-vpc" {
//  source = "terraform-aws-modules/vpc/aws"
//
//  name = "ekskam-vpc"
//  cidr = "10.0.0.0/16"
//
//  azs             = ["us-east-1a", "us-east-1b", "us-east-1c"]
//  private_subnets = ["10.0.1.0/24", "10.0.2.0/24", "10.0.3.0/24"]
//  public_subnets  = ["10.0.101.0/24", "10.0.102.0/24", "10.0.103.0/24"]
//
//  enable_nat_gateway = true
//  enable_vpn_gateway = true
//
//  tags = {
//    Terraform   = "true"
//    Environment = "dev"
//  }
//}
//
////////////////////////
//// EKSKam KubeConfig
////////////////////////
//
//locals {
//  kubeconfig = <<KUBECONFIG
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
//      command: heptio-authenticator-aws
//      args:
//        - "token"
//        - "-i"
//        - "${aws_eks_cluster.ekskam-cluster.name}"
//KUBECONFIG
//}
//
//output "ekskam-kubeconfig" {
//  value = "${local.kubeconfig}"
//}

