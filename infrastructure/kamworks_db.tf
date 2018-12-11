//// DB Cluster Creation
//resource "aws_rds_cluster" "kamworks_sqldb" {
//  cluster_identifier      = "aurora-cluster-kamworks"
//  engine                  = "aurora"
//  engine_mode             = "serverless"
//  database_name           = "kamworks_db"
//  master_username         = "djehuti"
//  master_password         = "seshat2018"
//  backup_retention_period = 5
//  preferred_backup_window = "07:00-09:00"
//
//
//  db_subnet_group_name   = "${aws_db_subnet_group.kamworks_db_subnet_group.name}"
//  vpc_security_group_ids = ["${aws_security_group.kamworks_sg.id}"]
//
//  scaling_configuration {
//    auto_pause               = true
//    max_capacity             = 256
//    min_capacity             = 2
//    seconds_until_auto_pause = 300
//  }
//
//  tags {
//    Name        = "aurora-cluster-kamworks-1"
//    Environment = "production"
//  }
//}
//
//// DB Subnet Group creation
//resource "aws_db_subnet_group" "kamworks_db_subnet_group" {
//  name        = "db-subnet-group-kamworks"
//  description = "Group of DB subnets"
//  subnet_ids  = ["${module.aurora-cluster-kamworks-vpc.public_subnets}"]
//
//  tags {
//    Name        = "aurora-cluster-kamworks-1"
//    Environment = "production"
//  }
//}
//
//// DB VPC Creation
//resource "aws_security_group" "kamworks_sg" {
//  name   = "aurora-security-group-kamworks"
//  vpc_id = "${module.aurora-cluster-kamworks-vpc.vpc_id}"
//
//  ingress {
//    protocol    = "tcp"
//    from_port   = 3306
//    to_port     = 3306
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  egress {
//    protocol    = -1
//    from_port   = 0
//    to_port     = 0
//    cidr_blocks = ["0.0.0.0/0"]
//  }
//
//  tags {
//    Name        = "aurora-cluster-kamworks-1"
//    Environment = "production"
//  }
//}
//
//// DB VPC Creation
//resource "aws_eip" "aurora-cluster-kamworks-nat" {
//  count = 3
//
//  vpc = true
//
//  tags {
//    Name        = "aurora-cluster-kamworks-1"
//    Environment = "production"
//  }
//}
//
//module "aurora-cluster-kamworks-vpc" {
//  source = "terraform-aws-modules/vpc/aws"
//
//  name                  = "aurora-cluster-kamworks-vpc"
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
//  reuse_nat_ips                    = true                                            # &lt;= Skip creation of EIPs for the NAT Gateways
//  external_nat_ip_ids              = ["${aws_eip.aurora-cluster-kamworks-nat.*.id}"] # &lt;= IPs specified here as input to the module
//
//  tags {
//    Name        = "aurora-cluster-kamworks-1"
//    Environment = "production"
//  }
//}
//
//// Outputs
//
//output "aurora_kamworks_sqldb_name" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.database_name)}"
//  description = "Database name"
//}
//
//output "aurora_kamworks_sqldb_user" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.master_username)}"
//  description = "Username for the master DB user"
//}
//
//output "aurora_kamworks_sqldb_password" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.master_password)}"
//  description = "Password for the master DB user"
//}
//
//output "aurora_kamworks_sqldb_cluster_name" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.cluster_identifier)}"
//  description = "Cluster Identifier"
//}
//
//output "aurora_kamworks_sqldb_arn" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.arn)}"
//  description = "Amazon Resource Name (ARN) of cluster"
//}
//
//output "aurora_kamworks_sqldb_endpoint" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.endpoint)}"
//  description = "The DNS address of the RDS instance"
//}
//
//output "aurora_kamworks_sqldb_reader_endpoint" {
//  value       = "${join("", aws_rds_cluster.kamworks_sqldb.*.reader_endpoint)}"
//  description = "A read-only endpoint for the Aurora cluster, automatically load-balanced across replicas"
//}
//
//output "aurora_kamworks_sqldb_master_host" {
//  value       = "${aws_rds_cluster.kamworks_sqldb.port}"
//  description = "PORT Number"
//}
//
//output "aurora_kamworks_sqldb_replicas_availability_zones" {
//  value       = "${join(", ", aws_rds_cluster.kamworks_sqldb.availability_zones)}"
//  description = "DB Availability Zones"
//}

