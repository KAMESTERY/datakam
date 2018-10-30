variable "aws_account_id" {
  description = "AWS account id"
  default     = "661405543333"
}

# Multiple Regions: us-east-1 sa-east-1 eu-central-1 cn-north-1 ap-northeast-1 ap-southeast-2
variable "aws_region" {
  description = "AWS region"
  default     = "us-east-1"
}

variable "web-bucket" {
  description = "The S3 bucket name for the Website"
  default     = "slapman-web"
}

variable "slapman-bucket" {
  description = "The S3 bucket name for the Website"
  default     = "slapmanfuns"
}

variable "burst_limit" {
  default = 5
}

variable "rate_limit" {
  default = 10
}

###### VPS Variables

## Docker
variable "docker_compose_version" {
  default = "1.23.1"
}

## Lightsail Master Instance
variable "lightsail_master_bundle_id" {
  default = "nano_1_0"
}

variable "lightsail_master_blueprint_id" {
  default = "ubuntu_18_04"
}

## Lightsail Worker Instances
variable "lightsail_worker_count" {
  default = 3
}

variable "lightsail_worker_bundle_id" {
  default = "nano_1_0"
}

variable "lightsail_worker_blueprint_id" {
  default = "ubuntu_18_04"
}
