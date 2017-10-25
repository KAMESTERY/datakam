
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
  default = "slapman-web"
}

