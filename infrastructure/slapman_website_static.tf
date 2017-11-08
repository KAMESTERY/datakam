
resource "aws_s3_bucket" "slapman-web" {
  bucket = "${var.web-bucket}"
  acl = "public-read"

  cors_rule {
    allowed_headers = ["*"]
    allowed_methods = ["GET", "POST"]
    allowed_origins = ["*"]
    expose_headers = ["Etag"]
    max_age_seconds = 3000
  }

  website {
    index_document = "index.html"
    error_document = "404.html"

    routing_rules = <<EOF
[{
    "Condition": {
        "KeyPrefixEquals": "docs/"
    },
    "Redirect": {
        "ReplaceKeyPrefixWith": "documents/"
    }
}]
EOF
  }
}

output "slapman-static-web-url" {
  value = "http://${var.web-bucket}.s3-website-${var.aws_region}.amazonaws.com"
}
