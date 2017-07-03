resource "aws_iam_role" "iam_for_slapman" {
  name = "iam_for_slapman"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_lambda_function" "slapman_slapalicious_GET" {
  filename         = "target/slapalicious.jar"
  function_name    = "slapman_slapalicious_GET"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "slapman.lambda::handleRequest"
  source_code_hash = "${base64sha256(file("target/slapalicious.jar"))}"
  memory_size      = "1024"
  runtime          = "java8"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_slapalicious_POST" {
  filename         = "target/slapalicious.jar"
  function_name    = "slapman_slapalicious_POST"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "slapman.lambda::handleRequest"
  source_code_hash = "${base64sha256(file("target/slapalicious.jar"))}"
  memory_size      = "1024"
  runtime          = "java8"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

