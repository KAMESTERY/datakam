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
    },
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "dynamodb.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_lambda_function" "slapman_slapalicious_OPTIONS" {
  filename         = "infrastructure/slapalicious.zip"
  function_name    = "slapman_slapalicious_OPTIONS"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "main.handle"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious.zip"))}"
  memory_size      = "128"
  runtime          = "python3.6"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_slapalicious_GET" {
  filename         = "infrastructure/slapalicious.zip"
  function_name    = "slapman_slapalicious_GET"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "main.handle"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious.zip"))}"
  memory_size      = "128"
  runtime          = "python3.6"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_slapalicious_POST" {
  filename         = "infrastructure/slapalicious.zip"
  function_name    = "slapman_slapalicious_POST"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "main.handle"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious.zip"))}"
  memory_size      = "128"
  runtime          = "python3.6"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

