resource "aws_iam_role" "iam_for_lambda" {
  name = "iam_for_lambda"

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

resource "aws_lambda_function" "fn-claws_hello_GET" {
  filename         = "hello.zip"
  function_name    = "fn-claws_hello_GET"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "index.handle"
  source_code_hash = "${base64sha256(file("hello.zip"))}"
  memory_size      = "512"
  runtime          = "nodejs6.10"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "fn-claws_hello_POST" {
  filename         = "hello.zip"
  function_name    = "fn-claws_hello_POST"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "index.handle"
  source_code_hash = "${base64sha256(file("hello.zip"))}"
  memory_size      = "512"
  runtime          = "nodejs6.10"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "fn-claws_pyfun_GET" {
  filename         = "pyfun.zip"
  function_name    = "fn-claws_pyfun_GET"
  role             = "${aws_iam_role.iam_for_lambda.arn}"
  handler          = "main.handle"
  source_code_hash = "${base64sha256(file("pyfun.zip"))}"
  runtime          = "python3.6"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

