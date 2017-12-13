resource "aws_lambda_function" "slapman_slapalicious_OPTIONS" {
  filename         = "infrastructure/slapalicious.zip"
  function_name    = "slapman_slapalicious_OPTIONS"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "main.handle"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious.zip"))}"
  memory_size      = "128"
  runtime          = "python3.6"
  timeout          = "300"

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
  timeout          = "300"

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
  timeout          = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}
