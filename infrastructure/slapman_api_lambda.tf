resource "aws_lambda_function" "slapman_slapalicious_OPTIONS" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_slapalicious_OPTIONS"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "slapman"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"
  memory_size      = "128"
  runtime          = "go1.x"
  timeout          = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_slapalicious_GET" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_slapalicious_GET"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "slapman"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"
  memory_size      = "128"
  runtime          = "go1.x"
  timeout          = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_slapalicious_POST" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_slapalicious_POST"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "slapman"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"
  memory_size      = "128"
  runtime          = "go1.x"
  timeout          = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}
