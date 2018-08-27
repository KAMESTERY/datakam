resource "aws_lambda_function" "slapman_lambda_fn_OPTIONS" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_lambda_fn_OPTIONS"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "lambda_fn"
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

resource "aws_lambda_function" "slapman_lambda_fn_GET" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_lambda_fn_GET"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "lambda_fn"
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

resource "aws_lambda_function" "slapman_lambda_fn_POST" {
  filename         = "infrastructure/slapalicious-api.zip"
  function_name    = "slapman_lambda_fn_POST"
  role             = "${aws_iam_role.iam_for_slapman.arn}"
  handler          = "lambda_fn"
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
