resource "aws_lambda_function" "slapman_worker_OPTIONS" {
  filename = "infrastructure/slapalicious-api.zip"

  //  filename         = "infrastructure/slapalicious-web.zip"
  function_name = "slapman_worker_OPTIONS"
  role          = "${aws_iam_role.iam_for_slapman.arn}"
  handler       = "worker-fn"

  //  handler          = "worker-exe.handle"
  //  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-web.zip"))}"
  //  memory_size      = "128"
  //  runtime          = "python3.6"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"

  memory_size = "128"
  runtime     = "go1.x"
  timeout     = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_worker_GET" {
  filename = "infrastructure/slapalicious-api.zip"

  //  filename         = "infrastructure/slapalicious-web.zip"
  function_name = "slapman_worker_GET"
  role          = "${aws_iam_role.iam_for_slapman.arn}"
  handler       = "worker-fn"

  //  handler          = "worker-exe.handle"
  //  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-web.zip"))}"
  //  memory_size      = "128"
  //  runtime          = "python3.6"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"

  memory_size = "128"
  runtime     = "go1.x"
  timeout     = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}

resource "aws_lambda_function" "slapman_worker_POST" {
  filename = "infrastructure/slapalicious-api.zip"

  //  filename         = "infrastructure/slapalicious-web.zip"
  function_name = "slapman_worker_POST"
  role          = "${aws_iam_role.iam_for_slapman.arn}"
  handler       = "worker-fn"

  //  handler          = "worker-exe.handle"
  //  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-web.zip"))}"
  //  memory_size      = "128"
  //  runtime          = "python3.6"
  source_code_hash = "${base64sha256(file("infrastructure/slapalicious-api.zip"))}"

  memory_size = "128"
  runtime     = "go1.x"
  timeout     = "300"

  environment {
    variables = {
      foo = "bar"
    }
  }
}
