
resource "aws_lambda_alias" "fn-claws_hello_GET_alias" {
  name             = "fnclawshelloGETalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.fn-claws_hello_GET.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_fn-claws_hello_GET" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.fn-claws_hello_GET.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.fn-claws_hello_GET_alias.name}"
}

resource "aws_lambda_alias" "fn-claws_hello_POST_alias" {
  name             = "fnclawshelloPOSTalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.fn-claws_hello_POST.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_fn-claws_hello_POST" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.fn-claws_hello_POST.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.fn-claws_hello_POST_alias.name}"
}

