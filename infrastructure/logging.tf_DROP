resource "aws_lambda_alias" "slapman_slapalicious_GET_alias" {
  name             = "slapmanslapaliciousGETalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.slapman_slapalicious_GET.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_slapman_slapalicious_GET" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.slapman_slapalicious_GET.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.slapman_slapalicious_GET_alias.name}"
}

resource "aws_lambda_alias" "slapman_slapalicious_POST_alias" {
  name             = "slapmanslapaliciousPOSTalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.slapman_slapalicious_POST.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_slapman_slapalicious_POST" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.slapman_slapalicious_POST.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.slapman_slapalicious_POST_alias.name}"
}

//resource "aws_lambda_alias" "slapman_slapman_WEB_alias" {
//  name             = "slapmanslapmanWEBalias"
//  description      = "a sample description"
//  function_name    = "${aws_lambda_function.slapman_web.function_name}"
//  function_version = "$LATEST"
//}
//
//resource "aws_lambda_permission" "allow_cloudwatch_slapman_slapman_WEB" {
//  statement_id   = "AllowExecutionFromCloudWatch"
//  action         = "lambda:InvokeFunction"
//  function_name  = "${aws_lambda_function.slapman_web.function_name}"
//  principal      = "events.amazonaws.com"
//  source_account = "${var.aws_account_id}"
//  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
//  qualifier      = "${aws_lambda_alias.slapman_slapman_WEB_alias.name}"
//}

resource "aws_lambda_alias" "slapman_worker_GET_alias" {
  name             = "slapmanworkerGETalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.slapman_worker_GET.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_slapman_worker_GET" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.slapman_worker_GET.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.slapman_worker_GET_alias.name}"
}

resource "aws_lambda_alias" "slapman_worker_POST_alias" {
  name             = "slapmanworkerPOSTalias"
  description      = "a sample description"
  function_name    = "${aws_lambda_function.slapman_worker_POST.function_name}"
  function_version = "$LATEST"
}

resource "aws_lambda_permission" "allow_cloudwatch_slapman_worker_POST" {
  statement_id   = "AllowExecutionFromCloudWatch"
  action         = "lambda:InvokeFunction"
  function_name  = "${aws_lambda_function.slapman_worker_POST.function_name}"
  principal      = "events.amazonaws.com"
  source_account = "${var.aws_account_id}"
  source_arn     = "arn:aws:events:${var.aws_region}:${var.aws_account_id}:rule/RunDaily"
  qualifier      = "${aws_lambda_alias.slapman_worker_POST_alias.name}"
}
