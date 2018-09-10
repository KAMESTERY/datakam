# Now, we need an API to expose those functions publicly
resource "aws_api_gateway_rest_api" "slapman_worker" {
  name = "SLAPMAN WORKER"
}

#### PATH: /slapman

# The API requires at least one "endpoint", or "resource" in AWS terminology.
# The endpoint created here is: /slapman
resource "aws_api_gateway_resource" "slapman_worker_res_slapman" {
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  parent_id   = "${aws_api_gateway_rest_api.slapman_worker.root_resource_id}"

  path_part = "worker"

  #path_part = "{proxy+}"
}

# Until now, the resource created could not respond to anything. We must set up
# a HTTP method (or verb) for that!

# This is the code for method ANY /slapman, that will talk to the first lambda
module "slapman_worker_options" {
  source      = "./web_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  resource_id = "${aws_api_gateway_resource.slapman_worker_res_slapman.id}"
  method      = "OPTIONS"
  path        = "${aws_api_gateway_resource.slapman_worker_res_slapman.path}"
  lambda      = "${aws_lambda_function.slapman_worker_OPTIONS.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

module "slapman_worker_get" {
  source      = "./web_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  resource_id = "${aws_api_gateway_resource.slapman_worker_res_slapman.id}"
  method      = "GET"
  path        = "${aws_api_gateway_resource.slapman_worker_res_slapman.path}"
  lambda      = "${aws_lambda_function.slapman_worker_GET.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

module "slapman_worker_post" {
  source      = "./web_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  resource_id = "${aws_api_gateway_resource.slapman_worker_res_slapman.id}"
  method      = "POST"
  path        = "${aws_api_gateway_resource.slapman_worker_res_slapman.path}"
  lambda      = "${aws_lambda_function.slapman_worker_POST.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

# We can deploy the API now! (i.e. make it publicly available)
resource "aws_api_gateway_deployment" "slapman_worker_deployment" {
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  stage_name  = "production"
  description = "Deploy methods: ${module.slapman_worker_options.http_method} ${module.slapman_worker_get.http_method} ${module.slapman_worker_post.http_method}"
}

resource "aws_api_gateway_method_settings" "slapman_worker_settings" {
  rest_api_id = "${aws_api_gateway_rest_api.slapman_worker.id}"
  stage_name  = "production"
  method_path = "${aws_api_gateway_resource.slapman_worker_res_slapman.path_part}/*"

  settings {
    metrics_enabled = true

    #logging_level   = "INFO"
  }

  depends_on = ["aws_api_gateway_deployment.slapman_worker_deployment"]
}

output "slapman-worker-url" {
  value = "https://${aws_api_gateway_rest_api.slapman_worker.id}.execute-api.${var.aws_region}.amazonaws.com/production/${aws_api_gateway_resource.slapman_worker_res_slapman.path_part}"
}

output "slapman-worker-invoke-url" {
  value = "${aws_api_gateway_deployment.slapman_worker_deployment.invoke_url}"
}
