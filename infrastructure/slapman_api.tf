
# Now, we need an API to expose those functions publicly
resource "aws_api_gateway_rest_api" "slapalicious_api" {
  name = "SLAPALICIOUS API"
}

#### PATH: /slapalicious

# The API requires at least one "endpoint", or "resource" in AWS terminology.
# The endpoint created here is: /slapalicious
resource "aws_api_gateway_resource" "slapalicious_api_res_slapalicious" {
  rest_api_id = "${aws_api_gateway_rest_api.slapalicious_api.id}"
  parent_id   = "${aws_api_gateway_rest_api.slapalicious_api.root_resource_id}"
  path_part   = "slapalicious"
}

# Until now, the resource created could not respond to anything. We must set up
# a HTTP method (or verb) for that!

# This is the code for method OPTIONS /slapalicious, that will talk to the first lambda
module "slapalicious_options" {
  source = "./api_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapalicious_api.id}"
  resource_id = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.id}"
  method = "OPTIONS"
  path = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.path}"
  lambda = "${aws_lambda_function.slapman_slapalicious_OPTIONS.function_name}"
  region = "${var.aws_region}"
  account_id = "${var.aws_account_id}"
}

# This is the code for method GET /slapalicious, that will talk to the first lambda
module "slapalicious_get" {
  source      = "./api_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapalicious_api.id}"
  resource_id = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.id}"
  method      = "GET"
  path        = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.path}"
  lambda      = "${aws_lambda_function.slapman_slapalicious_GET.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

# This is the code for method POST /slapalicious, that will talk to the second lambda
module "slapalicious_post" {
  source      = "./api_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapalicious_api.id}"
  resource_id = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.id}"
  method      = "POST"
  path        = "${aws_api_gateway_resource.slapalicious_api_res_slapalicious.path}"
  lambda      = "${aws_lambda_function.slapman_slapalicious_POST.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

# We can deploy the API now! (i.e. make it publicly available)
resource "aws_api_gateway_deployment" "slapalicious_api_deployment" {
  rest_api_id = "${aws_api_gateway_rest_api.slapalicious_api.id}"
  stage_name  = "production"
  description = "Deploy methods: ${module.slapalicious_options.http_method} ${module.slapalicious_get.http_method} ${module.slapalicious_post.http_method}"
}

