
# Now, we need an API to expose those functions publicly
resource "aws_api_gateway_rest_api" "pyfun_api" {
  name = "PYFUN API"
}

#### PATH: /pyfun

# The API requires at least one "endpoint", or "resource" in AWS terminology.
# The endpoint created here is: /pyfun
resource "aws_api_gateway_resource" "pyfun_api_res_pyfun" {
  rest_api_id = "${aws_api_gateway_rest_api.pyfun_api.id}"
  parent_id   = "${aws_api_gateway_rest_api.pyfun_api.root_resource_id}"
  path_part   = "pyfun"
}

module "pyfun_get" {
  source      = "./api_method"
  rest_api_id = "${aws_api_gateway_rest_api.pyfun_api.id}"
  resource_id = "${aws_api_gateway_resource.pyfun_api_res_pyfun.id}"
  method      = "GET"
  path        = "${aws_api_gateway_resource.pyfun_api_res_pyfun.path}"
  lambda      = "${aws_lambda_function.fn-claws_pyfun_GET.function_name}"
  region      = "${var.aws_region}"
  account_id  = "${var.aws_account_id}"
}

# We can deploy the API now! (i.e. make it publicly available)
resource "aws_api_gateway_deployment" "pyfun_api_deployment" {
  rest_api_id = "${aws_api_gateway_rest_api.pyfun_api.id}"
  stage_name  = "production"
  description = "Deploy methods: ${module.pyfun_get.http_method}"
}

