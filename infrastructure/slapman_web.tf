# Now, we need an API to expose those functions publicly
resource "aws_api_gateway_rest_api" "slapman_web" {
  name = "SLAPMAN WEB"
}

#### PATH: /slapman

# The API requires at least one "endpoint", or "resource" in AWS terminology.
# The endpoint created here is: /slapman
resource "aws_api_gateway_resource" "slapman_web_res_slapman" {
  rest_api_id = "${aws_api_gateway_rest_api.slapman_web.id}"
  parent_id = "${aws_api_gateway_rest_api.slapman_web.root_resource_id}"
  path_part = "{proxy+}"
}

# Until now, the resource created could not respond to anything. We must set up
# a HTTP method (or verb) for that!

# This is the code for method ANY /slapman, that will talk to the first lambda
module "slapman_web" {
  source = "./web_method"
  rest_api_id = "${aws_api_gateway_rest_api.slapman_web.id}"
  resource_id = "${aws_api_gateway_resource.slapman_web_res_slapman.id}"
  path = "${aws_api_gateway_resource.slapman_web_res_slapman.path}"
  lambda = "${aws_lambda_function.slapman_web.function_name}"
  region = "${var.aws_region}"
  account_id = "${var.aws_account_id}"
}

# We can deploy the API now! (i.e. make it publicly available)
resource "aws_api_gateway_deployment" "slapman_web_deployment" {
  rest_api_id = "${aws_api_gateway_rest_api.slapman_web.id}"
  stage_name = "production"
}

output "slapman-web-url" {
  value = "https://${aws_api_gateway_rest_api.slapman_web.id}.execute-api.${var.aws_region}.amazonaws.com/production/${aws_api_gateway_resource.slapman_web_res_slapman.path_part}"
}
