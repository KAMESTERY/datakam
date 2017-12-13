variable "rest_api_id" {
  description = "The ID of the associated REST API"
}

variable "resource_id" {
  description = "The API resource ID"
}

variable "method" {
  description = "The HTTP method"
  default     = "ANY"
}

variable "path" {
  description = "The API resource path"
}

variable "lambda" {
  description = "The lambda name to invoke"
}

variable "region" {
  description = "The AWS region, e.g., eu-west-1"
}

variable "account_id" {
  description = "The AWS account ID"
}

variable "api_gateway_stage" {
  default = "production"
}

# Example: request for GET /slapman
resource "aws_api_gateway_method" "proxy" {
  rest_api_id      = "${var.rest_api_id}"
  resource_id      = "${var.resource_id}"
  http_method      = "${var.method}"
  authorization    = "NONE"
  api_key_required = false
}

# Example: GET /slapman => POST lambda
resource "aws_api_gateway_integration" "proxy" {
  rest_api_id = "${var.rest_api_id}"
  resource_id = "${var.resource_id}"
  http_method = "${aws_api_gateway_method.proxy.http_method}"
  type        = "AWS_PROXY"
  uri         = "arn:aws:apigateway:${var.region}:lambda:path/2015-03-31/functions/arn:aws:lambda:${var.region}:${var.account_id}:function:${var.lambda}/invocations"

  # AWS lambdas can only be invoked with the POST method
  integration_http_method = "POST"
}

# lambda => GET response
resource "aws_api_gateway_method_response" "proxy" {
  rest_api_id = "${var.rest_api_id}"
  resource_id = "${var.resource_id}"
  http_method = "${aws_api_gateway_integration.proxy.http_method}"
  status_code = "200"

  response_models = {
    "application/json" = "Empty"
  }

  response_parameters = {
    "method.response.header.Access-Control-Allow-Headers" = true
    "method.response.header.Access-Control-Allow-Methods" = true
    "method.response.header.Access-Control-Allow-Origin"  = true
  }
}

# Response for: GET /slapman
resource "aws_api_gateway_integration_response" "proxy" {
  rest_api_id = "${var.rest_api_id}"
  resource_id = "${var.resource_id}"
  http_method = "${aws_api_gateway_method_response.proxy.http_method}"
  status_code = "${aws_api_gateway_method_response.proxy.status_code}"

  response_templates = {
    "application/json" = ""
  }

  response_parameters = {
    "method.response.header.Access-Control-Allow-Headers" = "'Content-Type,X-Amz-Date,Authorization,X-Api-Key,X-Amz-Security-Token'"
    "method.response.header.Access-Control-Allow-Methods" = "'POST,OPTIONS,GET,PUT,PATCH,DELETE'"

    #"method.response.header.Access-Control-Allow-Origin" = "'http://localhost:1818'"
    "method.response.header.Access-Control-Allow-Origin" = "'*'"
  }
}

resource "aws_lambda_permission" "allow_api_gateway_lambda_proxy" {
  function_name = "${var.lambda}"
  statement_id  = "AllowExecutionFromApiGateway"
  action        = "lambda:InvokeFunction"
  principal     = "apigateway.amazonaws.com"
  source_arn    = "arn:aws:execute-api:${var.region}:${var.account_id}:${var.rest_api_id}/*/*/*"
}

output "http_method" {
  value = "${aws_api_gateway_integration_response.proxy.http_method}"
}
