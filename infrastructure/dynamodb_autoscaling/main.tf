
variable "resource_id" {
  description = "The Table resource ID"
}

variable "role_arn" {
  description = "The Role ARN"
}

variable "max_capacity" {
  description = "Maximum Capacity"
  default = 20
}

variable "min_capacity" {
  description = "Minimum Capacity"
  default = 1
}

variable "index_name" {
  description = "Index Name"
}

//// TARGETS

// Tables

resource "aws_appautoscaling_target" "dynamodb_table_read_target" {
  max_capacity = "${var.max_capacity}"
  min_capacity = "${var.min_capacity}"
  resource_id = "table/${var.resource_id}"
  role_arn = "${var.role_arn}"
  scalable_dimension = "dynamodb:table:ReadCapacityUnits"
  service_namespace = "dynamodb"
}

resource "aws_appautoscaling_target" "dynamodb_table_write_target" {
  max_capacity = "${var.max_capacity}"
  min_capacity = "${var.min_capacity}"
  resource_id = "table/${var.resource_id}"
  role_arn = "${var.role_arn}"
  scalable_dimension = "dynamodb:table:WriteCapacityUnits"
  service_namespace = "dynamodb"
}

// Indices

resource "aws_appautoscaling_target" "dynamodb_index_read_target" {
  max_capacity = "${var.max_capacity}"
  min_capacity = "${var.min_capacity}"
  resource_id = "table/${var.resource_id}/index/${var.index_name}"
  role_arn = "${var.role_arn}"
  scalable_dimension = "dynamodb:index:ReadCapacityUnits"
  service_namespace = "dynamodb"
}

resource "aws_appautoscaling_target" "dynamodb_index_write_target" {
  max_capacity = "${var.max_capacity}"
  min_capacity = "${var.min_capacity}"
  resource_id = "table/${var.resource_id}/index/${var.index_name}"
  role_arn = "${var.role_arn}"
  scalable_dimension = "dynamodb:index:WriteCapacityUnits"
  service_namespace = "dynamodb"
}

//// POLICIES

// Tables

resource "aws_appautoscaling_policy" "dynamodb_table_read_policy" {
  name               = "DynamoDBReadCapacityUtilization:${aws_appautoscaling_target.dynamodb_table_read_target.resource_id}"
  policy_type        = "TargetTrackingScaling"
  resource_id        = "${aws_appautoscaling_target.dynamodb_table_read_target.resource_id}"
  scalable_dimension = "${aws_appautoscaling_target.dynamodb_table_read_target.scalable_dimension}"
  service_namespace  = "${aws_appautoscaling_target.dynamodb_table_read_target.service_namespace}"

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "DynamoDBReadCapacityUtilization"
    }

    scale_in_cooldown = 10
    scale_out_cooldown = 10

    target_value = 80
  }
}

resource "aws_appautoscaling_policy" "dynamodb_table_write_policy" {
  name               = "DynamoDBWriteCapacityUtilization:${aws_appautoscaling_target.dynamodb_table_write_target.resource_id}"
  policy_type        = "TargetTrackingScaling"
  resource_id        = "${aws_appautoscaling_target.dynamodb_table_write_target.resource_id}"
  scalable_dimension = "${aws_appautoscaling_target.dynamodb_table_write_target.scalable_dimension}"
  service_namespace  = "${aws_appautoscaling_target.dynamodb_table_write_target.service_namespace}"

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "DynamoDBWriteCapacityUtilization"
    }

    scale_in_cooldown = 10
    scale_out_cooldown = 10

    target_value = 80
  }
}

// Indices

resource "aws_appautoscaling_policy" "dynamodb_index_read_policy" {
  name               = "DynamoDBReadCapacityUtilization:${aws_appautoscaling_target.dynamodb_index_read_target.resource_id}"
  policy_type        = "TargetTrackingScaling"
  resource_id        = "${aws_appautoscaling_target.dynamodb_index_read_target.resource_id}"
  scalable_dimension = "${aws_appautoscaling_target.dynamodb_index_read_target.scalable_dimension}"
  service_namespace  = "${aws_appautoscaling_target.dynamodb_index_read_target.service_namespace}"

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "DynamoDBReadCapacityUtilization"
    }

    scale_in_cooldown = 10
    scale_out_cooldown = 10

    target_value = 80
  }
}

resource "aws_appautoscaling_policy" "dynamodb_index_write_policy" {
  name               = "DynamoDBWriteCapacityUtilization:${aws_appautoscaling_target.dynamodb_index_write_target.resource_id}"
  policy_type        = "TargetTrackingScaling"
  resource_id        = "${aws_appautoscaling_target.dynamodb_index_write_target.resource_id}"
  scalable_dimension = "${aws_appautoscaling_target.dynamodb_index_write_target.scalable_dimension}"
  service_namespace  = "${aws_appautoscaling_target.dynamodb_index_write_target.service_namespace}"

  target_tracking_scaling_policy_configuration {
    predefined_metric_specification {
      predefined_metric_type = "DynamoDBWriteCapacityUtilization"
    }

    scale_in_cooldown = 10
    scale_out_cooldown = 10

    target_value = 80
  }
}
