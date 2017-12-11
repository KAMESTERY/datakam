
// User Table

module "user-table_EmailIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.user-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 2
  min_capacity = 1
  index_name = "EmailIndex"
}

module "user-table_RoleIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.user-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 2
  min_capacity = 1
  index_name = "RoleIndex"
}

module "user-table_UsernameIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.user-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 2
  min_capacity = 1
  index_name = "UsernameIndex"
}

module "user-table_PasswordHashIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.user-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 2
  min_capacity = 1
  index_name = "PasswordHashIndex"
}

module "user-table_LastSeenIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.user-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 2
  min_capacity = 1
  index_name = "LastSeenIndex"
}

// User Profile Table

module "userprofile-table_LocationIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.userprofile-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "LocationIndex"
}

module "userprofile-table_AvatarHashIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.userprofile-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "AvatarHashIndex"
}

module "userprofile-table_NameIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.userprofile-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "NameIndex"
}

module "userprofile-table_MemberSinceIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.userprofile-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "MemberSinceIndex"
}

// User Group Table

module "usergroup-table_UserIDIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.usergroup-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "UserIDIndex"
}

module "usergroup-table_NameIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.usergroup-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  index_name = "NameIndex"
}

// Things Table

module "things-dynamodb-table_UserIDIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.things-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 4
  min_capacity = 1
  index_name = "UserIDIndex"
}

module "things-dynamodb-table_NameIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.things-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 4
  min_capacity = 1
  index_name = "NameIndex"
}

module "things-dynamodb-table_CreatedAtIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.things-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 4
  min_capacity = 1
  index_name = "CreatedAtIndex"
}

module "things-dynamodb-table_UpdatedAtIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.things-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 4
  min_capacity = 1
  index_name = "UpdatedAtIndex"
}

// Data Table

module "data-dynamodb-table_ThingIDIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.data-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 16
  min_capacity = 1
  index_name = "ThingIDIndex"
}

module "data-dynamodb-table_KeyIndex_autoscaling" {
  source = "./dynamodb_autoscaling"
  resource_id = "${aws_dynamodb_table.data-dynamodb-table.name}"
  role_arn = "${aws_iam_role.iam_for_slapman.arn}"
  max_capacity = 16
  min_capacity = 1
  index_name = "KeyIndex"
}
