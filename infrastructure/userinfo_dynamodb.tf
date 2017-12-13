resource "aws_dynamodb_table" "user-table" {
  name           = "User"
  read_capacity  = 1
  write_capacity = 1
  hash_key       = "UserID"
  range_key      = "Email"

  attribute {
    name = "UserID"
    type = "S"
  }

  attribute {
    name = "Email"
    type = "S"
  }

  attribute {
    name = "Username"
    type = "S"
  }

  attribute {
    name = "Role"
    type = "N"
  }

  attribute {
    name = "Confirmed"
    type = "N"
  }

  attribute {
    name = "PasswordHash"
    type = "S"
  }

  attribute {
    name = "LastSeen"
    type = "S"
  }

  ttl {
    attribute_name = "TimeToExist"
    enabled        = false
  }

  global_secondary_index {
    name            = "EmailIndex"
    hash_key        = "Email"
    range_key       = "Confirmed"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "RoleIndex"
    hash_key        = "Role"
    range_key       = "Confirmed"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "UsernameIndex"
    hash_key        = "Username"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "PasswordHashIndex"
    hash_key        = "PasswordHash"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "LastSeenIndex"
    hash_key        = "LastSeen"
    range_key       = "Email"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  tags {
    Name        = "user-table-1"
    Environment = "production"
  }
}

// https://bitbucket.org/manuinfo/free_kamite/src/8e3ff34af4c5d82c361f5d429c1f7f630f1bf5f1/src/models.py?at=master&fileviewer=file-view-default
// User
//id = db.Column(db.Integer, primary_key=True)
//email = db.Column(db.String(64), unique=True, index=True)
//username = db.Column(db.String(64), unique=True, index=True)
//role_id = db.Column(db.Integer, db.ForeignKey('roles.id'))
//confirmed = db.Column(db.Boolean, default=False)
//password_hash = db.Column(db.String(128))

//things = db.relationship('Thing', backref='user', lazy='dynamic')

resource "aws_dynamodb_table" "userprofile-table" {
  name           = "UserProfile"
  read_capacity  = 1
  write_capacity = 1
  hash_key       = "UserID"
  range_key      = "Location"

  attribute {
    name = "UserID"
    type = "S"
  }

  attribute {
    name = "AvatarHash"
    type = "S"
  }

  attribute {
    name = "Name"
    type = "S"
  }

  attribute {
    name = "Age"
    type = "N"
  }

  attribute {
    name = "AboutMe"
    type = "S"
  }

  attribute {
    name = "Location"
    type = "S"
  }

  attribute {
    name = "MemberSince"
    type = "S"
  }

  ttl {
    attribute_name = "TimeToExist"
    enabled        = false
  }

  global_secondary_index {
    name            = "LocationIndex"
    hash_key        = "Location"
    range_key       = "Age"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "AvatarHashIndex"
    hash_key        = "AvatarHash"
    range_key       = "Age"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  global_secondary_index {
    name            = "NameIndex"
    hash_key        = "Name"
    range_key       = "AboutMe"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }

  //  This one exceeded the limit reason why it was added as range_key for Name GSI
  //  global_secondary_index {
  //    name = "AboutMeIndex"
  //    hash_key = "AboutMe"
  //    write_capacity = 10
  //    read_capacity = 10
  //    projection_type = "INCLUDE"
  //    non_key_attributes = ["UserID"]
  //  }

  global_secondary_index {
    name            = "MemberSinceIndex"
    hash_key        = "MemberSince"
    range_key       = "Location"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "UserID",
    ]
  }
  tags {
    Name        = "userprofile-table-1"
    Environment = "production"
  }
}

// https://bitbucket.org/manuinfo/free_kamite/src/8e3ff34af4c5d82c361f5d429c1f7f630f1bf5f1/src/models.py?at=master&fileviewer=file-view-default
// UserProfile
//avatar_hash = db.Column(db.String(32))
//name = db.Column(db.String(64))
//about_me = db.Column(db.Text())
//location = db.Column(db.String(64))
//member_since = db.Column(db.DateTime(), default=datetime.utcnow)
//last_seen = db.Column(db.DateTime(), default=datetime.utcnow)

resource "aws_dynamodb_table" "usergroup-table" {
  name           = "UserGroups"
  read_capacity  = 1
  write_capacity = 1
  hash_key       = "GroupID"
  range_key      = "UserID"

  attribute {
    name = "GroupID"
    type = "S"
  }

  attribute {
    name = "UserID"
    type = "S"
  }

  attribute {
    name = "Name"
    type = "S"
  }

  ttl {
    attribute_name = "TimeToExist"
    enabled        = false
  }

  global_secondary_index {
    name            = "UserIDIndex"
    hash_key        = "UserID"
    range_key       = "Name"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "GroupID",
    ]
  }

  global_secondary_index {
    name            = "NameIndex"
    hash_key        = "Name"
    range_key       = "UserID"
    write_capacity  = 1
    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "GroupID",
    ]
  }

  tags {
    Name        = "usergroup-table-1"
    Environment = "production"
  }
}
