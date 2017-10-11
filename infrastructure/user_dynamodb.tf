resource "aws_dynamodb_table" "user-table" {
  name = "User"
  read_capacity = 20
  write_capacity = 20
  hash_key = "UserID"
  range_key = "Email"

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

  ttl {
    attribute_name = "TimeToExist"
    enabled = false
  }

  global_secondary_index {
    name = "EmailIndex"
    hash_key = "Email"
    range_key = "Confirmed"
    write_capacity = 10
    read_capacity = 10
    projection_type = "INCLUDE"
    non_key_attributes = ["UserID"]
  }

  global_secondary_index {
    name = "RoleIndex"
    hash_key = "Role"
    range_key = "Confirmed"
    write_capacity = 10
    read_capacity = 10
    projection_type = "INCLUDE"
    non_key_attributes = ["UserID"]
  }

  global_secondary_index {
    name = "UsernameIndex"
    hash_key = "Username"
    write_capacity = 10
    read_capacity = 10
    projection_type = "INCLUDE"
    non_key_attributes = ["UserID"]
  }

  global_secondary_index {
    name = "PasswordHashIndex"
    hash_key = "PasswordHash"
    write_capacity = 10
    read_capacity = 10
    projection_type = "INCLUDE"
    non_key_attributes = ["UserID"]
  }

  tags {
    Name = "user-table-1"
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
