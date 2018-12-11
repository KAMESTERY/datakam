resource "aws_dynamodb_table" "things-dynamodb-table" {
  name         = "Things"
  billing_mode = "PAY_PER_REQUEST"

  //  read_capacity  = 1
  //  write_capacity = 1
  hash_key = "Name"

  range_key = "ThingID"

  attribute {
    name = "Name"
    type = "S"
  }

  attribute {
    name = "UserID"
    type = "S"
  }

  attribute {
    name = "ThingID"
    type = "S"
  }

  attribute {
    name = "Version"
    type = "N"
  }

  attribute {
    name = "Score"
    type = "N"
  }

  attribute {
    name = "CreatedAt"
    type = "S"
  }

  attribute {
    name = "UpdatedAt"
    type = "S"
  }

  //  attribute {
  //    name = "DataIDs"
  //    type = "S"0000000000
  //  }

  ttl {
    attribute_name = "TimeToExist"
    enabled        = false
  }
  global_secondary_index {
    name      = "UserIDIndex"
    hash_key  = "UserID"
    range_key = "Score"

    //    write_capacity  = 1
    //    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "Name",
    ]
  }
  global_secondary_index {
    name      = "ThingIDIndex"
    hash_key  = "ThingID"
    range_key = "Version"

    //    write_capacity  = 1
    //    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "Name",
    ]
  }
  global_secondary_index {
    name      = "CreatedAtIndex"
    hash_key  = "CreatedAt"
    range_key = "Name"

    //    write_capacity  = 1
    //    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "Name",
    ]
  }
  global_secondary_index {
    name      = "UpdatedAtIndex"
    hash_key  = "UpdatedAt"
    range_key = "Name"

    //    write_capacity  = 1
    //    read_capacity   = 1
    projection_type = "INCLUDE"

    non_key_attributes = [
      "Name",
    ]
  }

  //  global_secondary_index {
  //    name            = "DataIDsIndex"
  //    hash_key        = "DataIDs"
  //    range_key       = "Name"
  //    write_capacity  = 1
  //    read_capacity   = 1
  //    projection_type = "INCLUDE"
  //
  //    non_key_attributes = [
  //      "Name",
  //    ]
  //  }

  tags {
    Name        = "things-table-1"
    Environment = "production"
  }
}

// https://bitbucket.org/manuinfo/free_kamite/src/8e3ff34af4c5d82c361f5d429c1f7f630f1bf5f1/src/models.py?at=master&fileviewer=file-view-default
// Things
//id = db.Column(db.Integer, primary_key=True)
//name = db.Column(db.String(64), index=True)
//score = db.Column(db.Integer)
//version = db.Column(db.Integer)
//user_id = db.Column(db.Integer, db.ForeignKey('users.id'))
//created_at = db.Column(db.DateTime, default=datetime.utcnow)
//updated_at = db.Column(db.DateTime, default=datetime.utcnow, onupdate=datetime.utcnow)

resource "aws_dynamodb_table" "data-dynamodb-table" {
  name         = "Data"
  billing_mode = "PAY_PER_REQUEST"

  //  read_capacity  = 1
  //  write_capacity = 1

  hash_key  = "ThingID"
  range_key = "DataID"
  attribute {
    name = "DataID"
    type = "S"
  }
  attribute {
    name = "ThingID"
    type = "S"
  }

  //  attribute {
  //    name = "Key"
  //    type = "S"
  //  }
  //
  //  attribute {
  //    name = "Value"
  //    type = "S"
  //  }

  ttl {
    attribute_name = "TimeToExist"
    enabled        = false
  }

  //  global_secondary_index {
  //    name            = "ThingIDIndex"
  //    hash_key        = "ThingID"
  //    range_key       = "Key"
  //    write_capacity  = 1
  //    read_capacity   = 1
  //    projection_type = "INCLUDE"
  //
  //    non_key_attributes = [
  //      "DataID",
  //    ]
  //  }
  //
  //  global_secondary_index {
  //    name            = "KeyIndex"
  //    hash_key        = "Key"
  //    range_key       = "Value"
  //    write_capacity  = 1
  //    read_capacity   = 1
  //    projection_type = "INCLUDE"
  //
  //      non_key_attributes = [
  //        "DataID",
  //      ]
  //    }

  tags {
    Name        = "data-table-1"
    Environment = "production"
  }
}

// Data


//data = db.relationship('Datum', backref='thing', lazy='dynamic', cascade="all, delete-orphan")
//# comments = db.relationship('Comment', backref='thing', lazy='dynamic', cascade="all, delete-orphan")
//# tags = db.relationship('Tag', secondary=tagged_things, backref='thing', lazy='dynamic')
//comments = db.relationship('Comment', backref='thing', lazy='subquery', cascade="all, delete-orphan")
//tags = db.relationship('Tag', secondary=tagged_things, backref='thing', lazy='subquery')
//# https://gist.github.com/techniq/5174410


//id = db.Column(db.Integer, primary_key=True)
//thing_id = db.Column(db.Integer, db.ForeignKey('things.id'))
//key = db.Column(db.String(64), index=True)
//value = db.Column(db.BLOB)

