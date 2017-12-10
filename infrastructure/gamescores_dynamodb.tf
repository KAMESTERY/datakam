resource "aws_dynamodb_table" "game-scores-dynamodb-table" {
  name = "GameScores"
  read_capacity = 1
  write_capacity = 1
  hash_key = "UserId"
  range_key = "GameTitle"

  attribute {
    name = "UserId"
    type = "S"
  }

  attribute {
    name = "GameTitle"
    type = "S"
  }

  attribute {
    name = "TopScore"
    type = "N"
  }

  ttl {
    attribute_name = "TimeToExist"
    enabled = false
  }

  global_secondary_index {
    name = "GameTitleIndex"
    hash_key = "GameTitle"
    range_key = "TopScore"
    write_capacity = 1
    read_capacity = 1
    projection_type = "INCLUDE"
    non_key_attributes = [
      "UserId"]
  }

  tags {
    Name = "game-scores-dynamodb-table-1"
    Environment = "production"
  }
}


