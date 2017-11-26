
resource "aws_iam_role" "iam_for_slapman" {
  name = "iam_for_slapman"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    },
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "dynamodb.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    },
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "s3.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    },
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "apigateway.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF
}

resource "aws_iam_role_policy" "iam_for_slapman-added-lambda-iam-role-policy" {
  name = "iam_for_slapman-added-lambda-iam-role-policy"
  role = "${aws_iam_role.iam_for_slapman.id}"
  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "dynamodb:GetItem",
        "dynamodb:BatchGetItem",
        "dynamodb:Query",
        "dynamodb:Scan",
        "dynamodb:PutItem",
        "dynamodb:UpdateItem",
        "dynamodb:DeleteItem",
        "dynamodb:BatchWriteItem",
        "dynamodb:GetRecords",
        "dynamodb:GetShardIterator",
        "dynamodb:DescribeTable",
        "dynamodb:DescribeLimits",
        "dynamodb:DescribeTimeToLive",
        "dynamodb:DescribeStream",
        "dynamodb:ListStreams",
        "dynamodb:TagResource",
        "dynamodb:UntagResource",
        "dynamodb:ListTagsOfResource"
      ],
      "Resource": [
        "${aws_dynamodb_table.game-scores-dynamodb-table.arn}",
        "${aws_dynamodb_table.game-scores-dynamodb-table.arn}/index/*",
        "${aws_dynamodb_table.user-table.arn}",
        "${aws_dynamodb_table.user-table.arn}/index/*",
        "${aws_dynamodb_table.userprofile-table.arn}",
        "${aws_dynamodb_table.userprofile-table.arn}/index/*",
        "${aws_dynamodb_table.usergroup-table.arn}",
        "${aws_dynamodb_table.usergroup-table.arn}/index/*",
        "${aws_dynamodb_table.things-dynamodb-table.arn}",
        "${aws_dynamodb_table.things-dynamodb-table.arn}/index/*",
        "${aws_dynamodb_table.data-dynamodb-table.arn}",
        "${aws_dynamodb_table.data-dynamodb-table.arn}/index/*"
      ],
      "Effect": "Allow",
      "Sid": ""
    },
    {
      "Action": [
        "cloudwatch:GetMetricStatistics",
        "cloudwatch:ListMetrics",
        "cloudwatch:PutMetricData",
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:DescribeLogStreams",
        "logs:PutSubscriptionFilter",
        "logs:PutLogEvents"
      ],
      "Effect": "Allow",
      "Resource": "arn:aws:logs:*:*:*"
    },
    {
      "Effect": "Allow",
      "Action": "s3:GetObject",
      "Resource": [
        "arn:aws:s3:::${var.web-bucket}/*"
      ]
    }
  ]
}
EOF
}
