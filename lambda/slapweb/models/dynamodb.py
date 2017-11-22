
import logging

from datetime import datetime
from pynamodb.models import Model
from pynamodb.indexes import (
    GlobalSecondaryIndex,
    AllProjection
)
from pynamodb.attributes import (
    BooleanAttribute,
    NumberAttribute,
    UnicodeAttribute,
    UTCDateTimeAttribute
)

logging.basicConfig()
log = logging.getLogger("pynamodb")
log.setLevel(logging.DEBUG)
log.propagate = True

class PartialIndex(GlobalSecondaryIndex):
    class Meta:
        # Specifies the write capacity
        write_capacity_units = 10
        # Specifies the read capacity
        read_capacity_units = 10
        # All attributes are projected
        projection = AllProjection()

class EmailIndex(PartialIndex):
    index_name = 'EmailIndex'
    email = UnicodeAttribute(attr_name='Email', hash_key=True)
    confirmed = BooleanAttribute(attr_name='Confirmed', range_key=True)

class User(Model):
    """
    A DynamoDB Slapman User
    """
    class Meta:
        table_name = "User"
        # Specifies the write capacity
        write_capacity_units = 10
        # Specifies the read capacity
        read_capacity_units = 10
    user_id = UnicodeAttribute(attr_name='UserID',hash_key=True)
    email = UnicodeAttribute(attr_name='Email', range_key=True)
    email_index = EmailIndex()
    username = UnicodeAttribute(attr_name='Username')
    role = NumberAttribute(attr_name='Role', default=0)
    confirmed = BooleanAttribute('Confirmed', default=False)
    password_hash = UnicodeAttribute(attr_name='PasswordHash')
    last_seen = UnicodeAttribute(attr_name='LastSeen')
    # last_seen = UTCDateTimeAttribute(attr_name='LastSeen', default=datetime.now())
