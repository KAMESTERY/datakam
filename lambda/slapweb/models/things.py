
from pynamodb.attributes import (
    BooleanAttribute,
    NumberAttribute,
    UnicodeAttribute,
)

from . import (
    PartialModel,
    PartialIndex
)

################ Thing Model and Indices

class UserIDIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'UserIDIndex'
    user_id = UnicodeAttribute(attr_name='UserID', hash_key=True)
    score = NumberAttribute(attr_name='Score', range_key=True)


class NameIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'NameIndex'
    name = UnicodeAttribute(attr_name='Name', hash_key=True)
    version = NumberAttribute(attr_name='Version', range_key=True)


class CreatedAtIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'CreatedAtIndex'
    created_at = UnicodeAttribute(attr_name='CreatedAt', hash_key=True)
    name = UnicodeAttribute(attr_name='Name', range_key=True)


class UpdatedAtIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'UpdatedAtIndex'
    created_at = UnicodeAttribute(attr_name='UpdatedAt', hash_key=True)
    name = UnicodeAttribute(attr_name='Name', range_key=True)


class Thing(PartialModel):
    """
    A DynamoDB Slapman Thing
    """
    class Meta(PartialModel.Meta):
        table_name = "Things"
    thing_id = UnicodeAttribute(attr_name='ThingID', hash_key=True)
    user_id = UnicodeAttribute(attr_name='UserID', range_key=True)
    user_id_index = UserIDIndex()
    name = UnicodeAttribute(attr_name='Name')
    name_index = NameIndex()
    version = NumberAttribute(attr_name='Version', default=0)
    score = NumberAttribute(attr_name='Score', default=0)
    created_at = UnicodeAttribute(attr_name='CreatedAt')
    created_at_index = CreatedAtIndex()
    updated_at = UnicodeAttribute(attr_name='UpdatedAt')
    updated_at_index = UpdatedAtIndex()

################ Data Model and Indices

class ThingIDIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'ThingIDIndex'
    thing_id = UnicodeAttribute(attr_name='ThingID', hash_key=True)
    key = UnicodeAttribute(attr_name='Key', range_key=True)


class KeyIndex(PartialIndex):
    class Meta(PartialIndex.Meta):
        index_name = 'KeyIndex'
    key = UnicodeAttribute(attr_name='Key', hash_key=True)
    value = UnicodeAttribute(attr_name='Value', range_key=True)


class Data(PartialModel):
    """
    A DynamoDB Slapman Data
    """
    class Meta(PartialModel.Meta):
        table_name = "Data"
    data_id = UnicodeAttribute(attr_name='DataID', hash_key=True)
    thing_id = UnicodeAttribute(attr_name='Thing', range_key=True)
    thing_id_index = ThingIDIndex()
    key = UnicodeAttribute(attr_name='Key')
    key_index = KeyIndex()
    value = UnicodeAttribute(attr_name='Value')
