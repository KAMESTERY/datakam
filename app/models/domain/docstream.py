
from typing import (
    List,
    Union
)

from app.models.common import (
    convert_json_to_realworld,
    convert_string_to_datetime
)
from app.models.domain.content import (
    Content,
    ContentDynaInOutInterface
)
from app.models.domain.content import (
    ENTITY_TYPE,
    DOCSTREAM_ENTITY,
    NAMESPACE,
    CONTENTID,
    USERID,
    SCORE,
    VERSION,
    CREATEDAT,
    UPDATEDAT
)
from app.models.domain.media import Media
from app.models.domain.textblock import TextBlock


class DocStream(Content, ContentDynaInOutInterface):
    item_stream: List[Union[Media,TextBlock]] = []

    def get_entity_type(self):
        return DOCSTREAM_ENTITY

    def get_key(self):
        key = dict()

        key[NAMESPACE] = self.namespace
        key[CONTENTID] = self.content_id

        return key

    def to_dynamo(self) -> List[dict]:
        list_of_dyn_dicts = [itms.to_dynamo() for itms in self.item_stream]

        dyn_dict = dict()

        dyn_dict[ENTITY_TYPE] = DOCSTREAM_ENTITY
        dyn_dict[NAMESPACE] = self.namespace
        dyn_dict[CONTENTID] = self.content_id
        dyn_dict[USERID] = self.user_id
        dyn_dict[SCORE] = self.score
        dyn_dict[VERSION] = self.version
        dyn_dict[CREATEDAT] = convert_json_to_realworld(self.created_at)
        dyn_dict[UPDATEDAT] = convert_json_to_realworld(self.updated_at)

        list_of_dyn_dicts.append(dyn_dict)

        return list_of_dyn_dicts

    @classmethod
    def from_dynamo(cls, item: dict) -> 'DocStream':
        ds = cls(
            namespace=item[NAMESPACE],
            content_id=item[CONTENTID],
            user_id=item[USERID],
            score=item[SCORE],
            version=item[VERSION],
            created_at=convert_string_to_datetime(item[CREATEDAT]),
            updated_at=convert_string_to_datetime(item[UPDATEDAT]),
        )

        return ds
