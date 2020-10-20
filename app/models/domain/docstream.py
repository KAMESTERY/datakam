from typing import (
    List,
    Union
)

from app.models.common import (
    convert_json_to_realworld,
    convert_string_to_datetime
)
from app.models.domain.textblock import TextBlock
from app.models.domain.content import (
    Content,
    ContentDynaInOutInterface
)
from app.models.domain.content import (
    ENTITY_TYPE,
    NAMESPACE,
    CONTENTID,
    USERID,
    TAGS,
    SCORE,
    VERSION,
    CREATEDAT,
    UPDATEDAT,
    POSITION,
    TYPE
)
from app.models.domain.media import Media


class DocStream(Content, ContentDynaInOutInterface):
    item_stream: List[Union[Media,TextBlock]]

    def to_dynamo(self) -> dict:
        dyn_dict = dict()

        dyn_dict[ENTITY_TYPE] = "DocStream"
        dyn_dict[NAMESPACE] = self.namespace
        dyn_dict[CONTENTID] = self.content_id
        dyn_dict[USERID] = self.user_id
        dyn_dict[SCORE] = self.score
        dyn_dict[VERSION] = self.version
        dyn_dict[CREATEDAT] = convert_json_to_realworld(self.created_at)
        dyn_dict[UPDATEDAT] = convert_json_to_realworld(self.updated_at)

        return dyn_dict

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
