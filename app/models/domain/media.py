
from typing import List

from pydantic import (
    EmailStr,
    HttpUrl
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin,
    convert_json_to_realworld,
    convert_string_to_datetime
)
from app.models.domain import (
    EXAMPLE_CONTENT_ID,
    EXAMPLE_EMAIL,
    EXAMPLE_TAGS,
    EXAMPLE_CHILD_ID,
    EXAMPLE_NUMBER,
    EXAMPLE_MEDIA_URL,
)
from app.models.domain.content import (
    Content,
    ContentDynaInOutInterface,
    ContentDynaUpdateInterface
)
from app.models.domain.content import (
    ENTITY_TYPE,
    MEDIA_ENTITY,
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

FILEURL = "FileUrl"


class Media(DateTimeModelMixin, ModelConfigMixin, ContentDynaInOutInterface, ContentDynaUpdateInterface):
    parentdocument_id: str = EXAMPLE_CONTENT_ID
    type: int = EXAMPLE_NUMBER
    media_id: str = EXAMPLE_CHILD_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    tags: List[str] = EXAMPLE_TAGS
    score: int = EXAMPLE_NUMBER
    version: int = EXAMPLE_NUMBER
    position: int = EXAMPLE_NUMBER
    file_url: HttpUrl = EXAMPLE_MEDIA_URL

    def get_entity_type(self):
        return MEDIA_ENTITY

    def get_key(self):
        key = dict()

        key[NAMESPACE] = self.parentdocument_id
        key[CONTENTID] = self.media_id

        return key

    def to_dynamo(self) -> dict:
        dyn_dict = dict()

        dyn_dict[ENTITY_TYPE] = MEDIA_ENTITY
        dyn_dict[NAMESPACE] = self.parentdocument_id
        dyn_dict[TYPE] = self.type
        dyn_dict[CONTENTID] = self.media_id
        dyn_dict[USERID] = self.user_id
        dyn_dict[TAGS] = self.tags
        dyn_dict[SCORE] = self.score
        dyn_dict[VERSION] = self.version
        dyn_dict[POSITION] = self.position
        dyn_dict[FILEURL] = self.file_url
        dyn_dict[CREATEDAT] = convert_json_to_realworld(self.created_at)
        dyn_dict[UPDATEDAT] = convert_json_to_realworld(self.updated_at)

        return dyn_dict

    def to_dynamo_update(self) -> dict:
        dyn_dict = dict()

        dyn_dict[TYPE] = self.type
        dyn_dict[CONTENTID] = self.media_id
        dyn_dict[USERID] = self.user_id
        dyn_dict[TAGS] = self.tags
        dyn_dict[SCORE] = self.score
        dyn_dict[VERSION] = self.version
        dyn_dict[POSITION] = self.position
        dyn_dict[FILEURL] = self.file_url
        dyn_dict[CREATEDAT] = convert_json_to_realworld(self.created_at)
        dyn_dict[UPDATEDAT] = convert_json_to_realworld(self.updated_at)

        return dyn_dict

    @classmethod
    def from_dynamo(cls, item: dict) -> 'TextBlock':

        tb = cls(
            parentdocument_id=item[NAMESPACE],
            type=item[TYPE],
            media_id=item[CONTENTID],
            user_id=item[USERID],
            tags=item[TAGS],
            score=item[SCORE],
            version=item[VERSION],
            position=item[POSITION],
            file_url=item[FILEURL],
            created_at=convert_string_to_datetime(item[CREATEDAT]),
            updated_at=convert_string_to_datetime(item[UPDATEDAT]),
        )

        return tb
