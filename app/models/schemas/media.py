
from datetime import datetime
from typing import (
    List,
    Optional
)

from pydantic import (
    EmailStr,
    HttpUrl
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin,
    convert_json_to_realworld
)
from app.models.domain.content import ContentDynaUpdateInterface, MEDIA_ENTITY
from app.models.domain.content import (
    NAMESPACE,
    CONTENTID,
    USERID,
    TAGS,
    SCORE,
    VERSION,
    UPDATEDAT,
    POSITION,
    TYPE
)

FILEURL = "FileUrl"


class MediaUpdateIn(DateTimeModelMixin, ModelConfigMixin, ContentDynaUpdateInterface):
    parent_content_id: str = None
    type: Optional[int] = None
    media_id: str = None
    user_id: Optional[EmailStr] = None
    tags: Optional[List[str]] = None
    score: Optional[int] = None
    version: Optional[int] = None
    position: Optional[int] = None
    file_url: Optional[HttpUrl] = None

    def get_key(self) -> dict:
        key = dict()

        key[NAMESPACE] = self.namespace
        key[CONTENTID] = self.content_id

    def get_entity_type(self) -> str:
        return MEDIA_ENTITY

    def to_dynamo_update(self) -> dict:
        dyn_update_dict = dict()

        if self.parent_content_id: dyn_update_dict[NAMESPACE] = self.parent_content_id
        if self.media_id: dyn_update_dict[CONTENTID] = self.media_id
        if self.type: dyn_update_dict[TYPE] = self.type
        if self.user_id: dyn_update_dict[USERID] = self.user_id
        if self.tags: dyn_update_dict[TAGS] = self.tags
        if self.score: dyn_update_dict[SCORE] = self.score
        if self.version: dyn_update_dict[VERSION] = self.version
        if self.position: dyn_update_dict[POSITION] = self.position
        if self.file_url: dyn_update_dict[FILEURL] = self.file_url

        dyn_update_dict[UPDATEDAT] = convert_json_to_realworld(datetime.now())

        return dyn_update_dict
