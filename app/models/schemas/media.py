
from datetime import datetime
from typing import List

from pydantic import (
    EmailStr,
    HttpUrl
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin,
    convert_json_to_realworld
)
from app.models.domain.content import ContentDynaUpdateInterface
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


class Media(DateTimeModelMixin, ModelConfigMixin, ContentDynaUpdateInterface):
    parentdocument_id: str = None
    type: int = None
    media_id: str = None
    user_id: EmailStr = None
    tags: List[str] = None
    score: int = None
    version: int = None
    position: int = None
    file_url: HttpUrl = None

    def to_dynamo_update(self) -> dict:
        dyn_update_dict = dict()

        if self.parentdocument_id: dyn_update_dict[NAMESPACE] = self.parentdocument_id
        if self.type: dyn_update_dict[TYPE] = self.type
        if self.media_id: dyn_update_dict[CONTENTID] = self.media_id
        if self.user_id: dyn_update_dict[USERID] = self.user_id
        if self.tags: dyn_update_dict[TAGS] = self.tags
        if self.score: dyn_update_dict[SCORE] = self.score
        if self.version: dyn_update_dict[VERSION] = self.version
        if self.position: dyn_update_dict[POSITION] = self.position
        if self.file_url: dyn_update_dict[FILEURL] = self.file_url

        dyn_update_dict[UPDATEDAT] = convert_json_to_realworld(datetime.now())

        return dyn_update_dict
