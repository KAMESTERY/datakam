
from datetime import datetime
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
    ContentDynaUpdateInterface
)
from app.models.domain.content import (
    DOCSTREAM_ENTITY,
    NAMESPACE,
    CONTENTID,
    USERID,
    SCORE,
    VERSION,
    CREATEDAT,
    UPDATEDAT
)
from app.models.schemas.media import MediaUpdateIn
from app.models.schemas.textblock import TextBlockUpdateIn


class DocStreamUpdateIn(Content, ContentDynaUpdateInterface):
    item_stream: List[Union[MediaUpdateIn,TextBlockUpdateIn]] = []

    def get_entity_type(self):
        return DOCSTREAM_ENTITY

    def to_dynamo_update(self) -> List[dict]:
        list_of_dyn_update_dicts = [itms.to_dynamo_update() for itms in self.item_stream]

        dyn_update_dict = dict()

        if self.namespace: dyn_update_dict[NAMESPACE] = self.namespace
        if self.content_id: dyn_update_dict[CONTENTID] = self.content_id
        if self.user_id: dyn_update_dict[USERID] = self.user_id
        if self.score: dyn_update_dict[SCORE] = self.score
        if self.version: dyn_update_dict[VERSION] = self.version
        dyn_update_dict[UPDATEDAT] = convert_json_to_realworld(datetime.now())

        list_of_dyn_update_dicts.append(dyn_update_dict)

        return list_of_dyn_update_dicts
