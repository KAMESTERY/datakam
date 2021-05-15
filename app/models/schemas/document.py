
from datetime import datetime
from typing import (
    List,
    Optional
)

from pydantic import EmailStr

from app.models.common import ModelConfigMixin, convert_json_to_realworld
from app.models.domain.content import (
    ContentRef,
    USERID,
    TAGS,
    SCORE,
    VERSION,
    UPDATEDAT,
    ContentDynaUpdateInterface, DOCUMENT_ENTITY
)
from app.models.domain.document import (
    NAMESPACE,
    CONTENTID,
    SLUG,
    TITLE,
    IDENTIFIER,
    BODY,
    PUBLISH,
    FILTREVISUEL,
    LANGUE,
    NIVEAU
)
from app.models.schemas.media import MediaUpdateIn


class DocumentUpdateIn(ModelConfigMixin, ContentDynaUpdateInterface):
    topic: str = None
    document_id: str = None
    user_id: Optional[EmailStr] = None
    tags: List[str] = None
    score: Optional[int] = None
    version: Optional[int] = None
    slug: Optional[str] = None
    title: Optional[str] = None
    identifier: Optional[str] = None
    body: Optional[str] = None
    publish: Optional[bool] = None
    filtre_visuel: Optional[int] = None
    langue: Optional[int] = None
    niveau: Optional[int] = None
    media: Optional[List[MediaUpdateIn]] = None

    def get_key(self) -> dict:
        key = dict()

        key[NAMESPACE] = self.topic
        key[CONTENTID] = self.document_id

    def get_entity_type(self) -> str:
        return DOCUMENT_ENTITY

    def to_dynamo_update(self) -> List[dict]:
        list_of_dyn_update_dicts = [itm.to_dynamo_update() for itm in self.media]

        dyn_update_dict = dict()

        if self.topic: dyn_update_dict[NAMESPACE] = self.topic
        if self.document_id: dyn_update_dict[CONTENTID] = self.document_id
        if self.user_id: dyn_update_dict[USERID] = self.user_id
        if self.tags: dyn_update_dict[TAGS] = self.tags
        if self.score: dyn_update_dict[SCORE] = self.score
        if self.version: dyn_update_dict[VERSION] = self.version
        if self.slug: dyn_update_dict[SLUG] = self.slug
        if self.title: dyn_update_dict[TITLE] = self.title
        if self.identifier: dyn_update_dict[IDENTIFIER] = self.identifier
        if self.body: dyn_update_dict[BODY] = self.body
        if self.publish: dyn_update_dict[PUBLISH] = self.publish
        if self.filtre_visuel: dyn_update_dict[FILTREVISUEL] = self.filtre_visuel
        if self.langue: dyn_update_dict[LANGUE] = self.langue
        if self.niveau: dyn_update_dict[NIVEAU] = self.niveau
        dyn_update_dict[UPDATEDAT] = convert_json_to_realworld(datetime.now())

        list_of_dyn_update_dicts.append(dyn_update_dict)

        return list_of_dyn_update_dicts
