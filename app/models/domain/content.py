
import abc

from typing import (
    Union,
    List
)

from pydantic import (
    EmailStr
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin
)
from app.models.domain import (
    EXAMPLE_NAMESPACE,
    EXAMPLE_CONTENT_ID,
    EXAMPLE_EMAIL,
    EXAMPLE_NUMBER
)

CONTENT_TBL = "Content"
NAMESPACE = "Namespace"
CONTENTID = "ContentID"
USERID = "UserID"
TAGS = "Tags"
SCORE = "Score"
VERSION = "Version"
CREATEDAT = "CreatedAt"
UPDATEDAT = "UpdatedAt"
TYPE = "Kind"
POSITION = "Idx"
ENTITY_TYPE = "EntityType"
DOCUMENT_ENTITY = "Document"
DOCSTREAM_ENTITY = "DocStream"
MEDIA_ENTITY = "Media"
TEXTBLOCK_ENTITY = "TextBlock"


class Content(DateTimeModelMixin, ModelConfigMixin):
    namespace: str = EXAMPLE_NAMESPACE
    content_id: str = EXAMPLE_CONTENT_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    score: int = EXAMPLE_NUMBER
    version: int = EXAMPLE_NUMBER


class ContentRef(ModelConfigMixin):
    namespace: str
    content_id: str


class ContentDynaInOutInterface(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def get_entity_type(self) -> str:
        pass

    @abc.abstractmethod
    def get_key(self) -> dict:
        pass

    @abc.abstractmethod
    def to_dynamo(self) -> Union[dict, List[dict]]:
        raise NotImplementedError

    @classmethod
    @abc.abstractmethod
    def from_dynamo(cls, item: dict):
        raise NotImplementedError


class ContentDynaUpdateInterface(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def get_entity_type(self) -> str:
        pass

    @abc.abstractmethod
    def get_key(self) -> dict:
        pass

    @abc.abstractmethod
    def to_dynamo_update(self):
        raise NotImplementedError

