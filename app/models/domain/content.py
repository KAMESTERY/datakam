import abc

from pydantic import (
    EmailStr
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin
)
from app.models.domain import EXAMPLE_NAMESPACE, EXAMPLE_CONTENT_ID, EXAMPLE_EMAIL


CONTENT_TBL = "Content"
NAMESPACE = "Namespace"
CONTENTID = "ContentID"
USERID = "UserID"
TAGS = "Tags"
SCORE = "Score"
VERSION = "Version"
CREATEDAT = "CreatedAt"
UPDATEDAT = "UpdatedAt"
TYPE = "Type"
POSITION = "Position"


class Content(DateTimeModelMixin, ModelConfigMixin):
    namespace: str = EXAMPLE_NAMESPACE
    content_id: str = EXAMPLE_CONTENT_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    score: int
    version: int

class ContentRef(ModelConfigMixin):
    namespace: str
    content_id: str


class ContentDynaInOutInterface(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def to_dynamo(self) -> dict:
        raise NotImplementedError

    @classmethod
    @abc.abstractmethod
    def from_dynamo(cls, item: dict):
        raise NotImplementedError


class ContentDynaUpdateInterface(metaclass=abc.ABCMeta):

    @abc.abstractmethod
    def to_dynamo_update(self) -> dict:
        raise NotImplementedError

