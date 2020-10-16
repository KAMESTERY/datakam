
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


class Content(DateTimeModelMixin, ModelConfigMixin):
    namespace: str = EXAMPLE_NAMESPACE
    content_id: str = EXAMPLE_CONTENT_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    score: int
    version: int

class ContentRef(ModelConfigMixin):
    namespace: str
    content_id: str
