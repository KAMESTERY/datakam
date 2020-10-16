
from typing import List

from pydantic import (
    EmailStr,
    HttpUrl
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin
)
from app.models.domain import (
    EXAMPLE_CONTENT_ID,
    EXAMPLE_EMAIL,
    EXAMPLE_TAGS,
    EXAMPLE_CHILD_ID,
    EXAMPLE_NUMBER,
    EXAMPLE_MEDIA_URL,
)

FILEURL = "FileUrl"
TYPE = "Type"
POSITION = "Position"


class Media(DateTimeModelMixin, ModelConfigMixin):
    parentdocument_id: str = EXAMPLE_CONTENT_ID
    type: int = EXAMPLE_NUMBER
    media_id: str = EXAMPLE_CHILD_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    tags: List[str] = EXAMPLE_TAGS
    score: int = EXAMPLE_NUMBER
    version: int = EXAMPLE_NUMBER
    position: int = EXAMPLE_NUMBER
    file_url: HttpUrl = EXAMPLE_MEDIA_URL
