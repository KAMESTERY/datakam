
import datetime

from typing import List

from pydantic import EmailStr

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin
)


AUTHOR = "Author"
TEXT = "Text"
TYPE = "Type"
POSITION = "Position"


class TextBlock(DateTimeModelMixin, ModelConfigMixin):
    parentdocument_id: str
    textblock_id: str
    user_id: EmailStr
    tags: List[str]
    score: int
    version: int
    position: int
    type: int
    text: str
    author: str
