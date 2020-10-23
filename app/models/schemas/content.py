
from pydantic import BaseModel

from app.models.domain.content import (
    ContentRef
)


class Message(BaseModel):
    message: str


class ContentWriteResponse(ContentRef):
    message: str
