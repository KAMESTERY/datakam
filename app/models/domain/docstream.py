from typing import (
    List,
    Union
)

from app.models.domain.textblock import TextBlock
from app.models.domain.content import Content
from app.models.domain.media import Media


class DocStream(Content):
    item_stream: List[Union[Media,TextBlock]]
