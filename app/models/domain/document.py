
from typing import (
    List,
    Optional
)

from pydantic import (
    EmailStr
)

from app.models.common import (
    DateTimeModelMixin,
    ModelConfigMixin,
    convert_json_to_realworld,
    convert_string_to_datetime
)
from app.models.domain import (
    EXAMPLE_NAMESPACE,
    EXAMPLE_CONTENT_ID,
    EXAMPLE_EMAIL,
    EXAMPLE_TAGS,
    EXAMPLE_NUMBER,
    EXAMPLE_CONTENT_SLUG,
    EXAMPLE_CONTENT_TITLE,
    EXAMPLE_CONTENT_TEXT,
    EXAMPLE_BOOL
)
from app.models.domain.content import (
    NAMESPACE,
    CONTENTID,
    USERID,
    TAGS,
    SCORE,
    VERSION,
    CREATEDAT,
    UPDATEDAT,
    ContentDynaInOutInterface
)
from app.models.domain.media import Media

SLUG = "Slug"
TITLE = "Title"
IDENTIFIER = "Identifier"
BODY = "Body"
PUBLISH = "Publish"
FILTREVISUEL = "FiltreVisuel"
LANGUE = "Langue"
NIVEAU = "Niveau"


class Document(DateTimeModelMixin, ModelConfigMixin, ContentDynaInOutInterface):
    topic: str = EXAMPLE_NAMESPACE
    document_id: str = EXAMPLE_CONTENT_ID
    user_id: EmailStr = EXAMPLE_EMAIL
    tags: List[str] = EXAMPLE_TAGS
    score: int = EXAMPLE_NUMBER
    version: int = EXAMPLE_NUMBER
    slug: str = EXAMPLE_CONTENT_SLUG
    title: str = EXAMPLE_CONTENT_TITLE
    identifier: str = EXAMPLE_CONTENT_ID
    body: str = EXAMPLE_CONTENT_TEXT
    publish: bool = EXAMPLE_BOOL
    filtre_visuel: int = EXAMPLE_NUMBER
    langue: int = EXAMPLE_NUMBER
    niveau: int = EXAMPLE_NUMBER
    media: Optional[List[Media]]

    def to_dynamo(self) -> dict:
        dyn_dict = dict()

        dyn_dict[NAMESPACE] = self.topic
        dyn_dict[CONTENTID] = self.document_id
        dyn_dict[USERID] = self.user_id
        dyn_dict[TAGS] = self.tags
        dyn_dict[SCORE] = self.score
        dyn_dict[VERSION] = self.version
        dyn_dict[SLUG] = self.slug
        dyn_dict[TITLE] = self.title
        dyn_dict[IDENTIFIER] = self.identifier
        dyn_dict[BODY] = self.body
        dyn_dict[PUBLISH] = self.publish
        dyn_dict[FILTREVISUEL] = self.filtre_visuel
        dyn_dict[LANGUE] = self.langue
        dyn_dict[NIVEAU] = self.niveau
        dyn_dict[CREATEDAT] = convert_json_to_realworld(self.created_at)
        dyn_dict[UPDATEDAT] = convert_json_to_realworld(self.updated_at)

        return dyn_dict

    @classmethod
    def from_dynamo(cls, item: dict) -> 'Document':

        doc = cls(
            topic=item[NAMESPACE],
            document_id=item[CONTENTID],
            user_id=item[USERID],
            tags=item[TAGS],
            score=item[SCORE],
            version=item[VERSION],
            slug=item[SLUG],
            title=item[TITLE],
            identifier=item[IDENTIFIER],
            body=item[BODY],
            publish=item[PUBLISH],
            filtre_visuel=item[FILTREVISUEL],
            langue=item[LANGUE],
            niveau=item[NIVEAU],
            created_at=convert_string_to_datetime(item[CREATEDAT]),
            updated_at=convert_string_to_datetime(item[UPDATEDAT]),
        )

        return doc
