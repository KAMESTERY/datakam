from typing import List

import graphene
from graphene import resolve_only_args
from graphene_pydantic import PydanticObjectType

from app.models.domain.document import Document
from app.models.domain.media import Media
from app.services.dal import content_svc


# Check this out: https://github.com/graphql-python/graphene-pydantic/blob/master/examples/departments.py
class MediaOut(PydanticObjectType):
    class Meta:
        model = Media

    @classmethod
    def is_type_of(cls, root, info):
        return isinstance(root, (cls, Media))


class DocumentOut(PydanticObjectType):
    class Meta:
        model = Document


class DocumentQuery:
    documents_by_topic = graphene.Field(
        graphene.List(DocumentOut),
        topic=graphene.String(
            required=True,
            description='Topic'
        )
    )

    @resolve_only_args
    async def resolve_documents_by_topic(self, topic: str) -> List[Document]:
        docs = await content_svc.get_documents_by_topic(topic)
        return docs
