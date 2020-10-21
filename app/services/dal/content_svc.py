from typing import Union, List

from loguru import logger

from app.models.domain.content import (
    ENTITY_TYPE,
    MEDIA_ENTITY,
    TEXTBLOCK_ENTITY,
    NAMESPACE,
    CONTENTID,
    CONTENT_TBL,
    ContentDynaInOutInterface,
    ContentDynaUpdateInterface
)
from app.models.domain.docstream import DocStream
from app.models.domain.document import Document
from app.models.domain.media import Media
from app.models.domain.textblock import TextBlock
from app.models.schemas.content import ContentWriteResponse
from app.services.dal import dynamodb_svc


async def create_content(
        content: ContentDynaInOutInterface
) -> ContentWriteResponse:
    item = content.to_dynamo()

    ns = item[NAMESPACE]
    content_id = item[CONTENTID]

    existing_doc = await get_document(ns=ns, content_id=content_id)
    if existing_doc: return None

    logger.debug(f"Content Item: {item}")
    response = await dynamodb_svc.put_item(
        tbl_name=CONTENT_TBL,
        item=item,
    )

    logger.debug(f"Create Response: {response}")

    return ContentWriteResponse(
        message="Document was created.",
        namespace=ns,
        content_id=content_id
    )


async def batch_create_content(
        *contents: ContentDynaInOutInterface
) -> List[ContentWriteResponse]:
    items = [content.to_dynamo() for content in contents]
    for item in items:

        ns = item[NAMESPACE]
        content_id = item[CONTENTID]

        existing_doc = await get_document(ns=ns, content_id=content_id)
        if existing_doc: return None

    logger.debug(f"Content Items: {items}")
    responses = await dynamodb_svc.batch_put_item(
        tbl_name=CONTENT_TBL,
        item=items,
    )

    logger.debug(f"Create Response: {responses}")

    return [
        ContentWriteResponse(
            message="Document was created.",
            namespace=ns,
            content_id=content_id
        )
        for item in items if (
            ns := item[NAMESPACE],
            content_id := item[CONTENTID]
        )
    ]


async def get_document(
        ns: str,
        content_id: str
) -> Document:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    response = await dynamodb_svc.get_item(
        tbl_name=CONTENT_TBL,
        key=key,
    )

    logger.debug(f"Get Document Response: {response}")

    if response:
        doc = Document.from_dynamo(response)
        logger.debug(f"Retrieved Document: {doc}")
        return doc
    else:
        return None


async def get_document_stream(
        ns: str,
        content_id: str
) -> DocStream:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    response = await dynamodb_svc.get_item(
        tbl_name=CONTENT_TBL,
        key=key,
    )

    logger.debug(f"Get Document Stream Response: {response}")

    if response:
        ds = DocStream.from_dynamo(response)
        logger.debug(f"Retrieved Document Stream: {ds}")

        child_responses = await dynamodb_svc.query_by_partition(
            CONTENT_TBL,
            NAMESPACE,
            content_id
        )

        ds.item_stream = []
        for child_resp in child_responses:
            if child_resp[ENTITY_TYPE] == MEDIA_ENTITY:
                media = Media.from_dynamo(child_resp[ENTITY_TYPE])
                ds.item_stream.append(media)
            if child_resp[ENTITY_TYPE] == TEXTBLOCK_ENTITY:
                tb = TextBlock.from_dynamo(child_resp[ENTITY_TYPE])
                ds.item_stream.append(tb)

        return ds
    else:
        return None


async def get_documents_by_topic(
        ns: str
) -> List[Document]:
    response = await dynamodb_svc.query_by_partition(
        tbl_name=CONTENT_TBL,
        partition_name=NAMESPACE,
        partition_value=ns,
    )

    logger.debug(f"Query Document by Topic Response: {response}")

    if response:
        docs = [Document.from_dynamo(item) for item in response]
        logger.debug(f"Retrieved Documents: {docs}")
        return docs
    else:
        return []


async def delete_content(
        ns: str,
        content_id: str
) -> ContentWriteResponse:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    response = await dynamodb_svc.delete_item(
        tbl_name=CONTENT_TBL,
        key=key,
    )

    logger.debug(f"Delete Response: {response}")

    return ContentWriteResponse(
        message="Content was deleted.",
        namespace=ns,
        content_id=content_id
    )


async def update_content(
        ns: str,
        content_id: str,
        content: ContentDynaUpdateInterface
) -> ContentWriteResponse:

    if type(content) is Document:
        existing_doc = await get_document(ns=ns, content_id=content_id)
        if not existing_doc: return None
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    new_item = content.to_dynamo_update()
    logger.debug(f"Content Item new: {new_item}")
    response = await dynamodb_svc.update_item(
        tbl_name=CONTENT_TBL,
        key=key,
        new_item=new_item,
    )

    logger.debug(f"Update Response: {response}")

    return ContentWriteResponse(
        message="Content was updated.",
        namespace=ns,
        content_id=content_id
    )
