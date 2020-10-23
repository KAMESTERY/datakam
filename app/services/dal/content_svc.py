
import asyncio
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
    ContentDynaUpdateInterface, DOCUMENT_ENTITY, DOCSTREAM_ENTITY
)
from app.models.domain.docstream import DocStream
from app.models.domain.document import Document
from app.models.domain.media import Media
from app.models.domain.textblock import TextBlock
from app.models.schemas.content import ContentWriteResponse
from app.services.dal import dynamodb_svc


async def create_content(
        content: ContentDynaInOutInterface
) -> Union[ContentWriteResponse, List[ContentWriteResponse]]:
    key = content.get_key()
    logger.debug(f"Content Key: {key}")

    existing_doc = await dynamodb_svc.get_item(tbl_name=CONTENT_TBL, key=key)
    if existing_doc: return None

    new_content = content.to_dynamo()
    entity_type = content.get_entity_type()

    if entity_type == DOCUMENT_ENTITY:
        response = await dynamodb_svc.put_item(
            tbl_name=CONTENT_TBL,
            item=new_content,
        )
        logger.debug(f"Create Response: {response}")
        return ContentWriteResponse(
            message="Content was created.",
            namespace=key[NAMESPACE],
            content_id=key[CONTENTID]
        )
    elif entity_type == DOCSTREAM_ENTITY:
        responses = await dynamodb_svc.batch_put_item(
            tbl_name=CONTENT_TBL,
            items=new_content,
        )
        logger.debug(f"Create Responses: {responses}")
        return [
            ContentWriteResponse(
                message="Content was created.",
                namespace=ns,
                content_id=content_id
            )
            for item in new_content if (
                ns := item[NAMESPACE],
                content_id := item[CONTENTID]
            )
        ]
    else:
        return None


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
                media = Media.from_dynamo(child_resp)
                ds.item_stream.append(media)
            if child_resp[ENTITY_TYPE] == TEXTBLOCK_ENTITY:
                tb = TextBlock.from_dynamo(child_resp)
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

    logger.debug(f"Content Key: {key}")

    existing_doc = await dynamodb_svc.get_item(CONTENT_TBL, key)
    if existing_doc is None: return None

    if existing_doc[ENTITY_TYPE] == DOCUMENT_ENTITY:
        response = await dynamodb_svc.delete_item(
            tbl_name=CONTENT_TBL,
            key=key,
        )
        logger.debug(f"Delete Response: {response}")

    elif existing_doc[ENTITY_TYPE] == DOCSTREAM_ENTITY:
        ds = await get_document_stream(ns=ns, content_id=content_id)

        keys = [itms.get_key() for itms in ds.item_stream]
        keys.append(key)
        responses = await dynamodb_svc.batch_delete_item(
            tbl_name=CONTENT_TBL,
            keys=keys,
        )
        logger.debug(f"Delete Responses: {responses}")
    else:
        return None

    return ContentWriteResponse(
        message="Content was deleted.",
        namespace=ns,
        content_id=content_id
    )


async def update_content(
        ns: str,
        content_id: str,
        content: ContentDynaUpdateInterface
) -> Union[ContentWriteResponse,List[ContentWriteResponse]]:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id

    existing_doc = await dynamodb_svc.get_item(tbl_name=CONTENT_TBL, key=key)
    if existing_doc: return None

    new_content = content.to_dynamo_update()
    entity_type = content.get_entity_type()

    if entity_type == DOCUMENT_ENTITY:
        response = await dynamodb_svc.update_item(
            tbl_name=CONTENT_TBL,
            key=key,
            new_item=new_content,
        )
        logger.debug(f"Update Response: {response}")
        return ContentWriteResponse(
            message="Content was updated.",
            namespace=ns,
            content_id=content_id
        )
    elif entity_type == DOCSTREAM_ENTITY:
        tasks = []
        for item in new_content:
            task = dynamodb_svc.update_item(
                tbl_name=CONTENT_TBL,
                key=item.get_key(),
                new_item=new_content,
            )
            tasks.append(task)
        responses = await asyncio.wait(tasks)
        logger.debug(f"Create Responses: {responses}")
        return [
            ContentWriteResponse(
                message="Content was updated.",
                namespace=item[NAMESPACE],
                content_id=item[CONTENTID]
            )
            for item in new_content
        ]
    else:
        return None


