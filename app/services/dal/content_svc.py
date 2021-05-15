
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


async def create_content(content: ContentDynaInOutInterface) -> ContentWriteResponse:
    key = content.get_key()
    logger.debug(f"Content Key: {key}")

    existing_doc = await dynamodb_svc.get_item(tbl_name=CONTENT_TBL, key=key)
    if existing_doc: return None

    new_content = content.to_dynamo()

    if isinstance(new_content, list):
        responses = await dynamodb_svc.batch_put_item(
            tbl_name=CONTENT_TBL,
            items=new_content,
        )
        logger.debug(f"Create Responses: {responses}")
    else:
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

        media_responses = await dynamodb_svc.query_by_partition(
            CONTENT_TBL,
            NAMESPACE,
            content_id
        )

        doc.media = []
        for media_resp in media_responses:
            if media_resp[ENTITY_TYPE] == MEDIA_ENTITY:
                media = Media.from_dynamo(media_resp)
                doc.media.append(media)

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
        for doc in docs:
            media_responses = await dynamodb_svc.query_by_partition(
                tbl_name=CONTENT_TBL,
                partition_name=NAMESPACE,
                partition_value=doc.document_id,
            )
            doc.media = []
            for media_resp in media_responses:
                if media_resp[ENTITY_TYPE] == MEDIA_ENTITY:
                    media = Media.from_dynamo(media_resp)
                    doc.media.append(media)

        logger.debug(f"Retrieved Documents: {docs}")
        return docs
    else:
        return []


async def get_document_streams_by_topic(
        ns: str
) -> List[DocStream]:
    response = await dynamodb_svc.query_by_partition(
        tbl_name=CONTENT_TBL,
        partition_name=NAMESPACE,
        partition_value=ns,
    )

    logger.debug(f"Query Document Stream by Topic Response: {response}")

    if response:
        dss = [DocStream.from_dynamo(item) for item in response]
        for ds in dss:
            item_stream_responses = await dynamodb_svc.query_by_partition(
                tbl_name=CONTENT_TBL,
                partition_name=NAMESPACE,
                partition_value=ds.content_id,
            )
            ds.item_stream = []
            for item_stream_resp in item_stream_responses:
                if item_stream_resp[ENTITY_TYPE] == MEDIA_ENTITY:
                    item_stream = Media.from_dynamo(item_stream_resp)
                    ds.item_stream.append(item_stream)
                if item_stream_resp[ENTITY_TYPE] == TEXTBLOCK_ENTITY:
                    item_stream = TextBlock.from_dynamo(item_stream_resp)
                    ds.item_stream.append(item_stream)

        logger.debug(f"Retrieved Document Streams: {dss}")
        return dss
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
    if not existing_doc: return None

    if existing_doc[ENTITY_TYPE] == DOCUMENT_ENTITY:
        doc = await get_document(ns=ns, content_id=content_id)
        keys = [itms.get_key() for itms in doc.media]
        keys.append(key)
        responses = await dynamodb_svc.batch_delete_item(
            tbl_name=CONTENT_TBL,
            keys=keys,
        )
        logger.debug(f"Delete Responses: {responses}")

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
) -> ContentWriteResponse:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id

    existing_doc = await dynamodb_svc.get_item(tbl_name=CONTENT_TBL, key=key)
    if not existing_doc: return None

    new_content = content.to_dynamo_update()
    entity_type = content.get_entity_type()

    if isinstance(new_content, list):
        tasks = []
        for item in new_content:
            item_key = {k: item.get(k, None) for k in [NAMESPACE, CONTENTID]}
            # item.pop(NAMESPACE, None)
            # item.pop(CONTENTID, None)
            task = dynamodb_svc.update_item(
                tbl_name=CONTENT_TBL,
                key=item_key,
                new_item=item,
            )
            tasks.append(task)
        responses = await asyncio.wait(tasks)
        logger.debug(f"Create Responses: {responses}")
    else:
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


