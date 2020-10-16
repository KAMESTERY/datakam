from loguru import logger

from app.models.domain.content import (
    NAMESPACE,
    CONTENTID,
    CONTENT_TBL
)
from app.models.domain.document import Document
from app.models.schemas.document import DocumentWriteResponse
from app.services.dal import dynamodb_svc


async def create_document(doc: Document, dynamodb=None) -> DocumentWriteResponse:
    item = doc.to_dynamo()
    logger.debug(f"Document Item: {item}")
    response = await dynamodb_svc.put_item(
        tbl_name=CONTENT_TBL,
        item=item,
        dynamodb=dynamodb,
    )

    logger.debug(f"Create Response: {response}")

    return DocumentWriteResponse(
        message="Document was created.",
        namespace=doc.topic,
        content_id=doc.document_id
    )


async def delete_document(
        ns: str,
        content_id: str,
        dynamodb=None
) -> DocumentWriteResponse:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    response = await dynamodb_svc.delete_item(
        tbl_name=CONTENT_TBL,
        key=key,
        dynamodb=dynamodb,
    )

    logger.debug(f"Delete Response: {response}")

    return DocumentWriteResponse(
        message="Document was deleted.",
        namespace=ns,
        content_id=content_id
    )


async def update_document(
        ns: str,
        content_id: str,
        doc: Document,
        dynamodb=None
) -> DocumentWriteResponse:
    key = dict()
    key[NAMESPACE] = ns
    key[CONTENTID] = content_id
    new_item = doc.to_dynamo_update()
    logger.debug(f"Document Item new: {new_item}")
    response = await dynamodb_svc.update_item(
        tbl_name=CONTENT_TBL,
        key=key,
        new_item=new_item,
        dynamodb=dynamodb,
    )

    logger.debug(f"Update Response: {response}")

    return DocumentWriteResponse(
        message="Document was updated.",
        namespace=ns,
        content_id=content_id
    )
