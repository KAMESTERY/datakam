
from typing import List, Union

from fastapi import APIRouter, Path, Body
from starlette.responses import JSONResponse

from app.api.routes import (
    ADMIN,
    PROVISIONERS,
    READERS
)
from app.models.domain.document import Document
from app.models.schemas.content import (
    ContentWriteResponse,
    Message
)
from app.models.schemas.document import DocumentUpdateIn
from app.services.dal import content_svc

document_api = APIRouter()


@document_api.post(
    "/document",
    status_code=201,
    response_model=ContentWriteResponse,
    responses={
        409: {"model": Message,
              "description": """
              The request could not be completed due to a conflict\n
              with the current state of the target resource
              """},
        422: {"model": Message,
              "description": """
              Unprocessable Entity
              """}
    },
    description="Create a Document",
    operation_id="Document_POST",
    tags=[PROVISIONERS]
)
async def create_document(doc: Document) -> ContentWriteResponse:
    resp = await content_svc.create_content(doc)
    return resp if resp else JSONResponse(
        status_code=409,
        content={"message": "Conflict"}
    )


@document_api.get(
    "/document/{topic}",
    response_model=List[Document],
    description="Get Documents by Topic",
    operation_id="Documents_GET_by_Topic",
    tags=[READERS]
)
async def get_documents_by_topic(
        topic: str = Path(..., title="Topic")
) -> List[Document]:
    resp = await content_svc.get_documents_by_topic(
        ns=topic
    )
    return resp


@document_api.get(
    "/document/{topic}/{document_id}/",
    response_model=Document,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Retrieve Document",
    operation_id="Document_GET",
    tags=[READERS]
)
async def retrieve_document(
        topic: str = Path(..., title="Topic"),
        document_id: str = Path(..., title="Document ID")
) -> Document:
    resp = await content_svc.get_document(
        ns=topic,
        content_id=document_id
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@document_api.patch(
    "/document/{topic}/{document_id}/",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"},
        422: {"model": Message,
              "description": """
              Unprocessable Entity
              """}
    },
    description="Patch Document",
    operation_id="Document_PATCH",
    tags=[ADMIN]
)
async def patch_document(
        doc: DocumentUpdateIn,
        topic: str = Path(..., title="Topic"),
        document_id: str = Path(..., title="Document ID")
) -> ContentWriteResponse:
    resp = await content_svc.update_content(
        ns=topic,
        content_id=document_id,
        content=doc
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@document_api.put(
    "/document/{topic}/{document_id}/",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"},
        422: {"model": Message,
              "description": """
              Unprocessable Entity
              """}
    },
    description="Update Document",
    operation_id="Document_UPDATE",
    tags=[ADMIN]
)
async def update_document(
        doc: DocumentUpdateIn,
        topic: str = Path(..., title="Topic"),
        document_id: str = Path(..., title="Document ID")
) -> ContentWriteResponse:
    resp = await content_svc.update_content(
        ns=topic,
        content_id=document_id,
        content=doc
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@document_api.delete(
    "/document/{topic}/{document_id}/",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Delete Document",
    operation_id="Document_DELETE",
    tags=[PROVISIONERS]
)
async def delete_document(
        topic: str = Path(..., title="Topic"),
        document_id: str = Path(..., title="Document ID")
) -> ContentWriteResponse:
    resp = await content_svc.delete_content(
        ns=topic,
        content_id=document_id
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )
