
from typing import List

from fastapi import APIRouter
from starlette.responses import JSONResponse

from app.api.routes import (
    ADMIN,
    PROVISIONERS,
    READERS
)
from app.models.domain.docstream import DocStream
from app.models.schemas.content import (
    ContentWriteResponse,
    Message
)
from app.models.schemas.docstream import DocStreamUpdateIn
from app.services.dal import content_svc

docstream_api = APIRouter()


@docstream_api.post(
    "/docstream",
    status_code=201,
    response_model=List[ContentWriteResponse],
    responses={
        409: {"model": Message,
              "description": """
              The request could not be completed due to a conflict\n
              with the current state of the target resource
              """}
    },
    description="Create a Document Stream",
    operation_id="DocumentStream_GET",
    tags=[PROVISIONERS]
)
async def create_document_stream(ds: DocStream) -> ContentWriteResponse:
    resp = await content_svc.create_content(ds)
    return resp if resp else JSONResponse(
        status_code=409,
        content={"message": "Conflict"}
    )


@docstream_api.get(
    "/docstream/{ns}/{content_id}/",
    response_model=DocStream,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Retrieve a Document Stream",
    operation_id="DocumentStream_GET",
    tags=[READERS]
)
async def retrieve_document_stream(ns: str, content_id: str) -> DocStream:
    resp = await content_svc.get_document_stream(
        ns=ns,
        content_id=content_id
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@docstream_api.patch(
    "/docstream/{ns}/{content_id}/",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Patch a Document Stream",
    operation_id="DocumentStream_PATCH",
    tags=[ADMIN]
)
async def patch_document_stream(
        ns: str,
        content_id: str,
        doc: DocStreamUpdateIn
) -> ContentWriteResponse:
    resp = await content_svc.update_content(
        ns=ns,
        content_id=content_id,
        content=doc
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@docstream_api.put(
    "/docstream/{ns}/{content_id}",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Update a Document Stream",
    operation_id="DocumentStream_PATCH",
    tags=[ADMIN]
)
async def update_document_stream(
        ns: str,
        content_id: str,
        doc: DocStreamUpdateIn
) -> ContentWriteResponse:
    resp = await content_svc.update_content(
        ns=ns,
        content_id=content_id,
        content=doc
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@docstream_api.delete(
    "/docstream/{ns}/{content_id}",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    description="Delete Document Stream",
    operation_id="DocumentStream_DELETE",
    tags=[PROVISIONERS]
)
async def delete_document_stream(ns: str, content_id: str) -> ContentWriteResponse:
    resp = await content_svc.delete_content(
        ns=ns,
        content_id=content_id
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )
