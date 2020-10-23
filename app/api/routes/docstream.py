
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

router = APIRouter()

@router.post(
    "/docstream",
    response_model=List[ContentWriteResponse],
    responses={
        409: {"model": Message,
              "description": """
              The request could not be completed due to a conflict\n
              with the current state of the target resource
              """}
    },
    name="Create a Document Stream",
    tags=[PROVISIONERS]
)
async def create_document_stream(ds: DocStream) -> List[ContentWriteResponse]:
    resp = await content_svc.create_content(ds)
    return resp if len(resp) > 0 else JSONResponse(
        status_code=409,
        content={"message": "Conflict"}
    )


@router.get(
    "/docstream/{ns}/{content_id}",
    response_model=DocStream,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    name="Retrieve Document",
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


@router.patch(
    "/docstream/{ns}/{content_id}",
    response_model=List[ContentWriteResponse],
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    name="Patch Document",
    tags=[ADMIN]
)
async def patch_document_stream(
        ns: str,
        content_id: str,
        doc: DocStreamUpdateIn
) -> List[ContentWriteResponse]:
    resp = await content_svc.update_content(
        ns=ns,
        content_id=content_id,
        content=doc
    )
    return resp if len(resp) > 0 else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@router.put(
    "/docstream/{ns}/{content_id}",
    response_model=List[ContentWriteResponse],
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    name="Update Document Stream",
    tags=[ADMIN]
)
async def update_document_stream(
        ns: str,
        content_id: str,
        doc: DocStreamUpdateIn
) -> List[ContentWriteResponse]:
    resp = await content_svc.update_content(
        ns=ns,
        content_id=content_id,
        content=doc
    )
    return resp if resp else JSONResponse(
        status_code=404,
        content={"message": "Item not found"}
    )


@router.delete(
    "/docstream/{ns}/{content_id}",
    response_model=ContentWriteResponse,
    responses={
        404: {"model": Message, "description": "The item was not found"}
    },
    name="Delete Document Stream",
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
