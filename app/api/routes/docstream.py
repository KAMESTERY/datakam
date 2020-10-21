
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
    DocumentUpdateIn,
    Message
)
from app.services.dal import content_svc

router = APIRouter()

@router.post(
    "/docstream",
    response_model=ContentWriteResponse,
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
async def create_document_stream(ds: DocStream) -> ContentWriteResponse:
    child_resps = await content_svc.batch_create_content(ds, *ds.item_stream)
    resp = ContentWriteResponse(
            message="Document Stream was created.",
            namespace=ds.namespace,
            content_id=ds.content_id
        )
    return resp if len(child_resps) == 0 else JSONResponse(
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
