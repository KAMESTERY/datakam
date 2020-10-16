from fastapi import APIRouter

from app.api.routes import (
    ADMIN,
    PROVISIONERS,
    READERS
)
from app.models.domain.document import Document
from app.models.schemas.document import DocumentWriteResponse
from app.services.dal import content_svc

router = APIRouter()


@router.post(
    "/document",
    response_model=DocumentWriteResponse,
    name="Create a Document",
    tags=[PROVISIONERS]
)
async def create_document(doc: Document) -> DocumentWriteResponse:
    resp = await content_svc.create_document(doc)
    return resp


@router.get(
    "/document/{documentId}",
    response_model=Document,
    name="Retrieve Document",
    tags=[READERS]
)
async def retrieve_document(document_id: str):
    pass


@router.put(
    "/document/{documentId}",
    response_model=DocumentWriteResponse,
    name="Update Document",
    tags=[ADMIN]
)
async def update_document(doc: Document) -> DocumentWriteResponse:
    resp = await content_svc.update_document(doc)
    return resp


@router.delete(
    "/document/{documentId}",
    name="Delete Document",
    tags=[PROVISIONERS]
)
async def delete_document(document_id: str):
    pass
