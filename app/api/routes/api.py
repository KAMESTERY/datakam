
from fastapi import APIRouter

from app.api.routes import (
    document,
    docstream
)

router = APIRouter()

router.include_router(
    document.document_api,
    prefix="/content"
)

router.include_router(
    docstream.docstream_api,
    prefix="/content"
)


@router.get("/hello/{name}")
def greet(name: str) -> str:
    return f"Hello {name} :-)"
