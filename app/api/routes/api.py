
from fastapi import APIRouter

from app.api.routes import (
    document,
    docstream
)

router = APIRouter()

router.include_router(
    document.router,
    prefix="/content"
)

router.include_router(
    docstream.router,
    prefix="/content"
)


@router.get("/hello/{name}")
def greet(name: str) -> str:
    return f"Hello {name} :-)"
