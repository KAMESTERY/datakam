
from fastapi import APIRouter

from app.api.routes import (
    content,
    graphql
)

router = APIRouter()

router.include_router(
    content.router,
    prefix="/content"
)


@router.get("/hello/{name}")
def greet(name: str) -> str:
    return f"Hello {name} :-)"
