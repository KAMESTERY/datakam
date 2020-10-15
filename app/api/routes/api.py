from fastapi import APIRouter

router = APIRouter()


@router.get("/hello/{name}")
def greet(name: str) -> str:
    return f"Hello {name} :-)"
