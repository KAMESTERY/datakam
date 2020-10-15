
from fastapi import APIRouter

router = APIRouter()

@router.post("/")
async def create():
    pass

@router.get("/{contentId}")
async def retrieve(contentId: str):
    pass

@router.update("/{contentId}")
async def update(contentId: str):
    pass


@router.delete("/{contentId}")
async def delete(contentId: str):
    pass
