from app.models.domain.content import ContentRef


class DocumentWriteResponse(ContentRef):
    message: str
