from loguru import logger

from app.models.domain.document import Document
from app.models.schemas.document import DocumentWriteResponse
from app.services.dal import dynamodb_svc


CONTENT_TBL = "Content"


async def create_document(doc: Document, dynamodb = None) -> DocumentWriteResponse:
     item = doc.to_dynamo()
     logger.debug(f"Document Item: {item}")
     response = await dynamodb_svc.put_item(
          tbl_name=CONTENT_TBL,
          item=item,
          dynamodb=dynamodb,
     )

     logger.debug(f"Create Response: {response}")

     return DocumentWriteResponse(
          message="Document was created.",
          namespace=doc.topic,
          content_id=doc.document_id
     )
