
from fastapi import FastAPI, HTTPException
from fastapi.exceptions import RequestValidationError

from app.api.errors.http_error import http_error_handler
from app.api.errors.validation_error import http422_error_handler
from app.api.routes.api import router as api_router
from app.core.events import (
    create_start_app_handler,
    create_stop_app_handler
)


def get_application() -> FastAPI:
    application = FastAPI(title='DataKam', debug=True, version='1.0.0')

    application.add_event_handler("startup", create_start_app_handler(application))
    application.add_event_handler("shutdown", create_stop_app_handler(application))

    application.add_exception_handler(HTTPException, http_error_handler)
    application.add_exception_handler(RequestValidationError, http422_error_handler)

    application.include_router(api_router, prefix="")

    return application


app = get_application()

if __name__ == "__main__":
    import uvicorn

    uvicorn.run("app.main:app", host='0.0.0.0', port=1717, reload=True, debug=True, workers=1)